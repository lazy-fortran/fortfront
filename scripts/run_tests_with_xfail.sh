#!/usr/bin/env bash
set -euo pipefail

# XFAIL-aware test runner for fpm projects with optional parallel execution.
# - Discovers tests via `fpm test --list`
# - Runs each test binary directly from the build tree
# - Honors XFAIL map in test/xfail.csv
# - Supports parallel runs via TEST_JOBS (default: min(8, nproc))
# - Enforces per-test timeout via TIME_LIMIT (default: 120s)

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"

# Configuration
TIME_LIMIT=${TIME_LIMIT:-120}
if command -v nproc >/dev/null 2>&1; then
  default_jobs=$(nproc)
else
  default_jobs=2
fi
# Cap default to avoid oversubscription in CI
if [[ $default_jobs -gt 8 ]]; then default_jobs=8; fi
TEST_JOBS=${TEST_JOBS:-$default_jobs}
if [[ $TEST_JOBS -lt 1 ]]; then TEST_JOBS=1; fi

xfail_file="test/xfail.csv"
declare -A XFAIL
if [[ -f "$xfail_file" ]]; then
  # Format: test_name,issue_url
  while IFS=, read -r name url; do
    name=$(echo "$name" | sed 's/^ *//;s/ *$//')
    url=$(echo "$url" | sed 's/^ *//;s/ *$//')
    [[ -n "$name" ]] || continue
    [[ "$name" =~ ^# ]] && continue
    XFAIL["$name"]="$url"
  done < "$xfail_file"
fi

echo "=== Building tests (fpm test --list) ==="
tmp_list=$(mktemp)
fpm test --list > "$tmp_list" 2>&1
mapfile -t tests < <(awk 'BEGIN{p=0} /^ Matched names:/{p=1;next} p{print}' "$tmp_list" | sed 's/[[:space:]]\+$//' | sed '/^$/d')
rm -f "$tmp_list"

build_dir=$(ls -d build/*/test 2>/dev/null | head -n1 || true)
if [[ -z "${build_dir:-}" ]]; then
  # Fallback: try to derive path after a dummy run (non-fatal)
  fpm build >/dev/null 2>&1 || true
  build_dir=$(ls -d build/*/test 2>/dev/null | head -n1 || true)
fi

if [[ -z "${build_dir:-}" ]]; then
  echo "ERROR: Could not locate build test directory." >&2
  exit 2
fi

echo "Using test directory: $build_dir"
echo "Running with TEST_JOBS=$TEST_JOBS, TIME_LIMIT=${TIME_LIMIT}s"

mkdir -p logs
results_dir=$(mktemp -d)

run_one() {
  local t="$1"
  local exe="$build_dir/$t"
  if [[ ! -x "$exe" ]]; then
    # Some tests may have different names; try to find matching file
    exe=$(ls "$build_dir" | rg -n "^${t}$" -N -r "$build_dir/$t" || true)
  fi
  if [[ ! -x "$exe" ]]; then
    echo "[SKIP] $t (executable not found)" | tee -a "$results_dir/summary"
    echo "SKIP" >"$results_dir/$t.status"
    return 0
  fi
  echo "[RUN ] $t"
  local code=0
  if command -v timeout >/dev/null 2>&1; then
    timeout -k 5s "${TIME_LIMIT}s" "$exe" >"logs/${t}.log" 2>&1 || code=$?
  else
    "$exe" >"logs/${t}.log" 2>&1 || code=$?
  fi
  if [[ $code -eq 0 ]]; then
    if [[ -n "${XFAIL[$t]:-}" ]]; then
      echo "[XPASS] $t (was xfail: ${XFAIL[$t]})" | tee -a "$results_dir/summary"
      echo "XPASS" >"$results_dir/$t.status"
    else
      echo "[PASS] $t" | tee -a "$results_dir/summary"
      echo "PASS" >"$results_dir/$t.status"
    fi
  elif [[ $code -eq 124 ]]; then
    if [[ -n "${XFAIL[$t]:-}" ]]; then
      echo "[XFAIL] $t -> timeout after ${TIME_LIMIT}s (${XFAIL[$t]})" | tee -a "$results_dir/summary"
      echo "XFAIL" >"$results_dir/$t.status"
    else
      echo "[FAIL] $t -> timeout after ${TIME_LIMIT}s (see logs/${t}.log)" | tee -a "$results_dir/summary"
      echo "FAIL" >"$results_dir/$t.status"
    fi
  else
    if [[ -n "${XFAIL[$t]:-}" ]]; then
      echo "[XFAIL] $t -> exit=$code (${XFAIL[$t]})" | tee -a "$results_dir/summary"
      echo "XFAIL" >"$results_dir/$t.status"
    else
      echo "[FAIL] $t -> exit=$code (see logs/${t}.log)" | tee -a "$results_dir/summary"
      echo "FAIL" >"$results_dir/$t.status"
    fi
  fi
}

# Run tests, optionally in parallel
if [[ $TEST_JOBS -gt 1 ]]; then
  # Background jobs up to TEST_JOBS
  running=0
  pids=()
  for t in "${tests[@]}"; do
    run_one "$t" &
    pids+=("$!")
    ((running++))
    if [[ $running -ge $TEST_JOBS ]]; then
      wait -n || true
      ((running--))
    fi
  done
  # Wait remaining
  for pid in "${pids[@]}"; do
    wait "$pid" || true
  done
else
  for t in "${tests[@]}"; do
    run_one "$t"
  done
fi

# Aggregate results
pass=0; fail=0; xfail=0; xpass=0; skip=0
for f in "$results_dir"/*.status; do
  [[ -e "$f" ]] || continue
  s=$(cat "$f")
  case "$s" in
    PASS) ((pass++)) ;;
    FAIL) ((fail++)) ;;
    XFAIL) ((xfail++)) ;;
    XPASS) ((xpass++)) ;;
    SKIP) ((skip++)) ;;
  esac
done

echo
echo "Summary: PASS=$pass  XFAIL=$xfail  XPASS=$xpass  FAIL=$fail  SKIP=$skip"
rm -rf "$results_dir"
if [[ $fail -gt 0 ]]; then
  exit 1
fi
exit 0
