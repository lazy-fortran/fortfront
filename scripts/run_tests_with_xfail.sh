#!/usr/bin/env bash
set -euo pipefail

# Simple xfail-aware test runner for fpm projects.
# - Builds tests via `fpm test --list`
# - Executes each test program directly from build tree
# - Treats failures listed in test/xfail.csv as expected (XFAIL)
# - Emits XPASS if an xfail test unexpectedly passes (does not fail the run)

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"

xfail_file="test/xfail.csv"
declare -A XFAIL
if [[ -f "$xfail_file" ]]; then
  # Format: test_name,issue_url
  while IFS=, read -r name url; do
    name=$(echo "$name" | sed 's/^ *//;s/ *$//')
    url=$(echo "$url" | sed 's/^ *//;s/ *$//')
    [[ -n "$name" ]] || continue
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

pass=0
fail=0
xfail=0
xpass=0

for t in "${tests[@]}"; do
  exe="$build_dir/$t"
  if [[ ! -x "$exe" ]]; then
    # Some tests may have different names; try to find matching file
    exe=$(ls "$build_dir" | rg -n "^${t}$" -N -r "$build_dir/$t" || true)
  fi
  if [[ ! -x "$exe" ]]; then
    echo "[SKIP] $t (executable not found)"
    continue
  fi
  echo "[RUN ] $t"
  mkdir -p logs
  set +e
  "$exe" >"logs/${t}.log" 2>&1
  code=$?
  set -e
  if [[ $code -eq 0 ]]; then
    if [[ -n "${XFAIL[$t]:-}" ]]; then
      echo "[XPASS] $t (was xfail: ${XFAIL[$t]})"
      ((xpass++))
    else
      echo "[PASS] $t"
      ((pass++))
    fi
  else
    if [[ -n "${XFAIL[$t]:-}" ]]; then
      echo "[XFAIL] $t -> exit=$code (${XFAIL[$t]})"
      ((xfail++))
    else
      echo "[FAIL] $t -> exit=$code (see logs/${t}.log)"
      ((fail++))
    fi
  fi
done

echo
echo "Summary: PASS=$pass  XFAIL=$xfail  XPASS=$xpass  FAIL=$fail"
if [[ $fail -gt 0 ]]; then
  exit 1
fi
exit 0
