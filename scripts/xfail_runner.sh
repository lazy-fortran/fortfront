#!/usr/bin/env bash
set -euo pipefail

exe="$1"
name=$(basename "$exe")
repo_root=$(cd "$(dirname "$0")/.." && pwd)
xfail_file="$repo_root/test/xfail.csv"

# Per-repo policy: enforce a strict 120s cap for all test runs
# Allow override via TIME_LIMIT env var for local experimentation
TIME_LIMIT=${TIME_LIMIT:-120}

issue=""
# Support both exact names and glob patterns in test/xfail.csv
if [[ -f "$xfail_file" ]]; then
  # Exact match fast-path
  issue=$(awk -F, -v n="$name" 'BEGIN{found=""} $1==n{gsub(/\r$/, "", $2); found=$2} END{print found}' "$xfail_file")
  if [[ -z "$issue" ]]; then
    # Pattern match: treat entries containing glob metacharacters (*, ?, [) as shell globs
    while IFS=, read -r pat url; do
      # Trim spaces
      pat=$(echo "$pat" | sed 's/^ *//;s/ *$//')
      url=$(echo "$url" | sed 's/^ *//;s/ *$//')
      # Skip comments/blank lines
      [[ -z "$pat" ]] && continue
      [[ "$pat" =~ ^# ]] && continue
      # Only consider globs here; exact names already handled above
      if [[ "$pat" == *[*?[]* ]]; then
        if [[ "$name" == $pat ]]; then
          issue="$url"
          break
        fi
      fi
    done < "$xfail_file"
  fi
fi

mkdir -p "$repo_root/logs"

# Prune old logs to prevent repository bloat (infra hygiene)
# - Remove logs older than LOG_RETENTION_DAYS (default: 7)
# - Keep directory tidy but non-destructive for recent runs
LOG_RETENTION_DAYS=${LOG_RETENTION_DAYS:-7}
if [ -d "$repo_root/logs" ]; then
  find "$repo_root/logs" -type f -name '*.log' -mtime +"$LOG_RETENTION_DAYS" -delete 2>/dev/null || true
fi

# Internal helper: run a command with a timeout even if GNU timeout is absent.
# Behavior:
# - Returns 124 on timeout, mirroring GNU timeout.
# - Sends SIGTERM at TIME_LIMIT and SIGKILL after 5 seconds.
run_with_timeout() {
  local limit_kill=5
  if command -v timeout >/dev/null 2>&1; then
    timeout -k ${limit_kill}s "${TIME_LIMIT}s" "$@"
    return $?
  fi

  # Portable bash fallback with a timeout sentinel
  local sentinel
  sentinel=$(mktemp)
  rm -f "$sentinel"

  "$@" &
  local cmd_pid=$!
  (
    # Watchdog subshell
    sleep "$TIME_LIMIT" && touch "$sentinel" && \
      kill -s TERM "$cmd_pid" 2>/dev/null && \
      sleep "$limit_kill" && kill -s KILL "$cmd_pid" 2>/dev/null
  ) &
  local killer_pid=$!

  # Wait for the command to finish
  wait "$cmd_pid"
  local ec=$?

  # Stop watchdog if still running
  if kill -0 "$killer_pid" 2>/dev/null; then
    kill -s TERM "$killer_pid" 2>/dev/null || true
    wait "$killer_pid" 2>/dev/null || true
  fi

  # If sentinel exists, normalize to 124 (timeout)
  if [[ -f "$sentinel" ]]; then
    rm -f "$sentinel" 2>/dev/null || true
    return 124
  fi
  return $ec
}

# Run the test with a timeout; capture exit code robustly
# To reduce IO overhead, write logs to a temp file and only persist on failure/XFAIL
tmp_log=$(mktemp)
code=0
run_with_timeout "$exe" >"$tmp_log" 2>&1 || code=$?
code=${code:-0}

# Interpret results, including timeout exit (124)
if [[ $code -eq 0 ]]; then
  if [[ -n "$issue" ]]; then
    echo "[XPASS] $name (was xfail: $issue)"
  else
    echo "[PASS] $name"
  fi
  # Passing test: drop log to save disk/time
  rm -f "$tmp_log" 2>/dev/null || true
  exit 0
elif [[ $code -eq 124 ]]; then
  # Timeout
  if [[ -n "$issue" ]]; then
    # Persist log for reference
    mkdir -p "$repo_root/logs"; mv -f "$tmp_log" "$repo_root/logs/${name}.log" 2>/dev/null || true
    echo "[XFAIL] $name -> timeout after ${TIME_LIMIT}s ($issue)"
    exit 0
  else
    mkdir -p "$repo_root/logs"; mv -f "$tmp_log" "$repo_root/logs/${name}.log" 2>/dev/null || true
    echo "[FAIL] $name -> timeout after ${TIME_LIMIT}s (see logs/${name}.log)"
    exit 1
  fi
else
  if [[ -n "$issue" ]]; then
    mkdir -p "$repo_root/logs"; mv -f "$tmp_log" "$repo_root/logs/${name}.log" 2>/dev/null || true
    echo "[XFAIL] $name -> exit=$code ($issue)"
    exit 0
  else
    mkdir -p "$repo_root/logs"; mv -f "$tmp_log" "$repo_root/logs/${name}.log" 2>/dev/null || true
    echo "[FAIL] $name -> exit=$code (see logs/${name}.log)"
    exit 1
  fi
fi
