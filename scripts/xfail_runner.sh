#!/usr/bin/env bash
set -euo pipefail

exe="$1"
name=$(basename "$exe")
repo_root=$(cd "$(dirname "$0")/.." && pwd)
xfail_file="$repo_root/test/xfail.csv"

# Per-repo policy: enforce a strict 300s cap for all test runs
# Allow override via TIME_LIMIT env var for local experimentation
TIME_LIMIT=${TIME_LIMIT:-300}

issue=""
if [[ -f "$xfail_file" ]]; then
  issue=$(awk -F, -v n="$name" 'BEGIN{found=""} $1==n{found=$2} END{print found}' "$xfail_file")
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

  # Portable bash fallback
  "$@" &
  local cmd_pid=$!
  (
    # Watchdog subshell
    sleep "$TIME_LIMIT" && kill -s TERM "$cmd_pid" 2>/dev/null && \
      sleep "$limit_kill" && kill -s KILL "$cmd_pid" 2>/dev/null
  ) &
  local killer_pid=$!

  # Wait for the command to finish
  wait "$cmd_pid"
  local ec=$?

  # If the command finished, stop watchdog; if it didn’t, infer timeout
  if kill -0 "$killer_pid" 2>/dev/null; then
    kill -s TERM "$killer_pid" 2>/dev/null || true
    wait "$killer_pid" 2>/dev/null || true
    return $ec
  else
    # Watchdog already fired; normalize to 124 for timeout
    return 124
  fi
}

# Run the test with a timeout; capture exit code robustly
code=0
run_with_timeout "$exe" > "$repo_root/logs/${name}.log" 2>&1 || code=$? || code=0
code=${code:-0}

# Interpret results, including timeout exit (124)
if [[ $code -eq 0 ]]; then
  if [[ -n "$issue" ]]; then
    echo "[XPASS] $name (was xfail: $issue)"
  else
    echo "[PASS] $name"
  fi
  exit 0
elif [[ $code -eq 124 ]]; then
  # Timeout
  if [[ -n "$issue" ]]; then
    echo "[XFAIL] $name -> timeout after ${TIME_LIMIT}s ($issue)"
    exit 0
  else
    echo "[FAIL] $name -> timeout after ${TIME_LIMIT}s (see logs/${name}.log)"
    exit 1
  fi
else
  if [[ -n "$issue" ]]; then
    echo "[XFAIL] $name -> exit=$code ($issue)"
    exit 0
  else
    echo "[FAIL] $name -> exit=$code (see logs/${name}.log)"
    exit 1
  fi
fi
