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

# Run the test with a timeout; capture exit code robustly
code=0
if command -v timeout >/dev/null 2>&1; then
  # Send SIGTERM at TIME_LIMIT, escalate to SIGKILL after 5s.
  # Do not preserve child status so timeout returns 124 on timeout consistently.
  timeout -k 5s "${TIME_LIMIT}s" "$exe" \
    > "$repo_root/logs/${name}.log" 2>&1 || code=$? || code=0
else
  # Fallback: no timeout available; still run and capture code
  "$exe" > "$repo_root/logs/${name}.log" 2>&1 || code=$? || code=0
fi
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
