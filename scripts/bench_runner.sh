#!/usr/bin/env bash
set -euo pipefail

exe="$1"
name=$(basename "$exe")
repo_root=$(cd "$(dirname "$0")/.." && pwd)

# Default per-benchmark timeout (seconds); override with TIME_LIMIT
TIME_LIMIT=${TIME_LIMIT:-300}

mkdir -p "$repo_root/logs"

code=0
if command -v timeout >/dev/null 2>&1; then
  timeout -k 5s "${TIME_LIMIT}s" "$exe" \
    > "$repo_root/logs/bench_${name}.log" 2>&1 || code=$? || code=0
else
  "$exe" > "$repo_root/logs/bench_${name}.log" 2>&1 || code=$? || code=0
fi
code=${code:-0}

if [[ $code -eq 0 ]]; then
  echo "[BENCH PASS] ${name}"
  exit 0
elif [[ $code -eq 124 ]]; then
  echo "[BENCH FAIL] ${name} -> timeout after ${TIME_LIMIT}s (see logs/bench_${name}.log)"
  exit 1
else
  echo "[BENCH FAIL] ${name} -> exit=$code (see logs/bench_${name}.log)"
  exit 1
fi

