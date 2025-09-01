#!/usr/bin/env bash
set -euo pipefail

exe="$1"
name=$(basename "$exe")
repo_root=$(cd "$(dirname "$0")/.." && pwd)
xfail_file="$repo_root/test/xfail.csv"
issue=""
if [[ -f "$xfail_file" ]]; then
  issue=$(awk -F, -v n="$name" 'BEGIN{found=""} $1==n{found=$2} END{print found}' "$xfail_file")
fi

mkdir -p "$repo_root/logs"
"$exe" > "$repo_root/logs/${name}.log" 2>&1 || code=$? || code=0
code=${code:-0}

if [[ $code -eq 0 ]]; then
  if [[ -n "$issue" ]]; then
    echo "[XPASS] $name (was xfail: $issue)"
  else
    echo "[PASS] $name"
  fi
  exit 0
else
  if [[ -n "$issue" ]]; then
    echo "[XFAIL] $name -> exit=$code ($issue)"
    exit 0
  else
    echo "[FAIL] $name -> exit=$code (see logs/${name}.log)"
    exit 1
  fi
fi

