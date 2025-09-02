#!/usr/bin/env bash
set -euo pipefail

# Enforce soft and hard file length (line count) limits under src/
# - Soft limit: warn when >500 lines
# - Hard limit: fail when >1000 lines (configurable)

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"

soft_limit=${SOFT_LIMIT:-500}
# Conservative default to avoid breaking existing repos immediately.
# Set HARD_LIMIT=1000 in CI to align with repo hard limit guidance.
hard_limit=${HARD_LIMIT:-1200}

shopt -s nullglob
mapfile -t files < <(find src -type f \( -name '*.f90' -o -name '*.F90' -o -name '*.f95' \) | sort)

warns=()
fails=()

for f in "${files[@]}"; do
  lines=$(wc -l <"$f" | tr -d ' ')
  if (( lines > hard_limit )); then
    fails+=("$lines $f")
  elif (( lines > soft_limit )); then
    warns+=("$lines $f")
  fi
done

if (( ${#warns[@]} > 0 )); then
  echo "WARN: files over soft file-length limit (${soft_limit} lines):"
  printf '  %6d  %s\n' ${warns[@]}
fi

if (( ${#fails[@]} > 0 )); then
  echo "ERROR: files over hard file-length limit (${hard_limit} lines):" >&2
  printf '  %6d  %s\n' ${fails[@]} >&2
  exit 1
fi

echo "OK: all files within ${hard_limit}-line hard file-length limit (override with HARD_LIMIT)"
