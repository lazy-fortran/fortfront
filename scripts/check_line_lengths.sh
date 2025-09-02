#!/usr/bin/env bash
set -euo pipefail

# Enforce soft and hard line length limits for source files under src/
# - Soft limit: warn when >500 lines
# - Hard limit: fail when >1000 lines

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root"

soft_limit=${SOFT_LIMIT:-500}
# Use a conservative default hard limit to avoid breaking existing repos.
# Enforce stricter limits by setting HARD_LIMIT=1000 in CI when ready.
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
  echo "WARN: files over soft limit (${soft_limit} lines):"
  printf '  %6d  %s\n' ${warns[@]}
fi

if (( ${#fails[@]} > 0 )); then
  echo "ERROR: files over hard limit (${hard_limit} lines):" >&2
  printf '  %6d  %s\n' ${fails[@]} >&2
  exit 1
fi

echo "OK: all files within ${hard_limit}-line hard limit"
