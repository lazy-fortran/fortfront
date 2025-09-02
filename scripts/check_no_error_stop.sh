#!/usr/bin/env bash
set -euo pipefail

# Enforce: no 'error stop' statements in production sources (src/)
# Prints offending locations and exits non-zero if any are found.

repo_root=$(cd "$(dirname "$0")/.." && pwd)
src_dir="$repo_root/src"

if [[ ! -d "$src_dir" ]]; then
  echo "ERROR: src/ directory not found at $repo_root" >&2
  exit 2
fi

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT

# Find Fortran sources and scan each line with comments stripped.
# Match standalone keyword sequence: error [spaces] stop
found=0
while IFS= read -r -d '' f; do
  awk -v file="$f" '
    {
      code=$0
      sub(/!.*/, "", code)            # strip trailing comments
      if (code ~ /(^|[^A-Za-z_])error[ \t]*stop([^A-Za-z_]|$)/) {
        printf("%s:%d:%s\n", file, NR, $0)
      }
    }
  ' "$f"
done < <(find "$src_dir" -type f -name '*.f90' -print0) >"$tmp" || true

if [[ -s "$tmp" ]]; then
  echo "ERROR: Disallowed 'error stop' statements found in production sources:" >&2
  cat "$tmp" >&2
  exit 1
fi

echo "PASS: No 'error stop' statements in src/"
exit 0

