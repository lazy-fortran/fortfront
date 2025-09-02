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

# Find Fortran sources and scan each line with comments and strings stripped.
# Match standalone keyword sequence: error [spaces] stop (case-insensitive)
found=0
while IFS= read -r -d '' f; do
  awk -v file="$f" '
    BEGIN { IGNORECASE=1 }
    function strip_strings(s,    out, i, c, in_str, sq) {
      out = ""
      in_str = 0
      sq = sprintf("%c", 39)
      for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (in_str) {
          if (c == sq) {
            if (i < length(s) && substr(s, i+1, 1) == sq) {
              i++  # skip the escaped quote
              continue
            } else {
              in_str = 0
            }
          }
          # skip characters while inside string
          continue
        } else {
          if (c == sq) {
            in_str = 1
            continue
          }
          out = out c
        }
      }
      return out
    }
    {
      code=$0
      sub(/!.*/, "", code)            # strip trailing comments
      code=strip_strings(code)          # strip string literals
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
