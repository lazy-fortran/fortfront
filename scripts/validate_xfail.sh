#!/usr/bin/env bash
set -euo pipefail

# Validate that every entry in test/xfail.csv corresponds to an actual
# test executable known to fpm (via `fpm test --list`).

repo_root=$(cd "$(dirname "$0")/.." && pwd)
xfail_file="$repo_root/test/xfail.csv"

if [[ ! -f "$xfail_file" ]]; then
  exit 0
fi

# Get candidate test names from fpm; normalize whitespace and capture stdout/stderr
# Robustly extract names after the "Matched names:" header, trim, and take the first field
mapfile -t candidates < <( \
  fpm test --list 2>&1 \
    | sed -n '/^ Matched names:/,$p' \
    | tail -n +2 \
    | sed 's/[[:space:]]\+$//' \
    | sed '/^$/d' \
    | awk '{print $1}' \
)

# Read xfail names (ignore comments/blank lines)
mapfile -t xfails < <(awk -F, 'NF>=1 && $1 !~ /^#/ && $1!="" {print $1}' "$xfail_file")

# If there are no xfails, nothing to validate
if [[ ${#xfails[@]} -eq 0 ]]; then
  exit 0
fi

# If candidates is empty, treat as an environment/listing failure and skip
# strict validation to avoid false negatives in CI while still allowing tests to run.
if [[ ${#candidates[@]} -eq 0 ]]; then
  echo "WARN: could not discover test names from 'fpm test --list'; skipping xfail validation" >&2
  exit 0
fi

# Compare sets to find dangling xfail entries not present in candidates
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

printf '%s\n' "${xfails[@]}" | sort -u >"$tmpdir/xfails.txt"
printf '%s\n' "${candidates[@]}" | sort -u >"$tmpdir/candidates.txt"

missing=$(comm -23 "$tmpdir/xfails.txt" "$tmpdir/candidates.txt" || true)

if [[ -n "$missing" ]]; then
  echo "ERROR: stale entries in test/xfail.csv (no matching tests):" >&2
  echo "$missing" | sed 's/^/  - /' >&2
  echo "Please remove or correct these entries to avoid XPASS noise." >&2
  exit 1
fi

exit 0
