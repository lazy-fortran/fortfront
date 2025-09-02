#!/usr/bin/env bash
set -euo pipefail

# Validate xfail mappings:
# - Each test name exists in `fpm test --list` output.
# - Each mapped GitHub issue URL refers to an OPEN issue.

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
mapfile -t xfails < <(\
  awk -F, 'NF>=1 && $1 !~ /^#/ && $1!="" {gsub(/^ +| +$/, "", $1); print $1}' "$xfail_file"\
)
mapfile -t issue_urls < <(\
  awk -F, 'NF>=2 && $1 !~ /^#/ && $1!="" {gsub(/\r$/, "", $2); gsub(/^ +| +$/, "", $2); print $2}' "$xfail_file"\
)

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

# Validate that referenced issues are open (best-effort; skip non-GitHub URLs)
closed_list=""
skipped_list=""
if command -v gh >/dev/null 2>&1; then
  for url in "${issue_urls[@]:-}"; do
    [[ -z "$url" ]] && continue
    if [[ "$url" =~ ^https://github.com/([^/]+)/([^/]+)/issues/([0-9]+)$ ]]; then
      owner_repo="${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
      num="${BASH_REMATCH[3]}"
      # If the query fails (e.g., no network/unauthenticated), treat as skipped not an error
      state=$(gh issue view "$num" --repo "$owner_repo" --json state --jq .state 2>/dev/null || echo __UNKNOWN__)
      if [[ "$state" == "OPEN" ]]; then
        :
      elif [[ "$state" == "__UNKNOWN__" ]]; then
        skipped_list+="${url} (state query failed)\n"
      else
        closed_list+="${url} (state: ${state})\n"
      fi
    fi
  done
else
  # gh not available; skip issue-state validation to avoid CI flakiness
  if [[ ${#issue_urls[@]} -gt 0 ]]; then
    skipped_list+="gh CLI not found; skipped validation for ${#issue_urls[@]} URLs\n"
  fi
fi

if [[ -n "$closed_list" ]]; then
  echo "ERROR: xfail entries point to closed/non-open issues:" >&2
  printf "%b" "$closed_list" | sed 's/^/  - /' >&2
  echo "Please remove these xfail rows now that tests should pass." >&2
  exit 1
fi

if [[ -n "$skipped_list" ]]; then
  echo "WARN: skipped issue-state validation for some xfail URLs:" >&2
  printf "%b" "$skipped_list" | sed 's/^/  - /' >&2
fi

exit 0
