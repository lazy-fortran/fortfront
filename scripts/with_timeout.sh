#!/usr/bin/env bash
set -euo pipefail

# Portable timeout wrapper.
# Usage: with_timeout.sh <limit> <command> [args...]
# - <limit>: seconds or value with trailing 's' (e.g., 120 or 120s)
# - On timeout, exits with 124 (GNU timeout convention)

if [[ $# -lt 2 ]]; then
  echo "Usage: $0 <limit[s]> <command> [args...]" >&2
  exit 2
fi

limit_raw="$1"; shift

# Normalize to integer seconds (strip a single trailing 's' if present)
limit_secs="${limit_raw%s}"
if ! [[ "$limit_secs" =~ ^[0-9]+$ ]]; then
  echo "ERROR: unsupported timeout value: '$limit_raw' (expected integer seconds or with trailing 's')" >&2
  exit 2
fi

# Prefer GNU coreutils timeout when available
if command -v timeout >/dev/null 2>&1; then
  # Send SIGTERM at limit, then SIGKILL after 5s grace
  exec timeout -k 5s "${limit_secs}s" "$@"
fi

# Fallback: implement timeout with a watchdog
sentinel=$(mktemp)
rm -f "$sentinel"

"$@" &
cmd_pid=$!
(
  sleep "$limit_secs" && touch "$sentinel" && \
    kill -s TERM "$cmd_pid" 2>/dev/null || true
  sleep 5 && kill -s KILL "$cmd_pid" 2>/dev/null || true
) &
killer_pid=$!

wait "$cmd_pid" || true
ec=$?

# Stop watchdog if still running
if kill -0 "$killer_pid" 2>/dev/null; then
  kill -s TERM "$killer_pid" 2>/dev/null || true
  wait "$killer_pid" 2>/dev/null || true
fi

if [[ -f "$sentinel" ]]; then
  rm -f "$sentinel" 2>/dev/null || true
  exit 124
fi

exit "$ec"

