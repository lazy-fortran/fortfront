#!/usr/bin/env bash
set -euo pipefail

# Prune log files in logs/ to control disk usage.
# Policy:
# - Delete logs older than RETAIN_DAYS (default: 7 days)
# - Keep at most KEEP_COUNT newest logs overall (default: 50)
#
repo_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
log_dir="$repo_root/logs"
RETAIN_DAYS="${RETAIN_DAYS:-7}"
KEEP_COUNT="${KEEP_COUNT:-50}"

if [ ! -d "$log_dir" ]; then
  exit 0
fi

# Delete logs older than RETAIN_DAYS
find "$log_dir" -type f -name "*.log" -mtime +"$RETAIN_DAYS" -print -delete 2>/dev/null || true

# Keep only the newest KEEP_COUNT logs (by mtime)
count=$(find "$log_dir" -type f -name "*.log" | wc -l | awk "{print $1}")
if [ "$count" -gt "$KEEP_COUNT" ]; then
  # List all logs sorted by mtime newest first, skip first KEEP_COUNT, delete the rest
  find "$log_dir" -type f -name "*.log" -printf "%T@ %p\n" | sort -nr | awk -v k="$KEEP_COUNT" "NR>k{print $2}" | xargs -r rm -f
fi

# Also clean stray top-level logs to avoid accumulation
find "$repo_root" -maxdepth 1 -type f \(
  -name "*.log" -o -name "*.err" -o -name "*.out"
\) -mtime +"$RETAIN_DAYS" -print -delete 2>/dev/null || true

