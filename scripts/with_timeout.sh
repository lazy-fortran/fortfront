#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <seconds> <command> [args...]" >&2
    exit 64
fi

limit="$1"
shift

if ! [[ "$limit" =~ ^[0-9]+$ ]]; then
    echo "Timeout must be an integer number of seconds" >&2
    exit 64
fi

if [[ $# -eq 0 ]]; then
    echo "No command specified" >&2
    exit 64
fi

if command -v timeout >/dev/null 2>&1; then
    exec timeout "$limit" "$@"
fi

start_time=$(date +%s)
"$@" &
cmd_pid=$!

trap 'kill -TERM "$cmd_pid" 2>/dev/null || true' INT TERM

while kill -0 "$cmd_pid" 2>/dev/null; do
    now=$(date +%s)
    elapsed=$((now - start_time))
    if (( elapsed >= limit )); then
        kill -TERM "$cmd_pid" 2>/dev/null || true
        sleep 0.2
        kill -KILL "$cmd_pid" 2>/dev/null || true
        wait "$cmd_pid" 2>/dev/null || true
        exit 124
    fi
    sleep 0.1
done

wait "$cmd_pid"
