#!/bin/bash

# Selective Test Runner for CI Infrastructure
# 
# This script provides optimized test execution for CI environments without coverage logic.

set -euo pipefail

echo "Selective Test Runner - CI Infrastructure"
echo "========================================"

# Basic environment validation
if ! command -v fpm >/dev/null 2>&1; then
    echo "ERROR: fpm is not installed or not on PATH" >&2
    exit 127
fi

echo

# Common FPM flags for all test runs
FPM_FLAGS="-cpp -fmax-stack-var-size=524288"

# Timeout configuration (seconds)
# Full mode can take longer but should still be bounded
TIMEOUT_FULL=${TIMEOUT_FULL:-600}
# Fast mode runs a focused subset and should be quick
TIMEOUT_FAST=${TIMEOUT_FAST:-180}

# Helper to run a command with timeout when available
run_with_timeout() {
    local t="$1"; shift
    if command -v timeout >/dev/null 2>&1; then
        # Prefer preserving child status when supported (GNU coreutils)
        if timeout --help 2>&1 | grep -q -- "--preserve-status"; then
            timeout --preserve-status "${t}" "$@"
        else
            # BusyBox/other implementations: no preserve flag available
            timeout "${t}" "$@"
        fi
    else
        # Fallback: no timeout available
        "$@"
    fi
}

echo "Running full test suite (no coverage)..."

# Run full suite with timeout and reasonable flags
FLAGS="-O0"
set +e
run_with_timeout "$TIMEOUT_FULL" fpm test --flag "$FPM_FLAGS $FLAGS" --verbose
rc=$?
set -e

if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ] || [ "$rc" -eq 143 ]; then
    echo "ERROR: test suite timed out after ${TIMEOUT_FULL}s (rc=$rc)" >&2
    exit 124
elif [ "$rc" -ne 0 ]; then
    echo "ERROR: test suite failed with exit code $rc" >&2
    exit "$rc"
fi

echo "Testing completed"

echo
echo "Selective test execution completed"
echo "CI infrastructure status: ready for further validation"
