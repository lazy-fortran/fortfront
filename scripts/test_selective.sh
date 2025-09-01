#!/bin/bash

# Selective Test Runner for CI Infrastructure
# 
# This script provides optimized test execution for different CI environments:
# - Fast testing for PR/branch validation (no coverage)
# - Full testing for main branch pushes (with coverage)
# 
# CRITICAL: This script was missing and causing exit code 127 failures

set -euo pipefail

echo "🧪 Selective Test Runner - CI Infrastructure"
echo "=============================================="

# Environment detection
CI_MODE=${ENABLE_COVERAGE:-false}
PARALLEL_JOBS=${OMP_NUM_THREADS:-4}

echo "CI Mode: Coverage enabled = $CI_MODE"
echo "Parallel jobs: $PARALLEL_JOBS"
echo ""

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
        timeout --preserve-status "${t}" "$@"
    else
        # Fallback: no timeout available
        "$@"
    fi
}

if [ "$CI_MODE" = "true" ]; then
    echo "🎯 FULL TESTING MODE (Main branch with coverage)"
    echo "Running comprehensive test suite..."
    
    # Add coverage flags for full testing
    COVERAGE_FLAGS="-O0 --coverage"
    echo "Applying timeout: ${TIMEOUT_FULL}s for full suite"
    set +e
    run_with_timeout "$TIMEOUT_FULL" fpm test --flag "$FPM_FLAGS $COVERAGE_FLAGS" --verbose
    rc=$?
    set -e
    if [ "$rc" -eq 124 ]; then
        echo "❌ Full test suite timed out after ${TIMEOUT_FULL}s" >&2
        exit 124
    elif [ "$rc" -ne 0 ]; then
        echo "❌ Full test suite failed with exit code $rc" >&2
        exit "$rc"
    fi
    
else
    echo "🚀 FAST TESTING MODE (PR/branch validation)"
    echo "Running core test suite without coverage..."
    
    # Fast mode - run essential tests only with optimization
    FAST_FLAGS="-O1"
    
    # Run critical test subset for fast feedback
    echo "Running critical infrastructure tests (timeout ${TIMEOUT_FAST}s each)..."
    set +e
    run_with_timeout "$TIMEOUT_FAST" fpm test test_minimal_bench --flag "$FPM_FLAGS $FAST_FLAGS" --verbose; rc=$?
    if [ "$rc" -eq 124 ]; then echo "❌ test_minimal_bench timed out" >&2; exit 124; fi
    if [ "$rc" -ne 0 ]; then echo "❌ test_minimal_bench failed with exit code $rc" >&2; exit "$rc"; fi

    run_with_timeout "$TIMEOUT_FAST" fpm test test_fortfront_api_parsing --flag "$FPM_FLAGS $FAST_FLAGS" --verbose; rc=$?
    if [ "$rc" -eq 124 ]; then echo "❌ test_fortfront_api_parsing timed out" >&2; exit 124; fi
    if [ "$rc" -ne 0 ]; then echo "❌ test_fortfront_api_parsing failed with exit code $rc" >&2; exit "$rc"; fi

    run_with_timeout "$TIMEOUT_FAST" fpm test test_fortfront_api_integration --flag "$FPM_FLAGS $FAST_FLAGS" --verbose; rc=$?
    if [ "$rc" -eq 124 ]; then echo "❌ test_fortfront_api_integration timed out" >&2; exit 124; fi
    if [ "$rc" -ne 0 ]; then echo "❌ test_fortfront_api_integration failed with exit code $rc" >&2; exit "$rc"; fi
    set -e
    
    echo "✅ Fast testing completed"
fi

echo ""
echo "✅ Selective test execution completed"
echo "CI Infrastructure Status: Ready for further validation"
