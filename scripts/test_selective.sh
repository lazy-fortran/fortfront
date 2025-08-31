#!/bin/bash

# Selective Test Runner for CI Infrastructure
# 
# This script provides optimized test execution for different CI environments:
# - Fast testing for PR/branch validation (no coverage)
# - Full testing for main branch pushes (with coverage)
# 
# CRITICAL: This script was missing and causing exit code 127 failures

set -e

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

if [ "$CI_MODE" = "true" ]; then
    echo "🎯 FULL TESTING MODE (Main branch with coverage)"
    echo "Running comprehensive test suite..."
    
    # Add coverage flags for full testing
    COVERAGE_FLAGS="-O0 --coverage"
    fpm test --flag "$FPM_FLAGS $COVERAGE_FLAGS" --verbose
    
else
    echo "🚀 FAST TESTING MODE (PR/branch validation)"
    echo "Running core test suite without coverage..."
    
    # Fast mode - run essential tests only with optimization
    FAST_FLAGS="-O1"
    
    # Run critical test subset for fast feedback
    echo "Running critical infrastructure tests..."
    fpm test test_minimal_bench --flag "$FPM_FLAGS $FAST_FLAGS" --verbose
    fpm test test_fortfront_api_parsing --flag "$FPM_FLAGS $FAST_FLAGS" --verbose
    fpm test test_fortfront_api_integration --flag "$FPM_FLAGS $FAST_FLAGS" --verbose
    
    echo "✅ Fast testing completed"
fi

echo ""
echo "✅ Selective test execution completed"
echo "CI Infrastructure Status: Ready for further validation"