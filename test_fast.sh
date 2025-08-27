#!/bin/bash

# Fast test runner script for performance optimization
# Optimized for CI environments with parallel compilation

set -e

echo "🚀 Fast Test Runner - Performance Optimized"
echo "Available CPU cores: $(nproc)"

# Set parallel compilation
export OMP_NUM_THREADS=$(nproc)
export FPM_CC=gcc
export FPM_CXX=g++
export FPM_FC=gfortran

# Optimized flags for fast compilation
FAST_FLAGS="-cpp -fmax-stack-var-size=131072 -O1 -pipe -march=native"

# Check if we're running in CI with coverage
if [[ "${CI:-false}" == "true" && "${ENABLE_COVERAGE:-false}" == "true" ]]; then
    echo "CI mode with coverage enabled"
    COVERAGE_FLAGS="-fprofile-arcs -ftest-coverage -g1"
    FLAGS="$FAST_FLAGS $COVERAGE_FLAGS"
else
    echo "Local/fast mode - no coverage"
    FLAGS="$FAST_FLAGS"
fi

echo "Compilation flags: $FLAGS"

# Smart cache usage - only clean if absolutely necessary
if [ ! -d "build/dependencies" ] || [ -z "$(ls -A build/dependencies 2>/dev/null)" ]; then
    echo "🧹 Cache miss - performing clean build"
    fpm clean --all
else
    echo "♻️  Using cached build artifacts"
fi

echo "⚡ Running tests with parallel compilation..."
time fpm test --flag "$FLAGS"

exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo "✅ All tests passed!"
else
    echo "❌ Tests failed with exit code $exit_code"
fi

exit $exit_code