#!/bin/bash

# Selective test runner for CI performance optimization
# Runs different test suites based on trigger type

set -e

echo "🎯 Selective Test Runner"

# Determine test mode
if [[ "${GITHUB_EVENT_NAME}" == "push" && "${GITHUB_REF}" == "refs/heads/main" ]]; then
    TEST_MODE="full"
    echo "📋 Full test suite (main branch push)"
elif [[ "${GITHUB_EVENT_NAME}" == "pull_request" ]]; then
    TEST_MODE="fast"
    echo "⚡ Fast test suite (pull request)"
else
    TEST_MODE="local"
    echo "🔧 Local test mode"
fi

# Set compilation flags based on mode
case $TEST_MODE in
    "full")
        # Full coverage testing
        FLAGS="-cpp -fmax-stack-var-size=131072 -fprofile-arcs -ftest-coverage -g1 -O1"
        echo "Flags: $FLAGS"
        ;;
    "fast"|"local")
        # Fast testing without coverage
        FLAGS="-cpp -fmax-stack-var-size=131072 -O1 -pipe -march=native"
        echo "Flags: $FLAGS"
        ;;
esac

# Set parallel compilation
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-$(nproc)}
echo "Using $OMP_NUM_THREADS parallel jobs"

# Smart caching
if [ ! -d "build/dependencies" ] || [ -z "$(ls -A build/dependencies 2>/dev/null)" ]; then
    echo "🧹 Cache miss - cleaning"
    fpm clean --all
else
    echo "♻️  Using cache"
fi

# Run tests
echo "🚀 Running tests..."
start_time=$(date +%s)

if [[ "$TEST_MODE" == "fast" ]]; then
    # For PRs, run core tests only to save time
    echo "Running essential test subset for PR validation"
    fpm test test_frontend_lexer_api test_semantic_simple test_codegen_core_direct --flag "$FLAGS"
else
    # Full test suite
    fpm test --flag "$FLAGS"
fi

end_time=$(date +%s)
duration=$((end_time - start_time))

echo "✅ Tests completed in ${duration}s"

if [ $duration -gt 180 ]; then  # 3 minutes
    echo "⚠️  WARNING: Tests took longer than 3 minutes"
fi

exit 0