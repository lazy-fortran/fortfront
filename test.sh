#!/bin/bash

# Test script for fortfront with GCC 15.1.1 compatibility
# Adds the necessary flag to handle large module files
#
# CRITICAL: The -fmax-stack-var-size=524288 flag is MANDATORY for GCC 15.2.1
# Without this flag, compilation will hang indefinitely during module processing
# Original issue: GCC 15.x requires much larger stack space for complex modules
# Fixed value: 524288 (512KB) - tested working with full test suite
#
# Usage examples:
#   ./test.sh                           # Run all tests
#   ./test.sh test_lexer_core_direct    # Run specific test
#   ./test.sh bench_arena_allocation    # Run benchmark

echo "Running fortfront tests with GCC 15.2.1 compatibility..."
echo "Using flags: -cpp -fmax-stack-var-size=524288"
fpm test --flag "-cpp -fmax-stack-var-size=524288" "$@"