#!/bin/bash

# Test script for fortfront with GCC 15.1.1 compatibility
# Adds the necessary flag to handle large module files
#
# CRITICAL: The -fmax-stack-var-size=131072 flag is MANDATORY for GCC 15.x
# Without this flag, module reading will fail with:
# "Reading module '*.mod' at line X column Y: Expected right parenthesis"
# Increased from 65536 to 131072 for coverage builds compatibility
#
# Usage examples:
#   ./test.sh                           # Run all tests
#   ./test.sh test_lexer_core_direct    # Run specific test
#   ./test.sh bench_arena_allocation    # Run benchmark

echo "Running fortfront tests with GCC 15.1.1 compatibility..."
echo "Using flags: -cpp -fmax-stack-var-size=131072"
fpm test --flag "-cpp -fmax-stack-var-size=131072" "$@"