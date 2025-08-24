#!/bin/bash

# Build script for fortfront with GCC 15.1.1 compatibility
# Adds the necessary flag to handle large module files
#
# CRITICAL: The -fmax-stack-var-size=131072 flag is MANDATORY for GCC 15.x
# Without this flag, module reading will fail with:
# "Reading module '*.mod' at line X column Y: Expected right parenthesis"
# Increased from 65536 to 131072 for coverage builds compatibility

echo "Building fortfront with GCC 15.1.1 compatibility..."
echo "Using flags: -cpp -fmax-stack-var-size=131072"
fpm build --flag "-cpp -fmax-stack-var-size=131072" "$@"