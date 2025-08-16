#!/bin/bash

# Build script for fortfront with GCC 15.1.1 compatibility
# Adds the necessary flag to handle large module files

echo "Building fortfront with GCC 15.1.1 compatibility..."
fpm build --flag "-cpp -fmax-stack-var-size=65536" "$@"