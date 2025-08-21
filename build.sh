#!/bin/bash

# Build script for fortfront with GCC 15.2.1 compatibility
# Adds necessary flags for large module files and safer allocatable handling

echo "Building fortfront with GCC 15.2.1 compatibility..."
fpm build --flag "-cpp -fmax-stack-var-size=65536 -finit-derived -finit-local-zero -fcheck=all -g -O0" --profile debug "$@"