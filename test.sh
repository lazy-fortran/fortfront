#!/bin/bash

# Test script for fortfront with GCC 15.2.1 compatibility
# Adds flags for large module files and specific allocatable workarounds

echo "Running fortfront tests with GCC 15.2.1 compatibility..."
fpm test --flag "-cpp -fmax-stack-var-size=65536 -finit-derived" "$@"