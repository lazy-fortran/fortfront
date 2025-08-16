#!/bin/bash

# Test script for fortfront with GCC 15.1.1 compatibility
# Adds the necessary flag to handle large module files

echo "Running fortfront tests with GCC 15.1.1 compatibility..."
fpm test --flag "-cpp -fmax-stack-var-size=65536" "$@"