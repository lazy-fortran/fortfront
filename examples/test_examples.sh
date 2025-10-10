#!/bin/bash

# Test all examples in the examples directory
# This script now calls the integrated Fortran test suite

echo "Testing fortfront examples..."
echo "Date: $(date)"
echo

# Run the Fortran integration test
fpm test test_all_examples
