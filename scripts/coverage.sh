#!/bin/bash
# Coverage generation script using lcov

set -e

echo "=== Generating code coverage with lcov ==="

# Clean build directory
echo "Cleaning build directory..."
fpm clean --all

# Build and run tests with coverage flags
echo "Building and running tests with coverage..."
fpm test --profile debug --flag '-cpp -fprofile-arcs -ftest-coverage -g'

# Capture coverage data
echo "Capturing coverage data..."
lcov --capture --directory build/ --output-file coverage.info \
  --rc branch_coverage=1 \
  --ignore-errors inconsistent

# Filter out unwanted files
echo "Filtering coverage data..."
lcov --remove coverage.info \
  'build/dependencies/*' \
  'test/*' \
  --output-file coverage_filtered.info \
  --ignore-errors unused

# Generate HTML report
echo "Generating HTML report..."
genhtml coverage_filtered.info --output-directory coverage_html \
  --branch-coverage \
  --legend

# Show summary
echo "=== Coverage Summary ==="
lcov --summary coverage_filtered.info

# Generate XML report for CI/CD if lcov_cobertura is available
if command -v lcov_cobertura &> /dev/null; then
    echo "Generating XML report for CI/CD..."
    lcov_cobertura coverage_filtered.info -o coverage.xml
else
    echo "Note: Install lcov_cobertura (pip install lcov-cobertura) to generate XML reports"
fi

echo "Coverage report generated in coverage_html/index.html"