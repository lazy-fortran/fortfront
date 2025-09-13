#!/bin/bash

# ci_code_validation.sh - CI integration for code validation
# Mandatory compilation validation before merge approval

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "======================================================================"
echo "CODE VALIDATION GATE"
echo "Mandatory validation before merge approval"
echo "======================================================================"

# Check if we're in a CI environment
if [[ "$CI" == "true" ]]; then
    echo "Running in CI environment: $GITHUB_EVENT_NAME"
    # Only run on PRs and main branch
    if [[ "$GITHUB_EVENT_NAME" == "pull_request" ]] || [[ "$GITHUB_REF" == "refs/heads/main" ]]; then
        echo "Code validation required"
    else
        echo "Skipping code validation for non-critical workflow"
        exit 0
    fi
else
    echo "Running locally - full validation"
fi

echo "Running compilation validator..."
if "$SCRIPT_DIR/code_validation.sh"; then
    echo "✅ CODE VALIDATION PASSED"
    exit 0
else
    echo "❌ CODE VALIDATION FAILED"
    exit 1
fi

