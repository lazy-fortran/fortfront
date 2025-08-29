#!/bin/bash

# ci_fraud_prevention.sh - CI integration for fraud prevention
# Mandatory compilation validation before merge approval

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "======================================================================"
echo "CI FRAUD PREVENTION GATE"
echo "Mandatory validation before merge approval"
echo "======================================================================"

# Check if we're in a CI environment
if [[ "$CI" == "true" ]]; then
    echo "Running in CI environment: $GITHUB_EVENT_NAME"
    
    # Only run fraud prevention on PRs and main branch
    if [[ "$GITHUB_EVENT_NAME" == "pull_request" ]] || [[ "$GITHUB_REF" == "refs/heads/main" ]]; then
        echo "Fraud prevention validation required"
    else
        echo "Skipping fraud prevention for non-critical workflow"
        exit 0
    fi
else
    echo "Running locally - full validation"
fi

# Run the fraud-proof validator
echo "Running fraud-proof compilation validator..."
if "$SCRIPT_DIR/fraud_proof_validator.sh"; then
    echo -e "${GREEN}✅ FRAUD PREVENTION PASSED${NC}"
    echo "Generated code compiles successfully - no fraud detected"
    
    # Create success marker for CI
    if [[ "$CI" == "true" ]]; then
        echo "fraud-prevention-status=success" >> "$GITHUB_OUTPUT"
    fi
    
    exit 0
else
    echo -e "${RED}❌ FRAUD PREVENTION FAILED${NC}"
    echo "Generated code contains compilation errors"
    echo "This PR cannot be merged until fraud issues are resolved"
    
    # Create failure marker for CI
    if [[ "$CI" == "true" ]]; then
        echo "fraud-prevention-status=failure" >> "$GITHUB_OUTPUT"
        
        # Add PR comment if possible
        if [[ "$GITHUB_EVENT_NAME" == "pull_request" ]] && [[ -n "$GITHUB_TOKEN" ]]; then
            gh pr comment "${{ github.event.number }}" --body "❌ **FRAUD PREVENTION FAILURE**

This PR generates invalid Fortran code that fails compilation. The fraud prevention system has detected the following issues:

- Generated code contains compilation errors
- This indicates systematic problems in type inference or code generation
- **This PR cannot be merged until these issues are resolved**

Please run the fraud prevention validator locally:
\`\`\`bash
./scripts/fraud_proof_validator.sh
\`\`\`

All tests must pass before merge approval." || true
        fi
    fi
    
    exit 1
fi