#!/bin/bash

# fraud_proof_validator.sh - Compilation validation framework
# Prevents fraudulent "resolved" claims by validating generated code compiles

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
FORTFRONT_EXEC=""
TEMP_DIR="/tmp/fortfront_validation_$$"
VALIDATION_LOG="$PROJECT_ROOT/fraud_validation.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "======================================================================"
echo "FRAUD-PROOF COMPILATION VALIDATOR"
echo "Prevents invalid generated code from being merged"
echo "======================================================================"

# Find fortfront executable
find_fortfront() {
    echo "Finding fortfront executable..."
    
    # Check if already built
    FORTFRONT_EXEC=$(find "$PROJECT_ROOT/build" -name "fortfront" -type f -executable 2>/dev/null | head -1)
    
    if [[ -z "$FORTFRONT_EXEC" ]]; then
        echo "No fortfront executable found. Building..."
        cd "$PROJECT_ROOT"
        fpm build --flag "-cpp -fmax-stack-var-size=131072"
        FORTFRONT_EXEC=$(find "$PROJECT_ROOT/build" -name "fortfront" -type f -executable 2>/dev/null | head -1)
    fi
    
    if [[ -z "$FORTFRONT_EXEC" ]]; then
        echo -e "${RED}ERROR: Could not find or build fortfront executable${NC}"
        exit 1
    fi
    
    echo "Using fortfront: $FORTFRONT_EXEC"
}

# Create temp directory
setup_temp() {
    mkdir -p "$TEMP_DIR"
    echo "Temp directory: $TEMP_DIR"
}

# Clean temp directory
cleanup_temp() {
    if [[ -d "$TEMP_DIR" ]]; then
        rm -rf "$TEMP_DIR"
    fi
}

# Trap to ensure cleanup
trap cleanup_temp EXIT

# Validate single input generates compilable code
validate_input() {
    local input="$1"
    local test_name="$2"
    
    echo "----------------------------------------"
    echo "VALIDATING: $test_name"
    echo "INPUT: $input"
    
    # Generate Fortran code
    local generated_file="$TEMP_DIR/${test_name}.f90"
    local object_file="$TEMP_DIR/${test_name}.o"
    
    echo "$input" | "$FORTFRONT_EXEC" > "$generated_file" 2>&1
    
    if [[ $? -ne 0 ]]; then
        echo -e "${RED}❌ GENERATION FAILED${NC}"
        cat "$generated_file"
        return 1
    fi
    
    echo "GENERATED CODE:"
    cat "$generated_file"
    echo ""
    
    # Test compilation
    echo "Testing compilation with gfortran..."
    if gfortran -c "$generated_file" -o "$object_file" 2>&1; then
        echo -e "${GREEN}✅ COMPILATION SUCCESS${NC}"
        echo "$(date): $test_name - SUCCESS" >> "$VALIDATION_LOG"
        return 0
    else
        echo -e "${RED}❌ COMPILATION FAILED${NC}"
        echo "$(date): $test_name - FAILED" >> "$VALIDATION_LOG"
        return 1
    fi
}

# Run comprehensive validation tests
run_validation_suite() {
    local total_tests=0
    local passed_tests=0
    local failed_tests=0
    
    echo "======================================================================"
    echo "RUNNING COMPREHENSIVE VALIDATION SUITE"
    echo "======================================================================"
    
    # Test cases from known fraud issues
    declare -A test_cases=(
        ["integer_assignment"]="a, b, c = 1, 2, 3"
        ["string_assignment"]="name = 'hello'"
        ["string_concatenation"]="result = 'hello' + 'world'"
        ["empty_string"]="result = ''"
        ["mixed_assignment"]="x, name = 42, 'test'"
        ["simple_arithmetic"]="result = 1 + 2"
        ["real_assignment"]="pi = 3.14159"
        ["boolean_assignment"]="flag = true"
        ["complex_expression"]="result = (1 + 2) * 3"
    )
    
    for test_name in "${!test_cases[@]}"; do
        total_tests=$((total_tests + 1))
        echo ""
        
        if validate_input "${test_cases[$test_name]}" "$test_name"; then
            passed_tests=$((passed_tests + 1))
        else
            failed_tests=$((failed_tests + 1))
        fi
    done
    
    echo ""
    echo "======================================================================"
    echo "VALIDATION SUMMARY"
    echo "======================================================================"
    echo "Total Tests: $total_tests"
    echo -e "Passed: ${GREEN}$passed_tests${NC}"
    echo -e "Failed: ${RED}$failed_tests${NC}"
    
    if [[ $failed_tests -gt 0 ]]; then
        echo ""
        echo -e "${RED}❌ FRAUD DETECTED: $failed_tests test(s) generate non-compilable code${NC}"
        echo "These issues must be resolved before merge."
        echo "See $VALIDATION_LOG for full history."
        return 1
    else
        echo ""
        echo -e "${GREEN}✅ ALL TESTS PASS: No compilation fraud detected${NC}"
        return 0
    fi
}

# Main execution
main() {
    echo "Starting fraud-proof validation..."
    echo "Log file: $VALIDATION_LOG"
    
    find_fortfront
    setup_temp
    
    if run_validation_suite; then
        echo -e "${GREEN}VALIDATION COMPLETE: System generates valid Fortran code${NC}"
        exit 0
    else
        echo -e "${RED}VALIDATION FAILED: System generates invalid Fortran code${NC}"
        exit 1
    fi
}

# Run main function if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi