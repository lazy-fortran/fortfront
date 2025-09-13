#!/bin/bash

# code_validation.sh - Compilation validation framework
# Validates that generated Fortran code compiles

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
FORTFRONT_EXEC=""
TEMP_DIR="/tmp/fortfront_validation_$$"
VALIDATION_LOG="$PROJECT_ROOT/code_validation.log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "======================================================================"
echo "COMPILATION VALIDATOR"
echo "Ensures generated code compiles"
echo "======================================================================"

# Find fortfront executable
find_fortfront() {
    echo "Finding fortfront executable..."
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

setup_temp() { mkdir -p "$TEMP_DIR"; }
cleanup_temp() { [[ -d "$TEMP_DIR" ]] && rm -rf "$TEMP_DIR"; }
trap cleanup_temp EXIT

validate_input() {
    local input="$1"; local test_name="$2"
    echo "----------------------------------------"
    echo "VALIDATING: $test_name"
    [[ "$CI" == "true" ]] && echo "INPUT: [${#input} chars]" || echo "INPUT: $input"
    local generated_file="$TEMP_DIR/${test_name}.f90"
    local object_file="$TEMP_DIR/${test_name}.o"
    local gen_exit_code
    if [[ "$CI" == "true" ]]; then
        timeout 30s bash -c "echo '$input' | '$FORTFRONT_EXEC'" > "$generated_file" 2>&1
        gen_exit_code=$?
    else
        echo "$input" | "$FORTFRONT_EXEC" > "$generated_file" 2>&1
        gen_exit_code=$?
    fi
    if [[ $gen_exit_code -ne 0 ]]; then
        echo -e "${RED}❌ GENERATION FAILED (exit code: $gen_exit_code)${NC}"
        [[ $gen_exit_code -ne 124 ]] && { echo "Generation error output:"; cat "$generated_file"; } || echo "Generation timed out after 30 seconds"
        return 1
    fi
    if [[ ! -s "$generated_file" ]]; then
        echo -e "${RED}❌ GENERATION PRODUCED EMPTY OUTPUT${NC}"; return 1
    fi
    [[ "$CI" == "true" ]] && echo "Generated: $(wc -l < "$generated_file") lines" || { echo "GENERATED CODE:"; cat "$generated_file"; echo; }
    echo "Testing compilation with gfortran..."
    if [[ "$CI" == "true" ]]; then
        if timeout 20s gfortran -c "$generated_file" -o "$object_file" -O0 -w 2>/dev/null; then
            echo -e "${GREEN}✅ COMPILATION SUCCESS${NC}"; echo "$(date): $test_name - SUCCESS" >> "$VALIDATION_LOG"; return 0
        else
            echo -e "${RED}❌ COMPILATION FAILED${NC}"; echo "$(date): $test_name - FAILED" >> "$VALIDATION_LOG"; return 1
        fi
    else
        local compile_output
        if compile_output=$(gfortran -c "$generated_file" -o "$object_file" 2>&1); then
            echo -e "${GREEN}✅ COMPILATION SUCCESS${NC}"; echo "$(date): $test_name - SUCCESS" >> "$VALIDATION_LOG"; return 0
        else
            echo -e "${RED}❌ COMPILATION FAILED${NC}"; echo "$compile_output"; echo "$(date): $test_name - FAILED: $compile_output" >> "$VALIDATION_LOG"; return 1
        fi
    fi
}

run_validation_suite() {
    local total=0 pass=0 fail=0
    echo "======================================================================"
    echo "RUNNING COMPILATION VALIDATION SUITE"
    echo "======================================================================"
    declare -A test_cases=( ["empty_program"]="" ["basic_code_gen"]=" " )
    for name in "${!test_cases[@]}"; do
        total=$((total+1)); echo
        if validate_input "${test_cases[$name]}" "$name"; then pass=$((pass+1)); else fail=$((fail+1)); fi
    done
    echo; echo "======================================================================"; echo "VALIDATION SUMMARY"; echo "======================================================================"
    echo "Total Tests: $total"; echo -e "Passed: ${GREEN}$pass${NC}"; echo -e "Failed: ${RED}$fail${NC}"
    [[ $fail -gt 0 ]] && { echo; echo -e "${RED}❌ $fail test(s) failed to compile${NC}"; echo "See $VALIDATION_LOG for details."; return 1; } || { echo; echo -e "${GREEN}✅ All tests compiled successfully${NC}"; return 0; }
}

main() {
    echo "Starting code validation..."; echo "Log file: $VALIDATION_LOG"
    find_fortfront; setup_temp
    if run_validation_suite; then echo -e "${GREEN}VALIDATION COMPLETE${NC}"; exit 0; else echo -e "${RED}VALIDATION FAILED${NC}"; exit 1; fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi

