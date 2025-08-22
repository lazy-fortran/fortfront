#!/bin/bash

# Static Library Test Runner for libfortfront.a
#
# **Given-When-Then**: Comprehensive test runner for static library verification
# **Given**: All test files have been created for static library validation
# **When**: Running this script to execute all tests
# **Then**: Should verify libfortfront.a meets all requirements from Issue #411

set -e  # Exit on any error

echo "=========================================="
echo "Static Library Test Runner for Issue #411"
echo "=========================================="

# Test configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BUILD_DIR="${PROJECT_ROOT}/build"
TEST_BUILD_DIR="${PROJECT_ROOT}/test/static_library"
RESULTS_DIR="${TEST_BUILD_DIR}/results"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Create results directory
mkdir -p "${RESULTS_DIR}"

log_test_result() {
    local test_name="$1"
    local result="$2"
    local message="$3"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if [ "$result" = "PASS" ]; then
        echo -e "${GREEN}✓ PASS${NC}: $test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}✗ FAIL${NC}: $test_name - $message"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    echo "$test_name,$result,$message" >> "${RESULTS_DIR}/test_results.csv"
}

echo "Test execution starting at $(date)"
echo "Project root: $PROJECT_ROOT"
echo "Build directory: $BUILD_DIR"
echo ""

# Initialize results file
echo "Test Name,Result,Message" > "${RESULTS_DIR}/test_results.csv"

# Test 1: Check if FPM build generates libfortfront.a
echo "=========================================="
echo "Test 1: Basic Library Generation"
echo "=========================================="

cd "$PROJECT_ROOT"

if fpm build --flag "-cpp -fmax-stack-var-size=65536" > "${RESULTS_DIR}/fpm_build.log" 2>&1; then
    log_test_result "FPM Build" "PASS" "Build completed successfully"
else
    log_test_result "FPM Build" "FAIL" "Build failed - see fmp_build.log"
fi

# Find the generated library
LIBFORTFRONT_PATH=$(find "${BUILD_DIR}" -name "libfortfront.a" -type f | head -1)

if [ -n "$LIBFORTFRONT_PATH" ] && [ -f "$LIBFORTFRONT_PATH" ]; then
    log_test_result "Library File Exists" "PASS" "Found at $LIBFORTFRONT_PATH"
    echo "Library found at: $LIBFORTFRONT_PATH"
else
    log_test_result "Library File Exists" "FAIL" "libfortfront.a not found in build directory"
    LIBFORTFRONT_PATH=""
fi

# Test 2: Run Fortran test programs (these will fail in RED phase)
echo ""
echo "=========================================="
echo "Test 2: Fortran Test Programs (RED Phase)"
echo "=========================================="

for test_file in "${TEST_BUILD_DIR}"/test_*.f90; do
    if [ -f "$test_file" ]; then
        test_name=$(basename "$test_file" .f90)
        echo "Running $test_name..."
        
        # These will fail in RED phase as expected
        if fpm test "$test_name" --flag "-cpp -fmax-stack-var-size=65536" > "${RESULTS_DIR}/${test_name}.log" 2>&1; then
            log_test_result "$test_name" "PASS" "Test passed"
        else
            log_test_result "$test_name" "FAIL" "Expected failure in RED phase - see ${test_name}.log"
        fi
    fi
done

# Test 3: Language Linking Tests (these will fail in RED phase)
echo ""
echo "=========================================="
echo "Test 3: Multi-Language Linking (RED Phase)"
echo "=========================================="

# C linking test
if [ -n "$LIBFORTFRONT_PATH" ] && [ -f "${TEST_BUILD_DIR}/c_test_program.c" ]; then
    echo "Testing C linking..."
    if gcc -static "${TEST_BUILD_DIR}/c_test_program.c" "$LIBFORTFRONT_PATH" -o "${RESULTS_DIR}/test_c" > "${RESULTS_DIR}/c_linking.log" 2>&1; then
        log_test_result "C Linking" "PASS" "C program linked successfully"
        
        # Try to run the C program
        if "${RESULTS_DIR}/test_c" > "${RESULTS_DIR}/c_execution.log" 2>&1; then
            log_test_result "C Execution" "PASS" "C program executed successfully"
        else
            log_test_result "C Execution" "FAIL" "C program failed to execute - see c_execution.log"
        fi
    else
        log_test_result "C Linking" "FAIL" "Expected failure in RED phase - C interface not implemented"
    fi
else
    log_test_result "C Linking" "FAIL" "Library or C test program not found"
fi

# C++ linking test
if [ -n "$LIBFORTFRONT_PATH" ] && [ -f "${TEST_BUILD_DIR}/cpp_test_program.cpp" ]; then
    echo "Testing C++ linking..."
    if g++ -static "${TEST_BUILD_DIR}/cpp_test_program.cpp" "$LIBFORTFRONT_PATH" -o "${RESULTS_DIR}/test_cpp" > "${RESULTS_DIR}/cpp_linking.log" 2>&1; then
        log_test_result "C++ Linking" "PASS" "C++ program linked successfully"
        
        # Try to run the C++ program
        if "${RESULTS_DIR}/test_cpp" > "${RESULTS_DIR}/cpp_execution.log" 2>&1; then
            log_test_result "C++ Execution" "PASS" "C++ program executed successfully"
        else
            log_test_result "C++ Execution" "FAIL" "C++ program failed to execute - see cpp_execution.log"
        fi
    else
        log_test_result "C++ Linking" "FAIL" "Expected failure in RED phase - C++ interface not implemented"
    fi
else
    log_test_result "C++ Linking" "FAIL" "Library or C++ test program not found"
fi

# Fortran linking test
if [ -n "$LIBFORTFRONT_PATH" ] && [ -f "${TEST_BUILD_DIR}/fortran_test_program.f90" ]; then
    echo "Testing Fortran linking..."
    if gfortran -static "${TEST_BUILD_DIR}/fortran_test_program.f90" "$LIBFORTFRONT_PATH" -o "${RESULTS_DIR}/test_fortran" > "${RESULTS_DIR}/fortran_linking.log" 2>&1; then
        log_test_result "Fortran Linking" "PASS" "Fortran program linked successfully"
        
        # Try to run the Fortran program
        if "${RESULTS_DIR}/test_fortran" > "${RESULTS_DIR}/fortran_execution.log" 2>&1; then
            log_test_result "Fortran Execution" "PASS" "Fortran program executed successfully"
        else
            log_test_result "Fortran Execution" "FAIL" "Fortran program failed to execute - see fortran_execution.log"
        fi
    else
        log_test_result "Fortran Linking" "FAIL" "Expected failure in RED phase - module installation not configured"
    fi
else
    log_test_result "Fortran Linking" "FAIL" "Library or Fortran test program not found"
fi

# Rust linking test
if [ -n "$LIBFORTFRONT_PATH" ] && [ -f "${TEST_BUILD_DIR}/Cargo.toml" ]; then
    echo "Testing Rust linking..."
    cd "${TEST_BUILD_DIR}"
    if cargo build > "${RESULTS_DIR}/rust_build.log" 2>&1; then
        log_test_result "Rust Linking" "PASS" "Rust program linked successfully"
        
        # Try to run the Rust program
        if cargo run > "${RESULTS_DIR}/rust_execution.log" 2>&1; then
            log_test_result "Rust Execution" "PASS" "Rust program executed successfully"
        else
            log_test_result "Rust Execution" "FAIL" "Rust program failed to execute - see rust_execution.log"
        fi
    else
        log_test_result "Rust Linking" "FAIL" "Expected failure in RED phase - FFI interface not implemented"
    fi
    cd "$PROJECT_ROOT"
else
    log_test_result "Rust Linking" "FAIL" "Library or Rust test project not found"
fi

# Test 4: Dependency Analysis
echo ""
echo "=========================================="
echo "Test 4: Dependency Analysis"
echo "=========================================="

if [ -n "$LIBFORTFRONT_PATH" ]; then
    echo "Analyzing library dependencies..."
    
    # Check file type
    if file "$LIBFORTFRONT_PATH" > "${RESULTS_DIR}/file_type.log" 2>&1; then
        log_test_result "File Type Analysis" "PASS" "File type analysis completed"
    else
        log_test_result "File Type Analysis" "FAIL" "Could not analyze file type"
    fi
    
    # Check if it's really a static library
    if ar -t "$LIBFORTFRONT_PATH" > "${RESULTS_DIR}/archive_contents.log" 2>&1; then
        log_test_result "Archive Contents" "PASS" "Successfully listed archive contents"
    else
        log_test_result "Archive Contents" "FAIL" "Could not list archive contents"
    fi
    
    # Symbol analysis
    if nm "$LIBFORTFRONT_PATH" > "${RESULTS_DIR}/symbols.log" 2>&1; then
        log_test_result "Symbol Analysis" "PASS" "Symbol analysis completed"
    else
        log_test_result "Symbol Analysis" "FAIL" "Could not analyze symbols"
    fi
    
else
    log_test_result "Dependency Analysis" "FAIL" "No library file to analyze"
fi

# Final Results Summary
echo ""
echo "=========================================="
echo "Test Results Summary"
echo "=========================================="

echo "Total tests: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${YELLOW}Some tests failed (expected in RED phase)${NC}"
    echo "This is expected behavior for TDD RED phase testing."
    echo "Failed tests indicate implementation work needed for GREEN phase."
    exit 1
fi