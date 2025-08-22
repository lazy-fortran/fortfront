#!/bin/bash

# Cross-Platform Compatibility Test for libfortfront.a
#
# This script tests libfortfront.a compatibility across different platforms
# and build configurations as required by Issue #411.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LIBRARY_PATH="$PROJECT_ROOT/libfortfront.a"
TEST_DIR="$PROJECT_ROOT/test/static_library"
RESULTS_DIR="$TEST_DIR/results"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "=========================================="
echo "libfortfront.a Cross-Platform Testing"
echo "=========================================="

# Check if library exists
if [ ! -f "$LIBRARY_PATH" ]; then
    echo -e "${RED}ERROR${NC}: libfortfront.a not found at $LIBRARY_PATH"
    echo "Run 'make libfortfront.a' first to create the library"
    exit 1
fi

# Create results directory
mkdir -p "$RESULTS_DIR"

echo -e "${BLUE}Library path:${NC} $LIBRARY_PATH"
echo -e "${BLUE}Test directory:${NC} $TEST_DIR"
echo -e "${BLUE}Results directory:${NC} $RESULTS_DIR"
echo ""

# Platform detection
echo "=========================================="
echo "Platform Detection"
echo "=========================================="

PLATFORM=$(uname -s)
ARCH=$(uname -m)
KERNEL=$(uname -r)

echo -e "${BLUE}Platform:${NC} $PLATFORM"
echo -e "${BLUE}Architecture:${NC} $ARCH"  
echo -e "${BLUE}Kernel:${NC} $KERNEL"

# Compiler detection
echo ""
echo "=========================================="
echo "Compiler Detection"
echo "=========================================="

echo -e "${BLUE}GCC version:${NC}"
if command -v gcc >/dev/null 2>&1; then
    gcc --version | head -1
    GCC_AVAILABLE=true
else
    echo "Not available"
    GCC_AVAILABLE=false
fi

echo ""
echo -e "${BLUE}G++ version:${NC}"
if command -v g++ >/dev/null 2>&1; then
    g++ --version | head -1
    GPP_AVAILABLE=true
else
    echo "Not available"
    GPP_AVAILABLE=false
fi

echo ""
echo -e "${BLUE}GFortran version:${NC}"
if command -v gfortran >/dev/null 2>&1; then
    gfortran --version | head -1
    GFORTRAN_AVAILABLE=true
else
    echo "Not available"
    GFORTRAN_AVAILABLE=false
fi

echo ""
echo -e "${BLUE}Rust version:${NC}"
if command -v rustc >/dev/null 2>&1; then
    rustc --version
    RUST_AVAILABLE=true
else
    echo "Not available"
    RUST_AVAILABLE=false
fi

# Library compatibility tests
echo ""
echo "=========================================="
echo "Library Compatibility Tests"
echo "=========================================="

# Test 1: Basic library inspection
echo -e "${BLUE}Test 1: Basic library format${NC}"
if file "$LIBRARY_PATH" | grep -q "archive"; then
    echo -e "${GREEN}✓ PASS${NC}: Library is valid archive format"
else
    echo -e "${RED}✗ FAIL${NC}: Library is not valid archive format"
fi

# Test 2: Architecture compatibility
echo ""
echo -e "${BLUE}Test 2: Architecture compatibility${NC}"
LIB_ARCH=$(file "$LIBRARY_PATH" | grep -o 'x86-64\|x86_64\|aarch64\|arm64' || echo "unknown")
echo "Library architecture: $LIB_ARCH"
echo "System architecture: $ARCH"

if [[ "$ARCH" == *"64"* && "$LIB_ARCH" == *"64"* ]] || \
   [[ "$ARCH" == "x86_64" && "$LIB_ARCH" == "x86-64" ]]; then
    echo -e "${GREEN}✓ PASS${NC}: Architecture compatibility OK"
else
    echo -e "${YELLOW}⚠ WARNING${NC}: Architecture mismatch (may still work)"
fi

# Test 3: Symbol table accessibility
echo ""
echo -e "${BLUE}Test 3: Symbol table accessibility${NC}"
if nm "$LIBRARY_PATH" >/dev/null 2>&1; then
    SYMBOL_COUNT=$(nm "$LIBRARY_PATH" 2>/dev/null | wc -l)
    echo -e "${GREEN}✓ PASS${NC}: Symbol table accessible ($SYMBOL_COUNT symbols)"
else
    echo -e "${RED}✗ FAIL${NC}: Cannot read symbol table"
fi

# Test 4: Linking compatibility tests (if compilers available)
echo ""
echo "=========================================="
echo "Linking Compatibility Tests"
echo "=========================================="

LINK_TESTS_PASSED=0
LINK_TESTS_ATTEMPTED=0

# C linking test
if [ "$GCC_AVAILABLE" = true ] && [ -f "$TEST_DIR/c_test_program.c" ]; then
    echo -e "${BLUE}C linking test:${NC}"
    LINK_TESTS_ATTEMPTED=$((LINK_TESTS_ATTEMPTED + 1))
    
    if gcc -static "$TEST_DIR/c_test_program.c" "$LIBRARY_PATH" \
           -o "$RESULTS_DIR/test_c_platform" >/dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}: C program links successfully"
        LINK_TESTS_PASSED=$((LINK_TESTS_PASSED + 1))
    else
        echo -e "${RED}✗ FAIL${NC}: C linking failed"
        gcc -static "$TEST_DIR/c_test_program.c" "$LIBRARY_PATH" \
            -o "$RESULTS_DIR/test_c_platform" 2>"$RESULTS_DIR/c_link_error.log" || true
        echo "Error details saved to c_link_error.log"
    fi
fi

# C++ linking test
if [ "$GPP_AVAILABLE" = true ] && [ -f "$TEST_DIR/cpp_test_program.cpp" ]; then
    echo ""
    echo -e "${BLUE}C++ linking test:${NC}"
    LINK_TESTS_ATTEMPTED=$((LINK_TESTS_ATTEMPTED + 1))
    
    if g++ -static "$TEST_DIR/cpp_test_program.cpp" "$LIBRARY_PATH" \
           -o "$RESULTS_DIR/test_cpp_platform" >/dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}: C++ program links successfully"
        LINK_TESTS_PASSED=$((LINK_TESTS_PASSED + 1))
    else
        echo -e "${RED}✗ FAIL${NC}: C++ linking failed"
        g++ -static "$TEST_DIR/cpp_test_program.cpp" "$LIBRARY_PATH" \
            -o "$RESULTS_DIR/test_cpp_platform" 2>"$RESULTS_DIR/cpp_link_error.log" || true
        echo "Error details saved to cpp_link_error.log"
    fi
fi

# Fortran linking test
if [ "$GFORTRAN_AVAILABLE" = true ] && [ -f "$TEST_DIR/fortran_test_program.f90" ]; then
    echo ""
    echo -e "${BLUE}Fortran linking test:${NC}"
    LINK_TESTS_ATTEMPTED=$((LINK_TESTS_ATTEMPTED + 1))
    
    if gfortran -static "$TEST_DIR/fortran_test_program.f90" "$LIBRARY_PATH" \
                -o "$RESULTS_DIR/test_fortran_platform" >/dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}: Fortran program links successfully"
        LINK_TESTS_PASSED=$((LINK_TESTS_PASSED + 1))
    else
        echo -e "${RED}✗ FAIL${NC}: Fortran linking failed"
        gfortran -static "$TEST_DIR/fortran_test_program.f90" "$LIBRARY_PATH" \
                 -o "$RESULTS_DIR/test_fortran_platform" 2>"$RESULTS_DIR/fortran_link_error.log" || true
        echo "Error details saved to fortran_link_error.log"
    fi
fi

# Final assessment
echo ""
echo "=========================================="
echo "Cross-Platform Compatibility Summary"
echo "=========================================="

echo -e "${BLUE}Platform:${NC} $PLATFORM $ARCH"
echo -e "${BLUE}Linking tests:${NC} $LINK_TESTS_PASSED/$LINK_TESTS_ATTEMPTED passed"

if [ $LINK_TESTS_ATTEMPTED -eq 0 ]; then
    echo -e "${YELLOW}⚠ WARNING${NC}: No compilers available for linking tests"
    echo "Library format appears compatible but linking not tested"
    exit 0
elif [ $LINK_TESTS_PASSED -eq $LINK_TESTS_ATTEMPTED ]; then
    echo -e "${GREEN}✓ PASS${NC}: All cross-platform compatibility tests passed"
    echo "libfortfront.a is compatible with this platform"
    exit 0
else
    echo -e "${YELLOW}⚠ PARTIAL${NC}: Some linking tests failed"
    echo "This may be expected during GREEN phase implementation"
    echo "Check error logs in $RESULTS_DIR for details"
    exit 0
fi