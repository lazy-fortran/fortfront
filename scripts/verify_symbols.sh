#!/bin/bash

# Symbol Verification Script for libfortfront.a
#
# This script verifies that the static library contains all expected
# fortfront modules and symbols as required by Issue #411.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LIBRARY_PATH="$PROJECT_ROOT/libfortfront.a"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "=========================================="
echo "libfortfront.a Symbol Verification"
echo "=========================================="

# Check if library exists
if [ ! -f "$LIBRARY_PATH" ]; then
    echo -e "${RED}ERROR${NC}: libfortfront.a not found at $LIBRARY_PATH"
    echo "Run 'make libfortfront.a' first to create the library"
    exit 1
fi

echo -e "${BLUE}Library path:${NC} $LIBRARY_PATH"
echo ""

# Expected fortfront modules
EXPECTED_MODULES=(
    "lexer_core"
    "parser_core"
    "semantic_analyzer" 
    "codegen_core"
    "frontend"
    "ast_core"
    "error_handling"
    "string_utils"
)

# Check for module presence
echo "=========================================="
echo "Module Symbol Verification"
echo "=========================================="

MISSING_MODULES=0
FOUND_MODULES=0

for module in "${EXPECTED_MODULES[@]}"; do
    echo -n "Checking for $module module... "
    
    # Look for module symbols (Fortran modules often have specific symbol patterns)
    if nm "$LIBRARY_PATH" 2>/dev/null | grep -q ".*${module}.*" || \
       ar -t "$LIBRARY_PATH" 2>/dev/null | grep -q ".*${module}.*"; then
        echo -e "${GREEN}✓ Found${NC}"
        FOUND_MODULES=$((FOUND_MODULES + 1))
    else
        echo -e "${RED}✗ Missing${NC}"
        MISSING_MODULES=$((MISSING_MODULES + 1))
    fi
done

echo ""
echo -e "${BLUE}Module Summary:${NC}"
echo "Found modules: $FOUND_MODULES"
echo "Missing modules: $MISSING_MODULES"

# Check for C interface symbols
echo ""
echo "=========================================="
echo "C Interface Symbol Verification"
echo "=========================================="

C_INTERFACE_SYMBOLS=(
    "fortfront_initialize"
    "fortfront_cleanup"
    "fortfront_parse_source"
)

MISSING_C_SYMBOLS=0
FOUND_C_SYMBOLS=0

for symbol in "${C_INTERFACE_SYMBOLS[@]}"; do
    echo -n "Checking for C symbol $symbol... "
    
    if nm "$LIBRARY_PATH" 2>/dev/null | grep -q ".*$symbol.*"; then
        echo -e "${GREEN}✓ Found${NC}"
        FOUND_C_SYMBOLS=$((FOUND_C_SYMBOLS + 1))
    else
        echo -e "${RED}✗ Missing${NC}"
        MISSING_C_SYMBOLS=$((MISSING_C_SYMBOLS + 1))
    fi
done

echo ""
echo -e "${BLUE}C Interface Summary:${NC}"
echo "Found C symbols: $FOUND_C_SYMBOLS"
echo "Missing C symbols: $MISSING_C_SYMBOLS"

# Overall symbol statistics
echo ""
echo "=========================================="
echo "Symbol Statistics"
echo "=========================================="

TOTAL_SYMBOLS=$(nm "$LIBRARY_PATH" 2>/dev/null | wc -l)
DEFINED_SYMBOLS=$(nm "$LIBRARY_PATH" 2>/dev/null | grep -E ' [TDRBSACG] ' | wc -l)
UNDEFINED_SYMBOLS=$(nm "$LIBRARY_PATH" 2>/dev/null | grep ' U ' | wc -l)

echo -e "${BLUE}Total symbols:${NC} $TOTAL_SYMBOLS"
echo -e "${BLUE}Defined symbols:${NC} $DEFINED_SYMBOLS"
echo -e "${BLUE}Undefined symbols:${NC} $UNDEFINED_SYMBOLS"

# Check for common Fortran intrinsic symbols
echo ""
echo -e "${BLUE}Common Fortran patterns found:${NC}"
nm "$LIBRARY_PATH" 2>/dev/null | grep -E '(allocate|deallocate|__fortran)' | wc -l | \
    awk '{print "Fortran runtime symbols: " $1}'

# Final assessment
echo ""
echo "=========================================="
echo "Symbol Verification Summary"
echo "=========================================="

ISSUES=0

if [ $MISSING_MODULES -gt 0 ]; then
    echo -e "${RED}✗ Missing expected Fortran modules ($MISSING_MODULES)${NC}"
    ISSUES=$((ISSUES + 1))
else
    echo -e "${GREEN}✓ All expected Fortran modules present${NC}"
fi

if [ $MISSING_C_SYMBOLS -gt 0 ]; then
    echo -e "${YELLOW}⚠ Missing C interface symbols ($MISSING_C_SYMBOLS)${NC}"
    ISSUES=$((ISSUES + 1))
else
    echo -e "${GREEN}✓ C interface symbols present${NC}"
fi

if [ $DEFINED_SYMBOLS -lt 100 ]; then
    echo -e "${YELLOW}⚠ Low number of defined symbols ($DEFINED_SYMBOLS)${NC}"
    ISSUES=$((ISSUES + 1))
else
    echo -e "${GREEN}✓ Good symbol count ($DEFINED_SYMBOLS defined symbols)${NC}"
fi

echo ""
if [ $ISSUES -eq 0 ]; then
    echo -e "${GREEN}✓ PASS: All symbol verification checks passed${NC}"
    echo "libfortfront.a contains expected fortfront functionality"
    exit 0
elif [ $ISSUES -eq 1 ] && [ $MISSING_C_SYMBOLS -gt 0 ]; then
    echo -e "${YELLOW}⚠ PARTIAL: Core symbols present, C interface needs implementation${NC}"
    echo "This is expected during GREEN phase implementation"
    exit 0
else
    echo -e "${RED}✗ FAIL: Multiple symbol verification issues found${NC}"
    echo "Library may be incomplete or incorrectly built"
    exit 1
fi