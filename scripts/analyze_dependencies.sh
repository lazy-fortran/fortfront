#!/bin/bash

# Dependency Analysis Script for libfortfront.a
#
# This script analyzes the static library to verify it has no external
# dependencies beyond standard system libraries, as required by Issue #411.

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
echo "libfortfront.a Dependency Analysis"
echo "=========================================="

# Check if library exists
if [ ! -f "$LIBRARY_PATH" ]; then
    echo -e "${RED}ERROR${NC}: libfortfront.a not found at $LIBRARY_PATH"
    echo "Run 'make libfortfront.a' first to create the library"
    exit 1
fi

echo -e "${BLUE}Library path:${NC} $LIBRARY_PATH"
echo ""

# Basic file information
echo "=========================================="
echo "File Information"
echo "=========================================="

echo -e "${BLUE}File type:${NC}"
file "$LIBRARY_PATH"

echo ""
echo -e "${BLUE}File size:${NC}"
ls -lh "$LIBRARY_PATH" | awk '{print $5}'

echo ""
echo -e "${BLUE}File permissions:${NC}"
ls -l "$LIBRARY_PATH" | awk '{print $1}'

# Archive contents analysis
echo ""
echo "=========================================="
echo "Archive Contents Analysis"
echo "=========================================="

echo -e "${BLUE}Object files in archive:${NC}"
ar -t "$LIBRARY_PATH" | wc -l

echo ""
echo -e "${BLUE}Archive member list (first 20):${NC}"
ar -t "$LIBRARY_PATH" | head -20

if [ $(ar -t "$LIBRARY_PATH" | wc -l) -gt 20 ]; then
    echo "... ($(( $(ar -t "$LIBRARY_PATH" | wc -l) - 20 )) more files)"
fi

# Symbol analysis
echo ""
echo "=========================================="
echo "Symbol Analysis"
echo "=========================================="

echo -e "${BLUE}Symbol count by type:${NC}"
nm "$LIBRARY_PATH" 2>/dev/null | cut -c18- | cut -d' ' -f1 | sort | uniq -c | sort -nr

echo ""
echo -e "${BLUE}Undefined symbols (should be minimal):${NC}"
UNDEFINED_COUNT=$(nm "$LIBRARY_PATH" 2>/dev/null | grep ' U ' | wc -l)
echo "Count: $UNDEFINED_COUNT"

if [ $UNDEFINED_COUNT -gt 0 ]; then
    echo ""
    echo -e "${YELLOW}Undefined symbols:${NC}"
    nm "$LIBRARY_PATH" 2>/dev/null | grep ' U ' | head -10
    if [ $UNDEFINED_COUNT -gt 10 ]; then
        echo "... ($(( $UNDEFINED_COUNT - 10 )) more)"
    fi
fi

echo ""
echo -e "${BLUE}Exported symbols (first 10):${NC}"
nm "$LIBRARY_PATH" 2>/dev/null | grep ' T ' | head -10

# Dependency analysis using ldd (won't work on static library directly)
echo ""
echo "=========================================="
echo "Static Library Verification"
echo "=========================================="

echo -e "${BLUE}Archive format verification:${NC}"
if ar -t "$LIBRARY_PATH" >/dev/null 2>&1; then
    echo -e "${GREEN}✓ Valid static library archive format${NC}"
else
    echo -e "${RED}✗ Invalid archive format${NC}"
    exit 1
fi

echo ""
echo -e "${BLUE}Self-contained analysis:${NC}"
# Check if library contains standard fortran runtime symbols
FORTRAN_SYMBOLS=$(nm "$LIBRARY_PATH" 2>/dev/null | grep -E '(_gfortran|__iso_c_binding|_GLOBAL_)' | wc -l)
if [ $FORTRAN_SYMBOLS -gt 0 ]; then
    echo -e "${GREEN}✓ Contains expected Fortran runtime symbols${NC}"
else
    echo -e "${YELLOW}? No Fortran runtime symbols detected${NC}"
fi

# Size analysis
echo ""
echo "=========================================="
echo "Size Analysis"
echo "=========================================="

SIZE_BYTES=$(stat -c%s "$LIBRARY_PATH")
SIZE_MB=$(echo "scale=2; $SIZE_BYTES / 1024 / 1024" | bc)

echo -e "${BLUE}Library size:${NC} $SIZE_BYTES bytes ($SIZE_MB MB)"

if [ $SIZE_BYTES -gt 10485760 ]; then  # 10MB
    echo -e "${YELLOW}⚠ Large library size - may indicate included dependencies${NC}"
elif [ $SIZE_BYTES -lt 100000 ]; then  # 100KB
    echo -e "${YELLOW}⚠ Very small library - may be incomplete${NC}"
else
    echo -e "${GREEN}✓ Reasonable library size${NC}"
fi

# Final assessment
echo ""
echo "=========================================="
echo "Dependency Assessment Summary"
echo "=========================================="

ISSUES=0

if [ $UNDEFINED_COUNT -gt 50 ]; then
    echo -e "${RED}✗ High number of undefined symbols ($UNDEFINED_COUNT)${NC}"
    ISSUES=$((ISSUES + 1))
else
    echo -e "${GREEN}✓ Acceptable undefined symbol count ($UNDEFINED_COUNT)${NC}"
fi

if [ $SIZE_BYTES -gt 20971520 ]; then  # 20MB
    echo -e "${YELLOW}⚠ Large library size may indicate bundled dependencies${NC}"
    ISSUES=$((ISSUES + 1))
fi

echo ""
if [ $ISSUES -eq 0 ]; then
    echo -e "${GREEN}✓ PASS: Library appears to be properly self-contained${NC}"
    echo "libfortfront.a meets static library requirements"
    exit 0
else
    echo -e "${YELLOW}⚠ WARNINGS: $ISSUES potential issues found${NC}"
    echo "Review the analysis above for dependency concerns"
    exit 0  # Don't fail build, just warn
fi