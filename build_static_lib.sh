#!/bin/bash
# Build script for libfortfront.a static library
# Issue #416: Foundation static library for pure Fortran integration

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${GREEN}=== Building libfortfront.a Static Library ===${NC}"
echo ""

# Step 1: Clean previous builds
echo -e "${YELLOW}Step 1: Cleaning previous build artifacts...${NC}"
rm -f libfortfront.a
rm -rf fortfront_modules/
echo "Cleaned."

# Step 2: Build the project with fpm
echo -e "${YELLOW}Step 2: Building fortfront with fpm...${NC}"
fpm build --profile release --flag "-cpp -fmax-stack-var-size=65536"
if [ $? -ne 0 ]; then
    echo -e "${RED}Error: fpm build failed${NC}"
    exit 1
fi
echo "Build successful."

# Step 3: Find and copy the static library
echo -e "${YELLOW}Step 3: Copying libfortfront.a to project root...${NC}"
LATEST_LIB=$(find build -name "libfortfront.a" -type f -printf "%T@ %p\n" | sort -n | tail -1 | cut -d' ' -f2-)
if [ -z "$LATEST_LIB" ]; then
    echo -e "${RED}Error: libfortfront.a not found in build directory${NC}"
    exit 1
fi
cp "$LATEST_LIB" ./libfortfront.a
echo "Copied from: $LATEST_LIB"

# Step 4: Collect all module files
echo -e "${YELLOW}Step 4: Collecting Fortran module files...${NC}"
mkdir -p fortfront_modules

# Find the directory with the most recent module files
MOD_DIR=$(find build -name "*.mod" -type f -printf "%T@ %h\n" | sort -n | tail -1 | cut -d' ' -f2-)
if [ -z "$MOD_DIR" ]; then
    echo -e "${RED}Error: No module files found${NC}"
    exit 1
fi

# Copy all fortfront-specific modules (exclude stdlib and json-fortran)
MOD_COUNT=0
for mod in $(find "$MOD_DIR" -name "*.mod" -type f); do
    MOD_NAME=$(basename "$mod")
    # Skip external dependency modules
    if [[ ! "$MOD_NAME" =~ ^(stdlib_|json_|iso_) ]]; then
        cp "$mod" fortfront_modules/
        MOD_COUNT=$((MOD_COUNT + 1))
    fi
done
echo "Collected $MOD_COUNT module files in fortfront_modules/"

# Step 5: Verify library contents
echo -e "${YELLOW}Step 5: Verifying library contents...${NC}"
SYMBOL_COUNT=$(nm libfortfront.a 2>/dev/null | grep -E "T __(fortfront|lexer|parser|semantic|codegen|ast|scope|type_system|frontend)" | wc -l)
echo "Found $SYMBOL_COUNT exported symbols"

# Step 6: Check library size
LIB_SIZE=$(stat -c%s libfortfront.a)
LIB_SIZE_MB=$((LIB_SIZE / 1048576))
echo "Library size: ${LIB_SIZE_MB} MB"

# Step 7: Test static library format
echo -e "${YELLOW}Step 6: Verifying archive format...${NC}"
file libfortfront.a | grep -q "ar archive"
if [ $? -eq 0 ]; then
    echo "Archive format: valid"
else
    echo -e "${RED}Error: Invalid archive format${NC}"
    exit 1
fi

# Summary
echo ""
echo -e "${GREEN}=== Build Complete ===${NC}"
echo "Static library: libfortfront.a"
echo "Module files: fortfront_modules/"
echo ""
echo "To use libfortfront in external Fortran programs:"
echo "  1. Add fortfront_modules/ to your module path (-I fortfront_modules/)"
echo "  2. Link with libfortfront.a"
echo "  3. Example: gfortran -I fortfront_modules/ my_tool.f90 libfortfront.a -o my_tool"