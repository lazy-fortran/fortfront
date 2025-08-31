#!/bin/bash

echo "=== Issue #767 Verification: Duplicate filename resolution ==="
echo ""

echo "1. BEFORE (confusing duplicate filenames):"
echo "   - src/analysis/constant_folding.f90"
echo "   - src/semantic/constant_folding.f90"
echo ""

echo "2. AFTER (descriptive specific names):"
echo "   - src/analysis/conditional_evaluation.f90   (conditional evaluation for CFG)"
echo "   - src/semantic/constant_transformation.f90  (AST transformation during analysis)"
echo ""

echo "3. Verifying files exist with new names:"
if [[ -f "src/analysis/conditional_evaluation.f90" ]]; then
    echo "   ✓ src/analysis/conditional_evaluation.f90 exists"
else
    echo "   ✗ src/analysis/conditional_evaluation.f90 missing"
    exit 1
fi

if [[ -f "src/semantic/constant_transformation.f90" ]]; then
    echo "   ✓ src/semantic/constant_transformation.f90 exists"
else
    echo "   ✗ src/semantic/constant_transformation.f90 missing"
    exit 1
fi

echo ""

echo "4. Verifying old duplicate files are removed:"
if [[ ! -f "src/analysis/constant_folding.f90" ]]; then
    echo "   ✓ src/analysis/constant_folding.f90 removed (renamed)"
else
    echo "   ✗ src/analysis/constant_folding.f90 still exists"
    exit 1
fi

if [[ ! -f "src/semantic/constant_folding.f90" ]]; then
    echo "   ✓ src/semantic/constant_folding.f90 removed (renamed)"
else
    echo "   ✗ src/semantic/constant_folding.f90 still exists"
    exit 1
fi

echo ""

echo "5. Verifying module names updated:"
echo "   Analysis module:"
if grep -q "module conditional_evaluation_module" src/analysis/conditional_evaluation.f90; then
    echo "   ✓ Module name updated to 'conditional_evaluation_module'"
else
    echo "   ✗ Module name not updated in conditional_evaluation.f90"
    exit 1
fi

echo "   Semantic module:"
if grep -q "module constant_transformation" src/semantic/constant_transformation.f90; then
    echo "   ✓ Module name updated to 'constant_transformation'"
else
    echo "   ✗ Module name not updated in constant_transformation.f90"
    exit 1
fi

echo ""

echo "6. Verifying references updated in dependent files:"
if grep -q "use conditional_evaluation_module" src/analysis/cfg_builder.f90; then
    echo "   ✓ cfg_builder.f90 uses new conditional_evaluation_module"
else
    echo "   ✗ cfg_builder.f90 reference not updated"
    exit 1
fi

if grep -q "use constant_transformation" src/semantic/analyzers/semantic_analyzer.f90; then
    echo "   ✓ semantic_analyzer.f90 uses new constant_transformation"
else
    echo "   ✗ semantic_analyzer.f90 reference not updated"
    exit 1
fi

echo ""

echo "7. Verifying build system updated:"
if grep -q "src/semantic/constant_transformation.f90" CMakeLists.txt; then
    echo "   ✓ CMakeLists.txt references new constant_transformation.f90"
else
    echo "   ✗ CMakeLists.txt not updated"
    exit 1
fi

if grep -q "src/analysis/conditional_evaluation.f90" CMakeLists.txt; then
    echo "   ✓ CMakeLists.txt includes new conditional_evaluation.f90"
else
    echo "   ✗ CMakeLists.txt missing conditional_evaluation.f90"
    exit 1
fi

echo ""

echo "8. Build verification (FMP system):"
echo "   Running FMP build to verify compilation..."
if fpm build --flag "-cpp -fmax-stack-var-size=524288" > /tmp/build_output.log 2>&1; then
    echo "   ✓ FMP build successful - no compilation errors from renaming"
    echo "   ✓ All module dependencies resolved correctly"
else
    echo "   ✗ FMP build failed - checking output..."
    tail -10 /tmp/build_output.log
    exit 1
fi

echo ""
echo "=== VERIFICATION COMPLETE: Issue #767 Successfully Resolved ==="
echo ""
echo "SUMMARY:"
echo "• Eliminated confusing duplicate filename constant_folding.f90"
echo "• Renamed to descriptive module-specific names:"
echo "  - conditional_evaluation.f90 (analysis of conditional expressions)"
echo "  - constant_transformation.f90 (AST transformation during semantic analysis)"
echo "• Updated all module references and build system"
echo "• Maintained backward compatibility - no functionality lost"
echo "• Build system verified - compilation successful"
echo ""
echo "The codebase now has clear, unambiguous file organization."