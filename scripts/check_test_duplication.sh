#!/bin/bash
# Check for end-to-end tests with inline code (violates CLAUDE.md zero-duplication policy)
# Exit code 0 = pass, 1 = violations found

set -eo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== Checking for End-to-End Test Duplication Violations ==="
echo ""
echo "CLAUDE.md policy: End-to-end tests MUST use examples/, not inline code"
echo "Unit tests with small inline code (<10 lines) are OK"
echo ""

# Find test files with large amounts of inline code (likely end-to-end tests)
violations=0
warned=0

# Check for files with >15 lines of new_line concatenation (definitely end-to-end)
echo "Scanning for end-to-end tests with inline code (>15 new_line lines)..."
echo ""

test_files=$(find "$PROJECT_ROOT/test" -name "*.f90" -type f)

for file in $test_files; do
    # Skip if file uses read_example (already compliant)
    if grep -q "read_example" "$file" 2>/dev/null; then
        continue
    fi

    # Count new_line occurrences
    count=$(grep -o "new_line('a')" "$file" 2>/dev/null | wc -l | tr -d ' ')

    if [ "$count" -gt 15 ]; then
        echo "VIOLATION: $file ($count inline lines)"
        echo "  This appears to be an end-to-end test with inline code"
        echo "  Action: Extract code to examples/ and use read_example()"
        echo ""
        violations=$((violations + 1))
    fi
done

# Warn about medium-sized inline code (needs review)
echo "Checking for tests with moderate inline code (6-15 lines, may need review)..."
echo ""

for file in $test_files; do
    # Skip if file uses read_example
    if grep -q "read_example" "$file" 2>/dev/null; then
        continue
    fi

    # Count new_line occurrences
    count=$(grep -o "new_line('a')" "$file" 2>/dev/null | wc -l | tr -d ' ')

    if [ "$count" -ge 6 ] && [ "$count" -le 15 ]; then
        echo "WARNING: $file ($count inline lines)"
        echo "  Review: Is this a unit test (<10 lines OK) or end-to-end (should use examples/)?"
        echo ""
        warned=$((warned + 1))
    fi
done

# Summary
echo "=== Summary ==="
echo "VIOLATIONS (must fix): $violations end-to-end tests with inline code"
echo "WARNINGS (review recommended): $warned tests with moderate inline code"
echo ""

if [ "$violations" -gt 0 ]; then
    echo "❌ FAIL: Found $violations end-to-end test violations"
    echo ""
    echo "Migration pattern:"
    echo "  1. Extract inline code to examples/f90/ or examples/lf/"
    echo "  2. Update test to use: call read_example('examples/.../file.ext', source)"
    echo "  3. Verify test still passes with: fpm test <test_name>"
    echo ""
    echo "See CLAUDE.md for complete migration guide"
    exit 1
else
    echo "✓ PASS: No end-to-end test violations found"
    if [ "$warned" -gt 0 ]; then
        echo "Note: $warned tests flagged for review (may be acceptable unit/integration tests)"
    fi
    exit 0
fi
