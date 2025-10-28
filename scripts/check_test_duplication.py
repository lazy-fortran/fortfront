#!/usr/bin/env python3
"""Check for end-to-end tests with inline code (violates CLAUDE.md zero-duplication policy)"""

import sys
import re
from pathlib import Path

def main():
    project_root = Path(__file__).parent.parent
    test_dir = project_root / "test"

    print("=== Checking for End-to-End Test Duplication Violations ===")
    print()
    print("CLAUDE.md policy: End-to-end tests MUST use examples/, not inline code")
    print("Unit tests with small inline code (<10 lines) are OK")
    print()

    violations = []
    warnings = []

    # Scan all test files
    test_files = list(test_dir.glob("**/*.f90"))

    print(f"Scanning {len(test_files)} test files for inline code...")
    print()

    for test_file in test_files:
        try:
            content = test_file.read_text()
        except Exception as e:
            print(f"Warning: Could not read {test_file}: {e}")
            continue

        # Skip if already uses read_example
        if "read_example" in content:
            continue

        # Count new_line('a') occurrences
        count = content.count("new_line('a')")

        if count > 15:
            violations.append((test_file, count))
        elif 6 <= count <= 15:
            warnings.append((test_file, count))

    # Report violations
    if violations:
        print("VIOLATIONS (end-to-end tests with inline code >15 lines):")
        print()
        for test_file, count in sorted(violations, key=lambda x: x[1], reverse=True):
            rel_path = test_file.relative_to(project_root)
            print(f"  {rel_path} ({count} inline lines)")
        print()

    # Report warnings
    if warnings:
        print("WARNINGS (moderate inline code 6-15 lines, review needed):")
        print()
        for test_file, count in sorted(warnings, key=lambda x: x[1], reverse=True):
            rel_path = test_file.relative_to(project_root)
            print(f"  {rel_path} ({count} inline lines)")
        print()

    # Summary
    print("=== Summary ===")
    print(f"VIOLATIONS (must fix): {len(violations)} end-to-end tests with inline code")
    print(f"WARNINGS (review recommended): {len(warnings)} tests with moderate inline code")
    print()

    if violations:
        print("❌ FAIL: Found {} end-to-end test violations".format(len(violations)))
        print()
        print("Migration pattern:")
        print("  1. Extract inline code to examples/f90/ or examples/lf/")
        print("  2. Update test to use: call read_example('examples/.../file.ext', source)")
        print("  3. Verify test still passes with: fpm test <test_name>")
        print()
        print("See CLAUDE.md for complete migration guide")
        return 1
    else:
        print("✓ PASS: No end-to-end test violations found")
        if warnings:
            print(f"Note: {len(warnings)} tests flagged for review (may be acceptable unit/integration tests)")
        return 0

if __name__ == "__main__":
    sys.exit(main())
