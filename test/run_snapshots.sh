#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SNAPSHOTS_DIR="$SCRIPT_DIR/snapshots"
CASES_DIR="$SNAPSHOTS_DIR/cases"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
FORTFRONT="$(find "$PROJECT_ROOT/build" -name "fortfront" -type f 2>/dev/null | head -1)"

if [[ -z "$FORTFRONT" ]]; then
    FORTFRONT="$PROJECT_ROOT/build/gfortran_debug/app/fortfront"
fi

UPDATE_MODE=0
VERBOSE=0
FAILED=0
PASSED=0

usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Run snapshot tests for fortfront transpiler.

OPTIONS:
    --update    Update all snapshots with current output (use with caution)
    --verbose   Show detailed output for each test
    -h, --help  Show this help message

WORKFLOW:
    1. Add test: Create .lf or .f90 file in test/snapshots/cases/
    2. Generate expected: Run with --update to create .expected file
    3. Review: Check git diff to verify expected output is correct
    4. Commit: Add both input and .expected files
    5. Verify: Run without --update to validate tests pass

EXAMPLES:
    $0                  # Run all snapshot tests
    $0 --update         # Update all expected outputs
    $0 --verbose        # Show detailed test output
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --update)
            UPDATE_MODE=1
            shift
            ;;
        --verbose)
            VERBOSE=1
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage
            exit 1
            ;;
    esac
done

if [[ ! -x "$FORTFRONT" ]]; then
    echo "Error: fortfront executable not found at $FORTFRONT" >&2
    echo "Please build the project first with: fpm build" >&2
    exit 1
fi

if [[ ! -d "$SNAPSHOTS_DIR" ]]; then
    echo "Error: snapshots directory not found at $SNAPSHOTS_DIR" >&2
    exit 1
fi

if [[ ! -d "$CASES_DIR" ]]; then
    echo "Error: snapshots cases directory not found at $CASES_DIR" >&2
    exit 1
fi

run_snapshot_test() {
    local input_file="$1"
    local basename="${input_file%.*}"
    local expected_file="${basename}.expected"
    local actual_file="${basename}.actual"

    if [[ $VERBOSE -eq 1 ]]; then
        echo "Testing: $(basename "$input_file")"
    fi

    if ! "$FORTFRONT" "$input_file" > "$actual_file" 2>&1; then
        if [[ $UPDATE_MODE -eq 1 ]]; then
            cp "$actual_file" "$expected_file"
            echo "UPDATED: $(basename "$input_file")"
        else
            echo "FAIL: $(basename "$input_file") - fortfront exited with error"
            if [[ $VERBOSE -eq 1 ]]; then
                cat "$actual_file"
            fi
            FAILED=$((FAILED + 1))
            return 0
        fi
    fi

    if [[ $UPDATE_MODE -eq 1 ]]; then
        cp "$actual_file" "$expected_file"
        rm -f "$actual_file"
        echo "UPDATED: $(basename "$input_file")"
    else
        if [[ ! -f "$expected_file" ]]; then
            echo "FAIL: $(basename "$input_file") - no expected file found"
            echo "      Run with --update to generate expected output"
            FAILED=$((FAILED + 1))
            return 0
        fi

        if diff -u "$expected_file" "$actual_file" > /dev/null 2>&1; then
            if [[ $VERBOSE -eq 1 ]]; then
                echo "PASS: $(basename "$input_file")"
            fi
            PASSED=$((PASSED + 1))
            rm -f "$actual_file"
        else
            echo "FAIL: $(basename "$input_file") - output differs from expected"
            if [[ $VERBOSE -eq 1 ]]; then
                echo "--- Expected"
                echo "+++ Actual"
                diff -u "$expected_file" "$actual_file" || true
            fi
            FAILED=$((FAILED + 1))
            return 0
        fi
    fi
}

cd "$CASES_DIR"

input_files=($(find . -maxdepth 1 \( -name "*.lf" -o -name "*.f90" \) | sort))

if [[ ${#input_files[@]} -eq 0 ]]; then
    echo "No snapshot test files found in $CASES_DIR"
    exit 0
fi

if [[ $UPDATE_MODE -eq 1 ]]; then
    echo "=== UPDATE MODE: Regenerating all expected outputs ==="
    echo ""
fi

for input_file in "${input_files[@]}"; do
    run_snapshot_test "$input_file"
done

if [[ $UPDATE_MODE -eq 1 ]]; then
    echo ""
    echo "Updated ${#input_files[@]} snapshot(s)"
    echo "Review changes with: git diff test/snapshots/cases/"
else
    echo ""
    echo "=== Snapshot Test Results ==="
    echo "PASSED: $PASSED"
    echo "FAILED: $FAILED"
    echo "TOTAL:  ${#input_files[@]}"

    if [[ $FAILED -gt 0 ]]; then
        exit 1
    fi
fi
