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
    local expected_file="$2"
    local base_name="$3"
    local actual_file="$CASES_DIR/${base_name}.actual"
    local display_name

    display_name="$(basename "$input_file")"

    if [[ $VERBOSE -eq 1 ]]; then
        echo "Testing: $display_name"
    fi

    if ! "$FORTFRONT" "$input_file" > "$actual_file" 2>&1; then
        if [[ $UPDATE_MODE -eq 1 ]]; then
            cp "$actual_file" "$expected_file"
            echo "UPDATED: $display_name"
        else
            echo "FAIL: $display_name - fortfront exited with error"
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
        echo "UPDATED: $display_name"
    else
        if [[ ! -f "$expected_file" ]]; then
            echo "FAIL: $display_name - no expected file found"
            echo "      Expected snapshot: $expected_file"
            FAILED=$((FAILED + 1))
            return 0
        fi

        if diff -u "$expected_file" "$actual_file" > /dev/null 2>&1; then
            if [[ $VERBOSE -eq 1 ]]; then
                echo "PASS: $display_name"
            fi
            PASSED=$((PASSED + 1))
            rm -f "$actual_file"
        else
            echo "FAIL: $display_name - output differs from expected"
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

mapfile -t expected_files < <(find "$CASES_DIR" -maxdepth 1 -name "*.expected" -print | sort)

if [[ ${#expected_files[@]} -eq 0 ]]; then
    echo "No snapshot expected files found in $CASES_DIR"
    exit 0
fi

if [[ $UPDATE_MODE -eq 1 ]]; then
    echo "=== UPDATE MODE: Regenerating all expected outputs ==="
    echo ""
fi

for expected_file in "${expected_files[@]}"; do
    base_name="$(basename "${expected_file%.expected}")"
    case_input="$CASES_DIR/${base_name}"
    input_file=""

    if [[ -f "${case_input}.lf" ]]; then
        input_file="${case_input}.lf"
    elif [[ -f "${case_input}.f90" ]]; then
        input_file="${case_input}.f90"
    elif [[ -f "$PROJECT_ROOT/examples/lf/${base_name}.lf" ]]; then
        input_file="$PROJECT_ROOT/examples/lf/${base_name}.lf"
    elif [[ -f "$PROJECT_ROOT/examples/f90/${base_name}.f90" ]]; then
        input_file="$PROJECT_ROOT/examples/f90/${base_name}.f90"
    else
        echo "FAIL: ${base_name} - no matching input example found"
        echo "      Checked:"
        echo "        $CASES_DIR/${base_name}.lf"
        echo "        $CASES_DIR/${base_name}.f90"
        echo "        $PROJECT_ROOT/examples/lf/${base_name}.lf"
        echo "        $PROJECT_ROOT/examples/f90/${base_name}.f90"
        FAILED=$((FAILED + 1))
        continue
    fi

    run_snapshot_test "$input_file" "$expected_file" "$base_name"
done

if [[ $UPDATE_MODE -eq 1 ]]; then
    echo ""
    echo "Updated ${#expected_files[@]} snapshot(s)"
    echo "Review changes with: git diff test/snapshots/cases/"
else
    echo ""
    echo "=== Snapshot Test Results ==="
    echo "PASSED: $PASSED"
    echo "FAILED: $FAILED"
    echo "TOTAL:  ${#expected_files[@]}"

    if [[ $FAILED -gt 0 ]]; then
        exit 1
    fi
fi
