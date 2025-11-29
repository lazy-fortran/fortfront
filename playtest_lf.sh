#!/bin/bash
# Usage: FORTFRONT=/path/to/fortfront ./playtest_lf.sh files...

FORTFRONT=${FORTFRONT:-fortfront}
COMPILER=gfortran
FILES="$@"

if ! command -v "$FORTFRONT" &> /dev/null && [ ! -x "$FORTFRONT" ]; then
    echo "Error: fortfront executable not found at '$FORTFRONT'"
    echo "Usage: FORTFRONT=/path/to/fortfront $0 <files>"
    exit 1
fi

if [ -z "$FILES" ]; then
    echo "Usage: $0 <files>"
    exit 1
fi

for f in $FILES; do
    echo "Testing $f..."
    "$FORTFRONT" "$f" > "${f}.f90"
    if [ $? -ne 0 ]; then
        echo "FAIL: fortfront failed on $f"
        continue
    fi

    $COMPILER "${f}.f90" -o "${f}.exe"
    if [ $? -ne 0 ]; then
        echo "FAIL: Compilation failed for $f"
    else
        echo "PASS: $f compiled successfully"
        rm -f "${f}.exe"
    fi
    rm -f "${f}.f90"
done
