#!/bin/bash
# Usage: FORTFRONT=/path/to/fortfront ./playtest_roundtrip.sh files...

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
    "$FORTFRONT" "$f" > "${f}.out"
    if [ $? -ne 0 ]; then
        echo "FAIL: fortfront failed on $f"
        continue
    fi

    # Check if output compiles
    # Use -fsyntax-only to just check validity without linking
    $COMPILER -fsyntax-only "${f}.out"
    if [ $? -ne 0 ]; then
        echo "FAIL: Output of $f failed to compile"
    else
        echo "PASS: Output of $f compiles"
    fi

    # Check roundtrip equality (ignoring whitespace)
    diff -wB "$f" "${f}.out" > /dev/null
    if [ $? -eq 0 ]; then
        echo "PASS: Roundtrip identical (ignoring whitespace)"
    else
        echo "WARN: Roundtrip differences found for $f"
    fi
    rm -f "${f}.out"
done
