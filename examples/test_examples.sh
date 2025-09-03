#!/bin/bash

# Test all examples in the examples directory
# This is a lightweight version of the comprehensive test

echo "Testing fortfront examples..."
echo "Date: $(date)"
echo

success_count=0
error_count=0
total_count=0

for file in examples/*.lf; do
    if [ -f "$file" ]; then
        total_count=$((total_count + 1))
        basename=$(basename "$file" .lf)
        
        echo -n "Testing $basename... "
        
        # Run fortfront with timeout
        if timeout 10s fpm run fortfront -- < "$file" > "/tmp/${basename}.f90" 2>/dev/null; then
            # Check for errors in output
            if grep -q "ERROR:" "/tmp/${basename}.f90"; then
                echo "FAILED (contains errors)"
                error_count=$((error_count + 1))
            else
                echo "OK"
                success_count=$((success_count + 1))
            fi
        else
            echo "FAILED (timeout or error)"
            error_count=$((error_count + 1))
        fi
    fi
done

echo
echo "Results:"
echo "  Total examples: $total_count"
echo "  Successful: $success_count"  
echo "  Failed: $error_count"
echo "  Success rate: $(echo "scale=1; $success_count * 100 / $total_count" | bc -l)%"

if [ $error_count -gt 0 ]; then
    echo
    echo "⚠️  Some examples failed. This indicates ongoing issues with fortfront."
    echo "   See GitHub issues #1229 and #1230 for details."
fi