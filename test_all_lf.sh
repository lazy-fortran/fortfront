#!/bin/bash

# Script to test fortfront on all .lf files in the repository
# Records timing, output, and errors for each file

RESULTS_DIR="test_results"
SUMMARY_FILE="${RESULTS_DIR}/summary.txt"
ERROR_FILE="${RESULTS_DIR}/errors.txt"

mkdir -p "$RESULTS_DIR"
echo "Testing fortfront on all .lf files - $(date)" > "$SUMMARY_FILE"
echo "Errors found during testing - $(date)" > "$ERROR_FILE"

total_files=0
success_count=0
error_count=0
timeout_count=0

# Find all .lf files
while read -r file; do
    total_files=$((total_files + 1))
    echo "Processing: $file"
    
    basename=$(basename "$file" .lf)
    output_file="${RESULTS_DIR}/${basename}.f90"
    timing_file="${RESULTS_DIR}/${basename}.time"
    error_log="${RESULTS_DIR}/${basename}.err"
    
    # Time the execution and capture output/errors
    start_time=$(date +%s.%N)
    timeout 30s fpm run fortfront -- < "$file" > "$output_file" 2> "$error_log"
    exit_code=$?
    end_time=$(date +%s.%N)
    
    # Calculate execution time
    exec_time=$(echo "$end_time - $start_time" | bc -l)
    echo "$exec_time" > "$timing_file"
    
    # Analyze results
    if [ $exit_code -eq 124 ]; then
        # Timeout
        echo "TIMEOUT: $file (>30s)" >> "$SUMMARY_FILE"
        echo "TIMEOUT: $file" >> "$ERROR_FILE"
        timeout_count=$((timeout_count + 1))
    elif [ $exit_code -ne 0 ]; then
        # Error exit
        echo "ERROR: $file (exit code: $exit_code, time: ${exec_time}s)" >> "$SUMMARY_FILE"
        echo "ERROR in $file:" >> "$ERROR_FILE"
        cat "$error_log" >> "$ERROR_FILE"
        echo "---" >> "$ERROR_FILE"
        error_count=$((error_count + 1))
    else
        # Check for errors in output
        if grep -q "ERROR:" "$output_file" || grep -q "error" "$error_log"; then
            echo "OUTPUT_ERROR: $file (time: ${exec_time}s)" >> "$SUMMARY_FILE"
            echo "OUTPUT ERROR in $file:" >> "$ERROR_FILE"
            grep "ERROR:" "$output_file" >> "$ERROR_FILE" 2>/dev/null
            cat "$error_log" >> "$ERROR_FILE" 2>/dev/null
            echo "---" >> "$ERROR_FILE"
            error_count=$((error_count + 1))
        else
            echo "SUCCESS: $file (time: ${exec_time}s)" >> "$SUMMARY_FILE"
            success_count=$((success_count + 1))
        fi
    fi
    
done < <(find . -name "*.lf" -type f | sort)

# Summary statistics
echo "" >> "$SUMMARY_FILE"
echo "SUMMARY:" >> "$SUMMARY_FILE"
echo "Total files: $total_files" >> "$SUMMARY_FILE"
echo "Successful: $success_count" >> "$SUMMARY_FILE"
echo "Errors: $error_count" >> "$SUMMARY_FILE"
echo "Timeouts: $timeout_count" >> "$SUMMARY_FILE"
echo "Success rate: $(echo "scale=2; $success_count * 100 / $total_files" | bc -l)%" >> "$SUMMARY_FILE"

echo "Testing complete. Results in $RESULTS_DIR/"
cat "$SUMMARY_FILE"