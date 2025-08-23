#!/bin/bash
# Arena Performance Benchmark Execution Script - Issue #400
# Runs comprehensive arena performance benchmarks and generates reports
# For regression detection and performance baseline establishment

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BENCHMARK_OUTPUT_DIR="$PROJECT_ROOT/benchmark_results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="$BENCHMARK_OUTPUT_DIR/arena_benchmark_report_$TIMESTAMP.txt"
BASELINE_FILE="$BENCHMARK_OUTPUT_DIR/arena_benchmark_baseline.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Arena Performance Benchmark Suite - Issue #400

OPTIONS:
    -h, --help              Show this help message
    -o, --output DIR        Set output directory (default: $BENCHMARK_OUTPUT_DIR)
    -b, --baseline          Set this run as new performance baseline
    -c, --compare           Compare with existing baseline (if available)
    -v, --verbose           Enable verbose output
    --ci                    CI mode - fail on performance regressions

EXAMPLES:
    $0                      Run benchmarks with default settings
    $0 --baseline          Set new performance baseline
    $0 --compare           Compare with baseline and check for regressions
    $0 --ci --compare      CI mode with regression detection
    
EOF
}

parse_arguments() {
    VERBOSE=false
    SET_BASELINE=false
    COMPARE_BASELINE=false
    CI_MODE=false
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -o|--output)
                BENCHMARK_OUTPUT_DIR="$2"
                shift 2
                ;;
            -b|--baseline)
                SET_BASELINE=true
                shift
                ;;
            -c|--compare)
                COMPARE_BASELINE=true
                shift
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            --ci)
                CI_MODE=true
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
}

setup_environment() {
    log_info "Setting up benchmark environment..."
    
    # Create output directory
    mkdir -p "$BENCHMARK_OUTPUT_DIR"
    
    # Change to project root
    cd "$PROJECT_ROOT"
    
    # Verify we can build the project
    if [[ ! -f "fpm.toml" ]]; then
        log_error "Not in fortfront project root (no fpm.toml found)"
        exit 1
    fi
    
    log_info "Project root: $PROJECT_ROOT"
    log_info "Output directory: $BENCHMARK_OUTPUT_DIR"
    log_info "Report file: $REPORT_FILE"
}

build_benchmarks() {
    log_info "Building arena benchmarks..."
    
    # Use project build system  
    if [[ -f "build.sh" ]]; then
        if $VERBOSE; then
            ./build.sh
        else
            ./build.sh > /dev/null 2>&1
        fi
    else
        # Fallback to fpm build
        if $VERBOSE; then
            fpm build --flag "-cpp -fmax-stack-var-size=65536"
        else
            fpm build --flag "-cpp -fmax-stack-var-size=65536" > /dev/null 2>&1
        fi
    fi
    
    if [[ $? -ne 0 ]]; then
        log_error "Failed to build benchmarks"
        exit 1
    fi
    
    log_success "Benchmarks built successfully"
}

run_benchmarks() {
    log_info "Running arena performance benchmarks..."
    
    # Run the benchmark test
    local benchmark_output
    local benchmark_exit_code
    
    if $VERBOSE; then
        fpm test test_arena_benchmarks --flag "-cpp -fmax-stack-var-size=65536" | tee "$REPORT_FILE"
        benchmark_exit_code=${PIPESTATUS[0]}
    else
        fpm test test_arena_benchmarks --flag "-cpp -fmax-stack-var-size=65536" > "$REPORT_FILE" 2>&1
        benchmark_exit_code=$?
    fi
    
    if [[ $benchmark_exit_code -ne 0 ]]; then
        log_error "Benchmark execution failed"
        if [[ ! $VERBOSE ]]; then
            log_info "Benchmark output:"
            cat "$REPORT_FILE"
        fi
        exit 1
    fi
    
    log_success "Benchmarks completed successfully"
    log_info "Results saved to: $REPORT_FILE"
}

analyze_results() {
    log_info "Analyzing benchmark results..."
    
    # Extract key metrics from report
    local total_benchmarks
    local successful_benchmarks  
    local failed_benchmarks
    local avg_ops_per_sec
    local total_time_ms
    
    if [[ -f "$REPORT_FILE" ]]; then
        total_benchmarks=$(grep "Total benchmarks:" "$REPORT_FILE" | head -1 | awk '{print $3}' || echo "0")
        successful_benchmarks=$(grep "Successful:" "$REPORT_FILE" | head -1 | awk '{print $2}' || echo "0")
        failed_benchmarks=$(grep "Failed:" "$REPORT_FILE" | head -1 | awk '{print $2}' || echo "0")
        avg_ops_per_sec=$(grep "Average ops/sec:" "$REPORT_FILE" | head -1 | awk '{print $3}' || echo "0.0")
        total_time_ms=$(grep "Total time:" "$REPORT_FILE" | head -1 | awk '{print $3}' || echo "0.0")
        
        log_info "Benchmark Results Summary:"
        log_info "  Total benchmarks: $total_benchmarks"
        log_info "  Successful: $successful_benchmarks"
        log_info "  Failed: $failed_benchmarks"
        log_info "  Average performance: $avg_ops_per_sec ops/sec"
        log_info "  Total time: $total_time_ms ms"
        
        if [[ "$failed_benchmarks" != "0" ]]; then
            log_warning "$failed_benchmarks benchmarks failed"
            if $CI_MODE; then
                log_error "Benchmark failures in CI mode - failing build"
                exit 1
            fi
        fi
    else
        log_error "No benchmark report found at $REPORT_FILE"
        exit 1
    fi
}

set_baseline() {
    if $SET_BASELINE; then
        log_info "Setting new performance baseline..."
        
        if [[ -f "$REPORT_FILE" ]]; then
            cp "$REPORT_FILE" "$BASELINE_FILE"
            log_success "New baseline set: $BASELINE_FILE"
        else
            log_error "Cannot set baseline - no report file found"
            exit 1
        fi
    fi
}

compare_with_baseline() {
    if $COMPARE_BASELINE && [[ -f "$BASELINE_FILE" ]]; then
        log_info "Comparing with performance baseline..."
        
        # Extract performance metrics from both reports
        local current_ops_per_sec
        local baseline_ops_per_sec
        local performance_ratio
        local regression_threshold=0.90  # 10% performance drop is regression
        
        current_ops_per_sec=$(grep "Average ops/sec:" "$REPORT_FILE" | head -1 | awk '{print $3}' || echo "0.0")
        baseline_ops_per_sec=$(grep "Average ops/sec:" "$BASELINE_FILE" | head -1 | awk '{print $3}' || echo "0.0")
        
        if [[ "$current_ops_per_sec" != "0.0" && "$baseline_ops_per_sec" != "0.0" ]]; then
            performance_ratio=$(echo "scale=3; $current_ops_per_sec / $baseline_ops_per_sec" | bc -l)
            
            log_info "Performance Comparison:"
            log_info "  Baseline:   $baseline_ops_per_sec ops/sec"  
            log_info "  Current:    $current_ops_per_sec ops/sec"
            log_info "  Ratio:      $performance_ratio"
            
            # Check for performance regression
            if (( $(echo "$performance_ratio < $regression_threshold" | bc -l) )); then
                log_error "Performance regression detected!"
                log_error "Current performance is $(echo "scale=1; (1 - $performance_ratio) * 100" | bc -l)% slower than baseline"
                
                if $CI_MODE; then
                    log_error "Performance regression in CI mode - failing build"
                    exit 1
                fi
            else
                local improvement=$(echo "scale=1; ($performance_ratio - 1) * 100" | bc -l)
                if (( $(echo "$improvement > 0" | bc -l) )); then
                    log_success "Performance improved by $improvement%!"
                else
                    log_success "Performance maintained within acceptable range"
                fi
            fi
        else
            log_warning "Could not extract performance metrics for comparison"
        fi
    elif $COMPARE_BASELINE; then
        log_warning "No baseline found at $BASELINE_FILE - cannot compare"
        log_info "Run with --baseline flag to set initial baseline"
    fi
}

generate_summary_report() {
    log_info "Generating summary report..."
    
    local summary_file="$BENCHMARK_OUTPUT_DIR/latest_summary.txt"
    
    cat > "$summary_file" << EOF
Arena Performance Benchmark Summary
===================================
Timestamp: $(date)
Report: $(basename "$REPORT_FILE")

$(analyze_results)

Full report: $REPORT_FILE
Baseline: $(if [[ -f "$BASELINE_FILE" ]]; then echo "$BASELINE_FILE"; else echo "Not set"; fi)

EOF

    log_success "Summary report: $summary_file"
}

main() {
    parse_arguments "$@"
    
    log_info "Arena Performance Benchmark Suite - Issue #400"
    log_info "================================================"
    
    setup_environment
    build_benchmarks
    run_benchmarks
    analyze_results
    set_baseline
    compare_with_baseline
    generate_summary_report
    
    log_success "Benchmark suite completed successfully!"
}

# Run main function with all arguments
main "$@"