#!/usr/bin/env bash
set -u

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
project_root=$(cd "$script_dir/.." && pwd)
python_bin=${PYTHON:-python3}

suite=all
report="$project_root/logs/frontend_conformance.jsonl"
extra_args=()

usage() {
    cat <<'USAGE'
Usage: scripts/run_frontend_conformance.sh [options]

Options:
  --suite {all,gfortran-dg,lfortran}
  --report PATH
  -h, --help

All other options are passed to scripts/run_gfortran_roundtrip.py.
USAGE
}

while (($# > 0)); do
    case "$1" in
        --suite)
            if (($# < 2)); then
                echo "ERROR: --suite requires a value" >&2
                exit 2
            fi
            suite=$2
            shift 2
            ;;
        --report|--output)
            if (($# < 2)); then
                echo "ERROR: $1 requires a path" >&2
                exit 2
            fi
            report=$2
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            extra_args+=("$1")
            shift
            ;;
    esac
done

case "$suite" in
    all|gfortran-dg|lfortran) ;;
    *)
        echo "ERROR: unknown suite: $suite" >&2
        exit 2
        ;;
esac

report_for_suite() {
    local suite_name=$1
    if [[ "$suite" != all ]]; then
        printf '%s\n' "$report"
        return
    fi

    local report_dir report_base report_stem safe_suite
    report_dir=$(dirname -- "$report")
    report_base=$(basename -- "$report")
    report_stem=${report_base%.jsonl}
    safe_suite=${suite_name//-/_}
    printf '%s/%s_%s.jsonl\n' "$report_dir" "$report_stem" "$safe_suite"
}

run_suite() {
    local suite_name=$1
    local suite_report
    local status

    suite_report=$(report_for_suite "$suite_name")
    mkdir -p "$(dirname -- "$suite_report")"

    echo "== frontend conformance: $suite_name =="
    "$python_bin" "$project_root/scripts/run_gfortran_roundtrip.py" \
        --suite "$suite_name" \
        --report "$suite_report" \
        "${extra_args[@]}"
    status=$?
    if ((status != 0)); then
        echo "suite $suite_name failed with exit $status" >&2
    fi
    return "$status"
}

overall_status=0
if [[ "$suite" == all ]]; then
    for suite_name in gfortran-dg lfortran; do
        run_suite "$suite_name"
        suite_status=$?
        if ((suite_status != 0 && overall_status == 0)); then
            overall_status=$suite_status
        fi
    done
else
    run_suite "$suite"
    overall_status=$?
fi

exit "$overall_status"
