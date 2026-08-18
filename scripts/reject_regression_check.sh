#!/usr/bin/env bash
# One-command before/after corpus rejection check (issue #2924).
#
# A rejection change must not newly reject valid Fortran. This script builds
# the frontend_probe for the branch under test and for a baseline ref, runs
# the corpus rejection gate over the same corpus with both probes, and diffs
# the accepted/rejected reports. It fails when a file the baseline accepted is
# rejected by the branch under test, unless that file is in the allowlist of
# intended fixtures.
#
# Usage:
#   scripts/reject_regression_check.sh [<branch>] [options]
#
#   <branch>            Branch under test. Default: the current working tree.
#   --baseline <ref|report>
#                       Baseline git ref (e.g. origin/main) or an existing
#                       report TSV produced by corpus_rejection_gate.sh.
#                       Default: origin/main (falls back to main).
#   --allow PATH        Allowlist of corpus paths permitted to become newly
#                       rejected (intended fixtures). One path per line;
#                       blank lines and lines starting with '#' are ignored.
#   --corpus DIR        Extra corpus directory (repeatable). Defaults to
#                       examples/ plus the gfortran.dg suite when it can be
#                       located (FF_GFORTRAN_DG_DIR or ../gcc/gcc/testsuite/
#                       gfortran.dg relative to the project root).
#   --jobs N            Parallel probe processes (default 3).
#   --keep              Keep temporary worktrees, builds, and reports under
#                       build/reject-regression/<timestamp>/ for inspection.
#   --probe-target PATH Use this frontend_probe for the branch under test
#                       instead of building it.
#   -h, --help          Show this help.
#
# Exit status: 0 when the diff is clean, 1 when files are newly rejected
# outside the allowlist, 2 on usage or environment errors.
set -uo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
project_root=$(cd "$script_dir/.." && pwd)

target_branch=""
baseline_arg="origin/main"
baseline_report=""
allow=""
jobs=${FF_GATE_JOBS:-3}
keep=0
probe_target=""
corpora=()

die() {
    echo "ERROR: $*" >&2
    exit 2
}

usage() {
    sed -n '2,46p' "${BASH_SOURCE[0]}"
}

while (($# > 0)); do
    case "$1" in
        --baseline)
            [[ $# -ge 2 ]] || die "--baseline requires a value"
            baseline_arg=$2
            shift 2
            ;;
        --allow)
            [[ $# -ge 2 ]] || die "--allow requires a path"
            allow=$2
            shift 2
            ;;
        --corpus)
            [[ $# -ge 2 ]] || die "--corpus requires a path"
            corpora+=("$2")
            shift 2
            ;;
        --jobs)
            [[ $# -ge 2 ]] || die "--jobs requires a number"
            jobs=$2
            shift 2
            ;;
        --keep)
            keep=1
            shift
            ;;
        --probe-target)
            [[ $# -ge 2 ]] || die "--probe-target requires a path"
            probe_target=$2
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        -*)
            die "unknown option: $1"
            ;;
        *)
            [[ -z "$target_branch" ]] || die "unexpected extra argument: $1"
            target_branch=$1
            shift
            ;;
    esac
done

# The baseline can be either a git ref (build it) or an existing report file.
baseline_ref=""
if [[ -f "$baseline_arg" ]]; then
    baseline_report=$baseline_arg
else
    baseline_ref=$baseline_arg
fi

# --- Working directories -----------------------------------------------------
base_stamp=$(date +%Y%m%d-%H%M%S)
work_base="$project_root/build/reject-regression/$base_stamp"
tmp="$work_base/tmp"
mkdir -p "$tmp"

# shellcheck disable=SC2329 # invoked indirectly by the EXIT trap below
cleanup() {
    local exit_status=$?
    if [[ -n "${target_wt:-}" ]]; then
        git -C "$project_root" worktree remove --force "$target_wt" 2>/dev/null || true
    fi
    if [[ -n "${baseline_wt:-}" ]]; then
        git -C "$project_root" worktree remove --force "$baseline_wt" 2>/dev/null || true
    fi
    if [[ $keep -eq 0 ]]; then
        rm -rf "$work_base"
    fi
    return "$exit_status"
}
trap cleanup EXIT

# --- Default corpus: examples/ plus gfortran.dg when locatable ---------------
dg_dir=${FF_GFORTRAN_DG_DIR:-$project_root/../gcc/gcc/testsuite/gfortran.dg}
if [[ -z "${FF_GFORTRAN_DG_DIR:-}" && -d "$project_root/../gcc/gcc/testsuite/gfortran.dg" ]]; then
    dg_dir="$project_root/../gcc/gcc/testsuite/gfortran.dg"
fi
if [[ -d "$dg_dir" ]]; then
    corpora+=("$dg_dir")
    echo "corpus: examples/ + $dg_dir" >&2
else
    echo "corpus: examples/ (gfortran.dg not found; set FF_GFORTRAN_DG_DIR)" >&2
fi

build_probe() {
    # build_probe <name> <dir>  ->  echoes the probe binary path
    local name=$1
    local dir=$2
    local log="$work_base/build-$name.log"
    echo "building $name probe in $dir" >&2
    if ! (cd "$dir" && fpm build >"$log" 2>&1); then
        echo "ERROR: failed to build the $name probe (see $log)" >&2
        return 1
    fi
    local probe
    probe=$(find "$dir/build" -type f -path '*/app/frontend_probe' -perm -u+x \
        -print0 2>/dev/null | xargs -0 -r ls -t 2>/dev/null | head -n 1)
    [[ -n "$probe" && -x "$probe" ]] || {
        echo "ERROR: no frontend_probe produced by the $name build (see $log)" >&2
        return 1
    }
    printf '%s\n' "$probe"
}

# --- Under-test probe --------------------------------------------------------
if [[ -z "$probe_target" ]]; then
    current_branch=$(git -C "$project_root" branch --show-current)
    if [[ -n "$target_branch" && "$target_branch" != "$current_branch" ]]; then
        target_wt="$tmp/worktree-target"
        git -C "$project_root" worktree add --detach "$target_wt" "$target_branch" >/dev/null 2>&1 \
            || die "cannot create worktree for target branch '$target_branch'"
        probe_target=$(build_probe target "$target_wt") || exit 2
    else
        # No branch argument, or the branch is the one we are already on:
        # probe the current working tree.
        probe_target=$(build_probe target "$project_root") || exit 2
    fi
fi
[[ -x "$probe_target" ]] || die "--probe-target is not executable: $probe_target"
echo "under-test probe: $probe_target" >&2

# --- Baseline report ---------------------------------------------------------
if [[ -z "$baseline_report" ]]; then
    baseline_wt="$tmp/worktree-baseline"
    git -C "$project_root" worktree add --detach "$baseline_wt" "$baseline_ref" >/dev/null 2>&1 \
        || die "cannot create worktree for baseline ref '$baseline_ref'"
    baseline_probe=$(build_probe baseline "$baseline_wt") || exit 2
    echo "baseline probe: $baseline_probe" >&2
    baseline_report="$tmp/baseline.tsv"
    corpus_args=()
    for c in "${corpora[@]}"; do
        corpus_args+=(--corpus "$c")
    done
    bash "$script_dir/corpus_rejection_gate.sh" \
        --out "$baseline_report" --probe "$baseline_probe" \
        "${corpus_args[@]}" --jobs "$jobs" >&2
    status=$?
    if [[ $status -ne 0 ]]; then
        echo "ERROR: baseline gate run failed (exit $status)" >&2
        exit 2
    fi
else
    echo "using existing baseline report: $baseline_report" >&2
fi
[[ -f "$baseline_report" ]] || die "baseline report missing: $baseline_report"

# --- Under-test report + diff ------------------------------------------------
corpus_args=()
for c in "${corpora[@]}"; do
    corpus_args+=(--corpus "$c")
done

target_report="$tmp/target.tsv"
gate_args=(--out "$target_report" --probe "$probe_target" \
    --baseline "$baseline_report" "${corpus_args[@]}" --jobs "$jobs")
if [[ -n "$allow" ]]; then
    [[ -f "$allow" ]] || die "allowlist not found: $allow"
    gate_args+=(--allow "$allow")
fi

echo "== before/after corpus rejection diff ==" >&2
bash "$script_dir/corpus_rejection_gate.sh" "${gate_args[@]}"
status=$?

echo >&2
if [[ $status -eq 0 ]]; then
    echo "PASS: no corpus file accepted by the baseline is newly rejected." >&2
    echo "Reports kept at: $work_base (re-run with --keep to preserve them)" >&2
else
    echo "FAIL: under-test report $target_report vs baseline $baseline_report" >&2
fi
exit "$status"
