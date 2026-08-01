#!/usr/bin/env bash
# Corpus rejection gate.
#
# Runs the fortfront frontend over a corpus of Fortran sources and records, per
# file, whether the frontend ACCEPTED or REJECTED it. The resulting report can be
# diffed against a baseline report to prove that a rejection change introduces
# zero newly rejected files outside its intended fixtures.
#
# Usage:
#   scripts/corpus_rejection_gate.sh --out logs/gate.tsv [options]
#   scripts/corpus_rejection_gate.sh --out new.tsv --baseline old.tsv \
#       [--allow allowlist.txt]
#
# Options:
#   --out PATH        Report path (TSV: STATUS<TAB>relative-path). Required.
#   --corpus DIR      Add a corpus directory (repeatable). Defaults to the
#                     fortfront examples/ tree plus the gfortran.dg suite when
#                     it can be located.
#   --probe PATH      frontend_probe executable. Default: newest under build/.
#   --jobs N          Parallel probe processes (default 3).
#   --timeout SEC     Per-file timeout (default 20).
#   --baseline PATH   Compare mode: fail when a file ACCEPTED in the baseline is
#                     REJECTED in the new report.
#   --allow PATH      Newline-separated list of corpus paths permitted to become
#                     newly rejected (intended fixtures). Blank lines and lines
#                     starting with '#' are ignored.
#
# Exit status: 0 on success, 1 when the comparison finds newly rejected files,
# 2 on usage or environment errors.
set -uo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
project_root=$(cd "$script_dir/.." && pwd)

out=""
probe=""
baseline=""
allow=""
jobs=${FF_GATE_JOBS:-3}
timeout_s=${FF_GATE_TIMEOUT:-20}
corpora=()

die() {
    echo "ERROR: $*" >&2
    exit 2
}

while (($# > 0)); do
    case "$1" in
        --out) [[ $# -ge 2 ]] || die "--out requires a path"; out=$2; shift 2 ;;
        --corpus) [[ $# -ge 2 ]] || die "--corpus requires a path"; corpora+=("$2"); shift 2 ;;
        --probe) [[ $# -ge 2 ]] || die "--probe requires a path"; probe=$2; shift 2 ;;
        --jobs) [[ $# -ge 2 ]] || die "--jobs requires a number"; jobs=$2; shift 2 ;;
        --timeout) [[ $# -ge 2 ]] || die "--timeout requires a number"; timeout_s=$2; shift 2 ;;
        --baseline) [[ $# -ge 2 ]] || die "--baseline requires a path"; baseline=$2; shift 2 ;;
        --allow) [[ $# -ge 2 ]] || die "--allow requires a path"; allow=$2; shift 2 ;;
        -h|--help) sed -n '2,32p' "${BASH_SOURCE[0]}"; exit 0 ;;
        *) die "unknown argument: $1" ;;
    esac
done

[[ -n "$out" ]] || die "--out is required"

resolve_probe() {
    local newest="" newest_mtime=-1 candidate mtime
    for candidate in "$project_root"/build/fo/app/frontend_probe \
        "$project_root"/build/gfortran_*/app/frontend_probe; do
        [[ -x "$candidate" ]] || continue
        mtime=$(stat -c %Y "$candidate")
        if ((mtime > newest_mtime)); then
            newest=$candidate
            newest_mtime=$mtime
        fi
    done
    printf '%s\n' "$newest"
}

if [[ -z "$probe" ]]; then
    probe=$(resolve_probe)
fi
[[ -n "$probe" && -x "$probe" ]] || die "frontend_probe not found; run 'fo build' or pass --probe"

if ((${#corpora[@]} == 0)); then
    corpora+=("$project_root/examples")
    dg_dir=${FF_GFORTRAN_DG_DIR:-$project_root/../gcc/gcc/testsuite/gfortran.dg}
    if [[ -d "$dg_dir" ]]; then
        corpora+=("$dg_dir")
    fi
fi

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
file_list="$work_dir/files.txt"
: >"$file_list"
for corpus in "${corpora[@]}"; do
    [[ -d "$corpus" ]] || die "corpus directory not found: $corpus"
    find "$corpus" -type f \( -name '*.f90' -o -name '*.F90' -o -name '*.f' \
        -o -name '*.F' -o -name '*.lf' \) -print >>"$file_list"
done
sort -u -o "$file_list" "$file_list"

total=$(wc -l <"$file_list")
((total > 0)) || die "no Fortran sources found in the requested corpora"
echo "corpus rejection gate: $total files, probe $probe" >&2

probe_one() {
    local file=$1
    local rel line status rc err
    rel=${file#"$project_root"/}
    err=$(mktemp)
    line=$(timeout "$FF_GATE_TIMEOUT_S" "$FF_GATE_PROBE" "$file" 2>"$err")
    rc=$?
    # Anything the probe writes to stderr -- a runtime abort, a scanner
    # invariant, an instrumentation trace -- used to be discarded here, which
    # made a whole class of diagnostic invisible to the gate. Keep it.
    if [[ -s "$err" ]]; then
        sed "s|^|$rel\t|" "$err" >>"$FF_GATE_STDERR_LOG"
    fi
    rm -f "$err"
    if [[ $rc -ne 0 || -z "$line" ]]; then
        status=REJECTED
    elif [[ "$line" == *'"parse_ok":true'* && "$line" == *'"semantic_ok":true'* ]]; then
        status=ACCEPTED
    else
        status=REJECTED
    fi
    printf '%s\t%s\n' "$status" "$rel"
}
export -f probe_one
export FF_GATE_PROBE="$probe"
export FF_GATE_TIMEOUT_S="$timeout_s"
export FF_GATE_STDERR_LOG="$out.stderr.log"
export project_root

mkdir -p "$(dirname -- "$out")"
: >"$out.stderr.log"
xargs -a "$file_list" -d '\n' -P "$jobs" -n 1 \
    bash -c 'probe_one "$0"' | sort -k2,2 >"$out"

accepted=$(grep -c '^ACCEPTED' "$out" || true)
rejected=$(grep -c '^REJECTED' "$out" || true)
echo "gate report: $out (accepted=$accepted rejected=$rejected)" >&2
stderr_files=$(cut -f1 "$out.stderr.log" | sort -u | grep -c . || true)
if ((stderr_files > 0)); then
    echo "probe stderr: $stderr_files file(s) wrote to stderr;" \
        "see $out.stderr.log" >&2
else
    rm -f "$out.stderr.log"
fi

[[ -n "$baseline" ]] || exit 0
[[ -f "$baseline" ]] || die "baseline report not found: $baseline"

allow_file="$work_dir/allow.txt"
: >"$allow_file"
if [[ -n "$allow" ]]; then
    [[ -f "$allow" ]] || die "allowlist not found: $allow"
    grep -v -e '^[[:space:]]*$' -e '^[[:space:]]*#' "$allow" | sed 's/[[:space:]]*$//' \
        >"$allow_file"
fi

regressions="$work_dir/regressions.txt"
awk -F '\t' '
    NR == FNR { base[$2] = $1; next }
    { if (base[$2] == "ACCEPTED" && $1 == "REJECTED") print $2 }
' "$baseline" "$out" | grep -vxF -f "$allow_file" >"$regressions" || true
# grep with an empty pattern file drops everything; recover that case.
if [[ ! -s "$allow_file" ]]; then
    awk -F '\t' '
        NR == FNR { base[$2] = $1; next }
        { if (base[$2] == "ACCEPTED" && $1 == "REJECTED") print $2 }
    ' "$baseline" "$out" >"$regressions"
fi

count=$(wc -l <"$regressions")
if ((count > 0)); then
    echo "FAIL: $count file(s) accepted by the baseline are rejected now:" >&2
    cat "$regressions" >&2
    exit 1
fi
echo "OK: no newly rejected corpus files versus $baseline" >&2
exit 0
