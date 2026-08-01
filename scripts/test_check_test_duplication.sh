#!/usr/bin/env bash
# Negative control for the zero-duplication gate (issue #2910).
#
# The gate was suppressed in CI for weeks and nobody noticed, because a gate
# that cannot fail is indistinguishable from no gate. This self-test proves
# both directions of the gate on every run:
#   1. the current tree passes,
#   2. a deliberately added inline end-to-end fixture makes it fail.
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROBE="${ROOT}/test/api/zz_duplication_gate_probe.f90"

cleanup() {
    rm -f "${PROBE}"
}
trap cleanup EXIT

cleanup

python3 "${ROOT}/scripts/check_test_duplication.py" >/dev/null
clean_status=$?
if [ "${clean_status}" -ne 0 ]; then
    echo "FAIL: duplication gate reports violations on the current tree" >&2
    python3 "${ROOT}/scripts/check_test_duplication.py" >&2
    exit 1
fi
echo "PASS: clean tree accepted by the duplication gate"

{
    echo "program zz_duplication_gate_probe"
    echo "    implicit none"
    echo "    character(len=:), allocatable :: source"
    echo ""
    echo "    source = 'program probe'//new_line('a')// &"
    for i in $(seq 1 20); do
        echo "        '    integer :: v${i}'//new_line('a')// &"
    done
    echo "        'end program probe'"
    echo "    print *, len(source)"
    echo "end program zz_duplication_gate_probe"
} > "${PROBE}"

python3 "${ROOT}/scripts/check_test_duplication.py" >/dev/null
probe_status=$?
cleanup

if [ "${probe_status}" -eq 0 ]; then
    echo "FAIL: duplication gate accepted an inline end-to-end fixture" >&2
    exit 1
fi
echo "PASS: inline end-to-end fixture rejected by the duplication gate"
