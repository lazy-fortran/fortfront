#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/corpus" "$work/bin"
printf 'program p\nend program\n' > "$work/corpus/source with spaces.f90"
cat > "$work/probe" <<'PROBE'
#!/usr/bin/env bash
printf '{"parse_ok":true,"semantic_ok":true}\n'
PROBE
chmod +x "$work/probe"
"$root/scripts/corpus_rejection_gate.sh" --corpus "$work/corpus" \
    --probe "$work/probe" --out "$work/baseline.tsv"
[[ $(wc -l < "$work/baseline.tsv") -eq 1 ]]
[[ $(cut -f1 "$work/baseline.tsv") == ACCEPTED ]]
cat > "$work/probe" <<'PROBE'
#!/usr/bin/env bash
exit 1
PROBE
if "$root/scripts/corpus_rejection_gate.sh" --corpus "$work/corpus" \
    --probe "$work/probe" --out "$work/rejected.tsv" \
    --baseline "$work/baseline.tsv"; then
    echo 'FAIL: newly rejected source was accepted by the gate' >&2
    exit 1
fi
cat > "$work/bin/xargs" <<'XARGS'
#!/usr/bin/env bash
exit 9
XARGS
chmod +x "$work/bin/xargs"
if PATH="$work/bin:$PATH" "$root/scripts/corpus_rejection_gate.sh" \
    --corpus "$work/corpus" --probe "$work/probe" --out "$work/failed.tsv"; then
    echo 'FAIL: failed probe executor produced a passing gate' >&2
    exit 1
fi
printf 'PASS: corpus gate preserves spaced paths and rejects incomplete execution\n'
