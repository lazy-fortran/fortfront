# Scripts

## Purpose

This directory contains utility scripts for building, testing, and validating fortfront. Scripts automate common development tasks, CI operations, and quality checks.

## Script Index

| Script | Description |
|--------|-------------|
| build.sh | Simple build script wrapper for fpm |
| check_test_duplication.py | Python script to detect inline code violations in tests |
| check_test_duplication.sh | Shell wrapper for duplication checker |
| run_frontend_conformance.sh | Frontend conformance wrapper for external suites |
| run_gfortran_roundtrip.py | Frontend conformance and round-trip validation |
| with_timeout.sh | Run command with timeout (Linux/macOS) |
| with_timeout.ps1 | Run command with timeout (Windows PowerShell) |

## Script Details

### build.sh
Simple wrapper for fpm build commands. Ensures consistent build invocations.

```bash
./scripts/build.sh
```

### check_test_duplication.py
Detects violations of the zero-duplication policy: end-to-end tests with inline full programs.

**Usage**:
```bash
python scripts/check_test_duplication.py
```

**Output**:
- **Violations**: Tests with >15 lines of inline code (full programs)
- **Warnings**: Tests with 6-15 lines (integration tests to review)
- **OK**: Tests with <6 lines (unit tests with inline code are fine)

**CI Integration**: Used by `make check-duplication` and CI workflow.

### check_test_duplication.sh
Shell wrapper for the Python duplication checker. Provides Unix-friendly interface.

```bash
./scripts/check_test_duplication.sh
```

### run_frontend_conformance.sh
Thin wrapper around `run_gfortran_roundtrip.py`. It runs `gfortran-dg`,
`lfortran`, or both, and leaves the detailed failure digest and construct
histogram to the Python runner.

```bash
scripts/run_frontend_conformance.sh --suite all --report /tmp/ff_frontend.jsonl
scripts/run_frontend_conformance.sh --suite gfortran-dg --max-tests 50
scripts/run_frontend_conformance.sh --suite lfortran --jobs 1
```

Suite roots:
- `FF_GFORTRAN_DG_DIR` points to the `gfortran.dg` directory. The wrapper
  derives the GCC root from it.
- `--gcc-root` overrides the derived GCC root.
- `FF_LFORTRAN_DIR`, or `--lfortran-root`, points to a lfortran source root.
- `--lfortran-root` overrides `FF_LFORTRAN_DIR`.

If a suite is absent, the runner prints `SKIP` and exits 0.

### run_gfortran_roundtrip.py
Validates parser correctness via frontend probing and round-trip testing.
For GCC DejaGNU files it also classifies expected failures using dg directives,
without invoking DejaGNU. The script writes per-file JSONL plus a summary JSON
with pass counts, failure digests, and construct histograms.

Round-trip steps:
1. Probe `compile_frontend_from_file` with semantics enabled.
2. Record parse and semantic-analysis state.
3. Parse standard Fortran to AST.
4. Emit AST to standardized Fortran.
5. Parse standardized output again.
6. Verify the second output matches the first.

Skipped tests:
- Missing external suites are reported as `SKIP`.
- No GCC or lfortran source is copied into this repository.

Expected failures:
- Inputs containing `dg-shouldfail` or `dg-xfail` comments are marked XFAIL; if such a test passes round-trip it is reported as XPASS.

Heuristics:
- Lightweight bucketization + keyword extraction of stderr/diff patterns (interface/data, unnamed wrappers, implicit handling, bind(c), OpenMP/ACC, coarrays) to surface the most common failure modes for humans and LLMs.
- Source-aware hints: failing records now attach keywords/pattern hits extracted from the original test case (e.g., bind(c), coarray, OpenMP, POINTER/ALLOCATABLE, NAMELIST). Digests and heatmaps include these for quick triage.
- Semantic leniency: if byte output differs, compile+run both reference and round-trip; when both compile/run and outputs match, the case is reported as “equivalent_not_identical” instead of a failure. Outputs/compile/runtime mismatches are split into compare/compile/runtime buckets.
- Minimal dg handling: honors `dg-options`/`dg-additional-options`, `dg-additional-source`/`dg-additional-files`, and `dg-do compile|run` to give reference/round-trip compiles the same flags and extra sources (without using the GCC DejaGnu harness).

**Usage**:
```bash
python scripts/run_gfortran_roundtrip.py --suite gfortran-dg --gcc-root ../gcc

# Quick iteration on first 25 tests:
python scripts/run_gfortran_roundtrip.py --suite gfortran-dg --max-tests 25

# Stream live top failure categories (default every 5s):
python scripts/run_gfortran_roundtrip.py --suite lfortran --lfortran-root ../lfortran
```

Use `--report` to choose the JSONL path and `--frontend-probe` to pin the
helper executable when the build tree has multiple candidates.

**Key options**:
- `--max-tests`: limit number of tests for fast debugging.
- `--suite`: select `gfortran-dg` or `lfortran`.
- `--report`: path for the JSONL report.
- `--live-digest-interval`: seconds between live top-category digests (0 to disable). Default: 5s.
- `--live-digest-limit`: number of categories/signatures shown per live digest. Default: 3.
- `--compile-timeout`: per-file compile timeout used during semantic checking (default 0.5s).
- `--run-timeout`: per-binary run timeout used during semantic checking (default 0.5s).

Defaults tuned for speed/visibility:
- 32 worker threads (or CPU count if lower)
- 50 ms per-test timeout
- Live digest every 5 seconds

**Purpose**:
- Validate parser and semantic coverage over external corpora.
- Ensure codegen produces round-trippable Fortran for accepted files.
- Surface construct buckets that block Fortran 2023 coverage.
- Report GCC expected failures and XPASS cases from dg directives.

### with_timeout.sh (Linux/macOS)
Run command with timeout using `timeout` utility or `perl`.

**Usage**:
```bash
./scripts/with_timeout.sh 30 command arg1 arg2
# Timeout after 30 seconds
```

**Used by**:
- Test suite (prevent hanging tests)
- CI (enforce time limits)

### with_timeout.ps1 (Windows)
Run command with timeout using PowerShell job control.

**Usage**:
```powershell
pwsh scripts/with_timeout.ps1 -Seconds 30 -Command "command arg1 arg2"
```

**Used by**:
- Windows CI
- Local Windows development

## CI Integration

Scripts used by `.github/workflows/ci.yml`:
- **build.sh** - Build project
- **check_test_duplication.sh** - Enforce zero-duplication policy
- **with_timeout.sh / .ps1** - Prevent hanging CI jobs
- **run_frontend_conformance.sh / run_gfortran_roundtrip.py** - Validate frontend conformance when external suites are present

## Development Workflow

**Check duplication before commit**:
```bash
make check-duplication
```

**Validate frontend conformance**:
```bash
scripts/run_frontend_conformance.sh --suite all --report /tmp/ff_frontend.jsonl
```

**Build with timeout**:
```bash
./scripts/with_timeout.sh 300 make
```

## Dependencies

**Python Scripts**:
- Python 3.8+
- `rapidfuzz` (used by `run_gfortran_roundtrip.py` for diff clustering)

**Shell Scripts**:
- Bash (Linux/macOS)
- PowerShell (Windows)
- `timeout` or `perl` (for with_timeout.sh)

**Build Scripts**:
- `fpm` - Fortran Package Manager
- `make` - Build automation
