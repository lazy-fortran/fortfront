# Scripts

## Purpose

This directory contains utility scripts for building, testing, and validating fortfront. Scripts automate common development tasks, CI operations, and quality checks.

## Script Index

| Script | Description |
|--------|-------------|
| build.sh | Simple build script wrapper for fpm |
| check_test_duplication.py | Python script to detect inline code violations in tests |
| check_test_duplication.sh | Shell wrapper for duplication checker |
| run_gfortran_roundtrip.py | Round-trip validation: parse → emit → parse → emit (should match) |
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

### run_gfortran_roundtrip.py
Validates parser correctness via round-trip testing and classifies expected failures using dg directives (no DejaGnu invocation). Adds a semantic check path: if byte-for-byte output differs, compile and run both the reference and round-tripped sources and treat them as equivalent when binaries run and outputs match.

Round-trip steps:
1. Pre-check: compile reference source with gfortran (skip test if compilation fails)
2. Parse standard Fortran → AST
3. Emit AST → standardized Fortran
4. Parse standardized → AST2
5. Emit AST2 → output2
6. Verify output1 == output2 (modulo whitespace)

Skipped tests:
- Tests where the reference source does not compile with gfortran are skipped automatically
- These are reported separately as "SKIP" and not counted as failures
- This ensures the test suite only reports genuine fortfront issues

Expected failures:
- Inputs containing `dg-shouldfail` or `dg-xfail` comments are marked XFAIL; if such a test passes round-trip it is reported as XPASS.

Heuristics:
- Lightweight bucketization + keyword extraction of stderr/diff patterns (interface/data, unnamed wrappers, implicit handling, bind(c), OpenMP/ACC, coarrays) to surface the most common failure modes for humans and LLMs.
- Source-aware hints: failing records now attach keywords/pattern hits extracted from the original test case (e.g., bind(c), coarray, OpenMP, POINTER/ALLOCATABLE, NAMELIST). Digests and heatmaps include these for quick triage.
- Semantic leniency: if byte output differs, compile+run both reference and round-trip; when both compile/run and outputs match, the case is reported as “equivalent_not_identical” instead of a failure. Outputs/compile/runtime mismatches are split into compare/compile/runtime buckets.
- Minimal dg handling: honors `dg-options`/`dg-additional-options`, `dg-additional-source`/`dg-additional-files`, and `dg-do compile|run` to give reference/round-trip compiles the same flags and extra sources (without using the GCC DejaGnu harness).

**Usage**:
```bash
python scripts/run_gfortran_roundtrip.py --gcc-root ../gcc-dev/gcc

# Quick iteration on first 25 tests:
python scripts/run_gfortran_roundtrip.py --gcc-root ../gcc-dev/gcc --max-tests 25

# Stream live top failure categories (default every 5s):
python scripts/run_gfortran_roundtrip.py --gcc-root ../gcc-dev/gcc
```

**Key options**:
- `--max-tests`: limit number of tests for fast debugging.
- `--live-digest-interval`: seconds between live top-category digests (0 to disable). Default: 5s.
- `--live-digest-limit`: number of categories/signatures shown per live digest. Default: 3.
- `--compile-timeout`: per-file compile timeout used during semantic checking (default 0.5s).
- `--run-timeout`: per-binary run timeout used during semantic checking (default 0.5s).

Defaults tuned for speed/visibility:
- 32 worker threads (or CPU count if lower)
- 50 ms per-test timeout
- Live digest every 5 seconds

**Purpose**:
- Validate parser correctness
- Ensure codegen produces valid Fortran
- Detect parse/emit asymmetries
- Surface whether failures align with GCC’s expected XFAIL/XPASS classifications via dg directives

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
- **run_gfortran_roundtrip.py** - Validate round-trip correctness

## Development Workflow

**Check duplication before commit**:
```bash
make check-duplication
```

**Validate round-trip**:
```bash
python scripts/run_gfortran_roundtrip.py examples/f90/*.f90
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
