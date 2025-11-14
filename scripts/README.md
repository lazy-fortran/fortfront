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
Validates parser correctness via round-trip testing:
1. Parse standard Fortran → AST
2. Emit AST → standardized Fortran
3. Parse standardized → AST2
4. Emit AST2 → output2
5. Verify output1 == output2 (modulo whitespace)

**Usage**:
```bash
python scripts/run_gfortran_roundtrip.py examples/f90/*.f90
```

**Purpose**:
- Validate parser correctness
- Ensure codegen produces valid Fortran
- Detect parse/emit asymmetries

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
- Python 3.6+
- No external dependencies (standard library only)

**Shell Scripts**:
- Bash (Linux/macOS)
- PowerShell (Windows)
- `timeout` or `perl` (for with_timeout.sh)

**Build Scripts**:
- `fpm` - Fortran Package Manager
- `make` - Build automation
