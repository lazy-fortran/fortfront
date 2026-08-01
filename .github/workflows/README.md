# GitHub Workflows

## Purpose

This directory contains GitHub Actions CI/CD workflow definitions for automated testing, validation, and quality checks. Workflows run on every push and pull request to ensure code quality and prevent regressions.

## Workflow Index

| Workflow | Description |
|----------|-------------|
| ci.yml | Main CI workflow: build, test, lint, duplication check |

## CI Workflow (ci.yml)

### Triggers
- **Push**: All branches
- **Pull Request**: All pull requests to any branch

### Jobs

**1. Build**
- Platform: Linux (Ubuntu latest)
- Compiler: gfortran
- Build tool: fpm via make
- Commands: `make build`

**2. Test**
- Platform: Linux and Windows
- Test suite: All fpm tests
- Timeout: 120 seconds per test
- Commands: `make test`
- Parallel execution: fpm manages parallelism

**3. Duplication Check**
- Validates zero-duplication policy
- Detects end-to-end tests with inline full programs
- Status: BLOCKING (violations fail CI; issue #2910)
- Commands: `make check-duplication`, `make check-duplication-gate`
- Script: `scripts/check_test_duplication.py`

**4. Round-Trip Validation**
- Validates parser correctness
- Tests: examples/f90/*.f90
- Verifies: parse → emit → parse → emit produces identical output
- Script: `scripts/run_gfortran_roundtrip.py`

**5. Small Stack Test (Linux only)**
- Simulates Windows stack limits (1 MB)
- Validates stack usage safety
- Commands: `make test-small-stack TEST_STACK_KB=1024`
- Ensures fortfront works on Windows stack constraints

### Artifacts
- Build logs (on failure)
- Test logs (on failure)
- Duplication check report

### Environment
- **Linux**: Ubuntu latest, gfortran 11+
- **Windows**: Windows latest, gfortran via MinGW
- **Dependencies**: fpm, make, Python 3.8+

### Performance Targets
- Build: <5 minutes
- Tests: <10 minutes (all tests run)
- Total CI time: <15 minutes

## CI Configuration

### fpm.toml Settings
```toml
[build]
auto-executables = false
auto-tests = true
```

### Test Discovery
- All files in `test/*.f90` discovered automatically
- No manual test registration required

### Parallelism
- fpm manages test parallelism automatically
- **NEVER** pass `-j` flag to fpm or make
- Each test runs in separate process

## Local CI Simulation

Run CI checks locally before pushing:

```bash
# Build
make build

# Test
make test

# Duplication check
make check-duplication

# Round-trip validation
python scripts/run_gfortran_roundtrip.py examples/f90/*.f90

# Small stack test
make test-small-stack TEST_STACK_KB=1024
```

## Troubleshooting CI Failures

**Build Failures**:
- Check compiler errors in logs
- Verify module dependencies
- Run `make clean && make build` locally

**Test Failures**:
- Run failing test locally: `fpm test test_name`
- Check test logs for assertion failures
- Verify test uses `read_example()` for end-to-end tests

**Duplication Failures**:
- Run `make check-duplication` locally
- Review reported violations
- Extract inline code to `examples/`
- Update test to use `read_example()`

**Round-Trip Failures**:
- Run `python scripts/run_gfortran_roundtrip.py file.f90` locally
- Compare parse → emit → parse outputs
- Check for parser or codegen bugs

## CI Best Practices

**Before Committing**:
1. Run full test suite locally
2. Check for duplication violations
3. Ensure all tests pass
4. Verify build succeeds

**Pull Requests**:
- All CI checks must pass
- Duplication warnings should be addressed
- No failing tests accepted
- Build must succeed on all platforms

**Performance**:
- Keep tests fast (<120 seconds each)
- Avoid network I/O in tests
- Use timeouts to prevent hanging

## Dependencies

**GitHub Actions**:
- actions/checkout@v3 - Repository checkout
- actions/cache@v3 - Dependency caching

**Build Tools**:
- fpm (Fortran Package Manager)
- make (Build automation)
- gfortran (Fortran compiler)

**Utilities**:
- Python 3.8+ (for scripts)
- timeout (for test timeouts)
