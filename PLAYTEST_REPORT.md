# Playtest Report

## Summary
The `fortfront` tool was playtested for:
1.  Roundtrip of standard F90 files.
2.  Compilation of Lazy Fortran (.lf) files.
3.  Codebase analysis for issues.

## Prerequisites
- `gfortran` (installed via apt)
- `fpm` (downloaded binary v0.10.1)

## Test Results

### 1. Build & Test (`make test`)
- **Status**: **FAILED**
- **Details**:
  - `examples/lf/intrinsic_functions.lf`: `ERROR: Type environment capacity exceeded ( 2 )`
  - `test_all_examples` returned exit code 1.
- **Analysis**: The internal test runner seems to use a fixed-size type environment (likely `MAX_ENV_SIZE` related or a configuration in the test harness) that is insufficient for `intrinsic_functions.lf`.

### 2. Roundtrip F90
- **Status**: **PASSED** (CLI)
- **Verified Files**:
  - `examples/f90/do_loop_simple_fixed.f90`
  - `examples/f90/issue_2387_comment_roundtrip.f90`
  - `examples/f90/array_slice_basic.f90`
- **Observations**: Output compiles and is semantically equivalent. Some whitespace/formatting changes were observed but are expected.

### 3. Lazy Fortran (.lf)
- **Status**: **PASSED** (CLI)
- **Verified Files**:
  - `examples/lf/function_simple.lf`
  - `examples/lf/arrays.lf`
  - `examples/lf/intrinsic_functions.lf` (Passed via CLI, unlike `make test`)
- **Observations**: Generated F90 code compiles successfully with `gfortran`.

## Usage of Playtest Scripts
Two scripts were added to facilitate testing:
- `playtest_roundtrip.sh`: Verifies that F90 files roundtrip correctly.
- `playtest_lf.sh`: Verifies that .lf files compile to valid F90.

Usage:
```bash
# Locate the binary
FF_BIN=$(find build -name fortfront -type f | head -n 1)

# Run tests
FORTFRONT="$FF_BIN" ./playtest_roundtrip.sh examples/f90/do_loop_simple_fixed.f90
FORTFRONT="$FF_BIN" ./playtest_lf.sh examples/lf/function_simple.lf
```

## Codebase Analysis: Issues & Hacks

### 1. Hardcoded Limits
- `MAX_ENV_SIZE` in `src/semantic/types/type_system_unified.f90` is set to 4096. This seems to cause failures in the test suite (`intrinsic_functions.lf`).
- `MAX_INPUT_SIZE` in `app/fortfront.f90` is set to 10MB (`10485760`).

### 2. Unclean Solutions / Hacks
- **Code Splitting via Include**: `src/standardizers/ast_monomorphization.f90` includes `ast_monomorphization_part1.inc`, `part2.inc`, `part3.inc` inside the `contains` block. This indicates the module is too large and should be refactored into proper submodules or separate modules.
- **Hardcoded PID**: `app/fortfront.f90` has a `get_pid_impl` subroutine that returns a hardcoded `12345` with a TODO comment.
- **Custom Buffered IO**: `app/fortfront.f90` implements its own buffered reading logic instead of using `stdlib` or standard facilities, leading to verbose and potentially error-prone code.
- **Duplicate Logic**: `src/semantic/semantic_inference_helpers.f90` contains repetitive code for processing different AST nodes (e.g., `process_if_node_branches`, `process_do_loop_body`) which all do essentially the same thing (return a generic control type).

### 3. Test Discrepancy
- The CLI tool successfully compiles `intrinsic_functions.lf`, but the internal test runner fails on it with "capacity exceeded". This suggests inconsistent configuration between the app and the test harness.

## Recommendations
1.  Increase `MAX_ENV_SIZE` or implement dynamic resizing for the type environment in the test harness.
2.  Refactor `ast_monomorphization` to use proper modules instead of `include`.
3.  Implement real `getpid` (e.g., via C interop) or remove the dependency.
4.  Refactor `semantic_inference_helpers` to reduce duplication.
