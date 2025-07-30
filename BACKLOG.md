# fortfront CLI Decoupling Implementation Backlog

## Overview

This backlog implements the complete decoupling of fortfront and fortrun by converting fortfront into a pure CLI tool that reads lazy fortran from stdin and writes standard fortran to stdout.

## IMPORTANT: File Movement and Editing Guidelines

**⚠️ CRITICAL: This backlog involves editing files in ../fortrun/ (external project) and moving code between projects.**

### File Operations Summary:
- **EDIT files in ../fortrun/**: Modify existing fortrun source files to remove fortfront dependencies
- **MOVE code from ../fortrun/ to fortfront/**: Transfer appropriate functionality from fortrun to fortfront
- **CREATE new files in fortfront/**: Add new source and test files to fortfront project
- **DELETE files in fortfront/**: Remove obsolete API modules

### Project Boundaries:
- **fortfront/**: Will contain transformation logic + CLI interface (this project)
- **../fortrun/**: Will contain build orchestration only (external project)

### File Operations by Project:

#### In fortfront/ (this project):
- **CREATE**: `app/fortfront.f90` (CLI wrapper - automatically built by fpm)
- **CREATE**: `test/frontend/test_string_transformation.f90` (unit tests)
- **CREATE**: `test/system/test_cli_integration.f90` (minimal system tests)
- **EDIT**: `src/frontend.f90` (add string transformation function)
- **EDIT**: `TESTS.md`, `README.md`, `FORTRUN.md` (documentation)
- **DELETE**: `src/frontend_integration.f90`, `src/debug_state.f90` (obsolete API modules)

#### In ../fortrun/ (external project):
- **EDIT**: `src/runner/runner.f90` (remove fortfront imports, add CLI calls)
- **CREATE**: `test/integration/test_cli_integration.f90` (integration tests)
- **CREATE**: `test/performance/test_cli_performance.f90` (performance tests)  
- **CREATE**: `test/error/test_cli_error_handling.f90` (error tests)
- **EDIT**: `README.md`, `FORTFRONT.md` (documentation updates)

## Phase 1: Create Minimal fortfront CLI

### Task 1.1: Create CLI Application Structure
**Priority**: High  
**Effort**: 1 hour  
**Description**: Set up the basic CLI application structure

**Steps**:
1. Create `app/fortfront.f90` with minimal structure
2. Test basic compilation with `fpm build` (no fpm.toml changes needed)
3. Verify executable is built automatically

**Acceptance Criteria**:
- `fpm build` produces `fortfront` executable automatically
- Executable runs without arguments (even if it does nothing yet)
- No build errors or warnings

**Files Changed**:
- `fortfront/app/fortfront.f90` (CREATE new file - fpm auto-builds based on filename)

### Task 1.2: Implement String-Based Transformation Function
**Priority**: High  
**Effort**: 4 hours  
**Description**: Create the core function that transforms lazy fortran strings to standard fortran strings

**Steps**:
1. Add `transform_lazy_fortran_string()` function to existing frontend module
2. Function signature: `subroutine transform_lazy_fortran_string(input, output, error_msg)`
3. Integrate with existing 4-phase pipeline (lexer→parser→semantic→codegen)
4. Handle all error cases with clear error messages

**Acceptance Criteria**:
- Function transforms lazy fortran string to standard fortran string
- Error cases return descriptive error messages, not crashes
- Function is pure transformation (no file I/O, no global state)
- Success/failure clearly indicated by empty/non-empty error_msg

**Files Changed**:
- `fortfront/src/frontend.f90` (EDIT to add string transformation function)

### Task 1.3: Implement Thin CLI Wrapper
**Priority**: High  
**Effort**: 2 hours  
**Description**: Create minimal CLI that just calls the string transformation function

**Steps**:
1. Implement `read_stdin()` to read all input into string
2. Call `transform_lazy_fortran_string()`
3. Write output to stdout or error to stderr
4. Set appropriate exit codes

**Acceptance Criteria**:
- CLI is <50 lines of code (thin wrapper)
- All logic is in the underlying function, not the CLI
- CLI handles only I/O and exit codes

**Files Changed**:
- `fortfront/app/fortfront.f90` (EDIT to implement CLI wrapper)

### Task 1.4: Unit Tests for String Transformation Function
**Priority**: High  
**Effort**: 4 hours  
**Description**: Create comprehensive unit tests for the core transformation function

**Steps**:
1. Create `test/frontend/test_string_transformation.f90`
2. Test function directly (no CLI subprocess calls)
3. Test all transformation cases: hello world, type inference, control flow, etc.
4. Test all error cases: syntax errors, semantic errors, etc.

**Test Cases**:
```fortran
! Test 1: Simple hello world
call transform_lazy_fortran_string("print *, 'Hello'", output, error)
assert(len_trim(error) == 0)
assert(index(output, "program main") > 0)
assert(index(output, "implicit none") > 0)

! Test 2: Type inference
call transform_lazy_fortran_string("x = 42" // new_line('A') // "y = 3.14", output, error)
assert(len_trim(error) == 0)
assert(index(output, "integer :: x") > 0)
assert(index(output, "real") > 0)

! Test 3: Syntax error
call transform_lazy_fortran_string("invalid fortran !!!", output, error)
assert(len_trim(error) > 0)
assert(index(error, "syntax") > 0 .or. index(error, "parse") > 0)
```

**Acceptance Criteria**:
- All unit tests pass via `fpm test test_string_transformation`
- Tests directly call function (no subprocess overhead)
- **Automatic coverage**: Tests exercise the full 4-phase pipeline
- Tests cover all major transformation scenarios

**Files Changed**:
- `fortfront/test/frontend/test_string_transformation.f90` (CREATE new file)

### Task 1.5: Minimal CLI System Tests  
**Priority**: Low  
**Effort**: 1 hour  
**Description**: Create minimal system tests just to verify CLI I/O works

**Steps**:
1. Create `test/system/test_cli_integration.f90`
2. Test only basic I/O: stdin→stdout, stderr, exit codes
3. 2-3 simple tests maximum (success case, error case)
4. Focus on CLI wrapper, not transformation logic

**Test Cases**:
```fortran
! Test 1: Basic CLI I/O works
command: echo "print *, 'test'" | fortfront
expected: Valid fortran output to stdout, exit code 0

! Test 2: Error handling works  
command: echo "invalid" | fortfront
expected: Error to stderr, exit code 1
```

**Acceptance Criteria**:
- Minimal system tests (2-3 tests only)
- Tests verify CLI wrapper works
- All transformation logic tested via unit tests (Task 1.4)

**Files Changed**:
- `fortfront/test/system/test_cli_integration.f90` (CREATE new file, minimal)

### Task 1.6: Update Test Coverage Tracking
**Priority**: Medium  
**Effort**: 1 hour  
**Description**: Update TESTS.md to reflect CLI testing approach

**Steps**:
1. Add CLI testing section to TESTS.md
2. Document how CLI tests contribute to coverage
3. Update coverage goals to include CLI path

**Acceptance Criteria**:
- TESTS.md includes CLI testing strategy
- Coverage tracking accounts for CLI execution path
- Documentation is clear and actionable

**Files Changed**:
- `fortfront/TESTS.md` (EDIT documentation to reflect new testing approach)

## Phase 2: Update fortrun Integration

### Task 2.1: Add File Extension Detection to fortrun
**Priority**: High  
**Effort**: 1 hour  
**Description**: Implement simple file extension detection in fortrun

**Steps**:
1. Add `is_lazy_fortran_file()` function to fortrun
2. Replace existing `use frontend_integration, only: is_simple_fortran_file` calls
3. Test file detection logic

**Acceptance Criteria**:
- `.lf` and `.LF` files detected as lazy fortran
- `.f90`, `.F90` files NOT detected as lazy fortran  
- Function is pure and handles edge cases (no extension, etc.)

**Files Changed**:
- `../fortrun/src/runner/runner.f90` (EDIT to add is_lazy_fortran_file function, replace existing calls)

### Task 2.2: Replace API Calls with CLI Subprocess
**Priority**: High  
**Effort**: 3 hours  
**Description**: Replace all fortfront API calls with CLI subprocess execution

**Steps**:
1. Replace `compile_with_frontend()` calls with `execute_command_line()`
2. Handle file redirection (`< input > output 2> error`)
3. Parse exit codes and error files
4. Update error handling to read stderr output

**Acceptance Criteria**:
- No more `use frontend_integration` or `use debug_state` imports
- Lazy fortran files transformed via CLI subprocess
- Error messages properly captured and reported
- Same functionality as before, different implementation

**Files Changed**:
- `../fortrun/src/runner/runner.f90` (EDIT to remove all fortfront imports, replace API calls with execute_command_line)

### Task 2.3: End-to-End Integration Testing
**Priority**: High  
**Effort**: 4 hours  
**Description**: Test complete fortrun workflow with CLI integration

**Steps**:
1. Create integration test with sample `.lf` file
2. Test caching behavior (CLI call only on cache miss)
3. Test error propagation from fortfront CLI
4. Test that `scan_modules` works on transformed output

**Test Cases**:
- Hello world `.lf` file → successful build and execution
- Type inference `.lf` file → correct variable declarations in cache
- Syntax error `.lf` file → proper error reporting
- Cache hit scenario → no CLI call made

**Acceptance Criteria**:
- All integration tests pass
- No regression in fortrun functionality
- Performance acceptable (subprocess overhead minimal)

**Files Changed**:
- `../fortrun/test/integration/test_cli_integration.f90` (CREATE new test file in fortrun project)

## Phase 3: Cleanup and Optimization

### Task 3.1: Remove Unused fortfront Modules
**Priority**: Medium  
**Effort**: 2 hours  
**Description**: Clean up modules no longer needed for CLI-only approach

**Steps**:
1. Remove `src/frontend_integration.f90`
2. Remove `src/debug_state.f90` 
3. Update any internal dependencies
4. Verify build still works

**Acceptance Criteria**:
- `fpm build` succeeds without removed modules
- No dead code warnings
- CLI functionality unchanged

**Files Changed**:
- `fortfront/src/frontend_integration.f90` (DELETE obsolete API module)
- `fortfront/src/debug_state.f90` (DELETE obsolete debug module)  
- `fortfront/src/*.f90` (EDIT any modules that imported deleted modules)

### Task 3.2: Performance Testing
**Priority**: Medium  
**Effort**: 2 hours  
**Description**: Verify subprocess overhead is acceptable

**Steps**:
1. Benchmark fortrun with CLI vs old API approach
2. Test with various file sizes
3. Measure cache hit vs miss performance
4. Profile subprocess creation overhead

**Acceptance Criteria**:
- < 10% performance regression for typical use cases
- Large files (>1000 lines) have negligible overhead
- Cache hit performance unaffected

**Files Changed**:
- `../fortrun/test/performance/test_cli_performance.f90` (CREATE new performance tests in fortrun project)

### Task 3.3: Error Handling Improvements
**Priority**: Medium  
**Effort**: 2 hours  
**Description**: Improve error messages and handling for CLI failures

**Steps**:
1. Better error parsing from CLI stderr
2. Handle CLI not found / not executable
3. Timeout handling for long-running transformations
4. Clear error messages for users

**Acceptance Criteria**:
- Clear error messages for common failure modes
- Graceful handling when `fortfront` not in PATH
- No hanging on malformed input

**Files Changed**:
- `../fortrun/src/runner/runner.f90` (EDIT to improve error handling for CLI failures)
- `../fortrun/test/error/test_cli_error_handling.f90` (CREATE new error handling tests in fortrun project)

### Task 3.4: Documentation Updates
**Priority**: Low  
**Effort**: 1 hour  
**Description**: Update all documentation to reflect CLI approach

**Steps**:
1. Update README files in both projects
2. Update FORTRUN.md with final implementation
3. Add CLI usage examples
4. Update API documentation (removal of API)

**Acceptance Criteria**:
- Documentation accurately reflects implementation
- Clear examples of CLI usage  
- Migration notes for existing users

**Files Changed**:
- `fortfront/README.md` (EDIT to document CLI interface)
- `fortfront/FORTRUN.md` (EDIT to reflect final implementation)
- `../fortrun/README.md` (EDIT to document fortfront CLI integration)
- `../fortrun/FORTFRONT.md` (EDIT to reflect CLI-based approach)

## Testing Strategy Integration with TESTS.md

### Function-Focused Coverage Strategy

Based on current TESTS.md showing only 34 measurable lines with 91.2% coverage, the string transformation function approach provides automatic coverage:

**New Coverage Sources**:
1. **String transformation function** - Direct function calls provide measurable lines
2. **String-based pipeline** - Exercises existing 4-phase pipeline through new entry point
3. **Error handling paths** - Direct error testing without CLI overhead

**Updated Test Categories**:

#### String Transformation Unit Tests
```fortran
! test/frontend/test_string_transformation.f90  
! Covers: Core transformation function directly
! Target coverage: 100% of transformation logic
! Benefit: Automatic coverage of entire pipeline (lexer→parser→semantic→codegen)
```

#### Minimal CLI System Tests
```fortran
! test/system/test_cli_integration.f90
! Covers: Only CLI wrapper I/O logic (2-3 tests)
! Target coverage: CLI wrapper verification only
! Benefit: Verify interface works, minimal test overhead
```

#### Existing Pipeline Tests
```fortran
! All existing tests remain unchanged
! Benefit: Dual coverage - existing tests + new string function tests
! Result: Much higher coverage with same test effort
```

### Coverage Goals Update

**Before**: 91.2% (31/34 lines)  
**After**: Target 95%+ with ~100+ measurable lines

**New measurable code**:
- String transformation function (~30 lines)
- CLI wrapper I/O logic (~20 lines) 
- Enhanced error handling (~10 lines)

**Coverage Strategy**:
- **Primary**: Unit tests call `transform_lazy_fortran_string()` directly
- **Secondary**: Minimal system tests for CLI wrapper
- **Automatic**: Pipeline coverage through string function calls

### Test Execution Strategy

**Phase 1 Testing**:
- Unit tests call transformation function directly (fast, comprehensive)
- Minimal CLI system tests verify wrapper only (2-3 tests)
- **Result**: High coverage, low test execution time

**Phase 2 Testing**:
- Integration tests in fortrun (using CLI subprocess)
- End-to-end workflow testing
- **Result**: Real-world validation

**Phase 3 Testing**:
- Performance validation (function vs CLI)
- Regression testing
- **Result**: Production readiness

## Success Criteria

### Technical Metrics
- [ ] Complete decoupling: Zero fortfront imports in fortrun
- [ ] CLI functionality: `fortfront` works standalone 
- [ ] Test coverage: >95% with CLI tests
- [ ] Performance: <10% regression in fortrun workflows
- [ ] Error handling: Clear messages for all failure modes

### Process Metrics  
- [ ] All tasks completed within estimated effort
- [ ] No breaking changes for fortrun end users
- [ ] Documentation complete and accurate
- [ ] Clean git history with logical commits

### Quality Metrics
- [ ] No compiler warnings in either project
- [ ] All tests pass in both projects
- [ ] CLI follows Unix conventions (exit codes, stderr, etc.)
- [ ] Code review approved for all changes

## Risk Mitigation

**Risk**: Subprocess overhead impacts performance  
**Mitigation**: Benchmark early, optimize if needed, consider process reuse

**Risk**: Error handling becomes complex  
**Mitigation**: Start with simple error propagation, iterate based on testing

**Risk**: CLI interface becomes complex over time  
**Mitigation**: Document principle of "no arguments, no options" and stick to it

**Risk**: Integration testing is insufficient  
**Mitigation**: Comprehensive end-to-end tests in Phase 2, validate real-world usage

This backlog provides a complete roadmap for decoupling fortfront and fortrun while maintaining functionality and improving testability.