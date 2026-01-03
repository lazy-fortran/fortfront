# Test

## Purpose

This directory contains the complete test suite for fortfront, organized by subsystem and test type. Tests follow the zero-duplication policy: end-to-end tests MUST reference `examples/` directory, unit tests use inline code. The test suite validates lexing, parsing, semantic analysis, code generation, and the complete transformation pipeline.

## Directory Structure

- `analysis/` - Analysis subsystem tests (call graph, variable usage)
- `api/` - Public API tests
- `ast/` - AST tests (node creation, traversal, arena)
- `build/` - Build system tests
- `cli/` - CLI application tests
- `cli_env/` - CLI environment tests
- `codegen/` - Code generation tests
- `common/` - Common utilities tests
- `error_handling/` - Error handling tests
- `error_reporting/` - Error reporting tests
- `fixtures/` - Test fixtures and helpers
- `frontend/` - Frontend pipeline tests
- `integration/` - Integration tests (organized by feature and issue)
- `intrinsic/` - Intrinsic function tests
- `lazy_fortran/` - Lazy Fortran transformation tests
- `lexer/` - Lexer tests
- `memory/` - Memory management tests
- `parser/` - Parser tests
- `semantic/` - Semantic analysis tests
- `snapshots/` - Snapshot tests (golden file comparisons)
- `standardizer/` - Standardizer tests
- `system/` - System-level integration tests
- `utilities/` - Utility function tests

## Key Concepts

**Test Hierarchy and Zero-Duplication Policy**

For complete test organization policy, see [CLAUDE.md Examples & Tests Organization](../CLAUDE.md#examples--tests-organization).

**Quick Summary**:
- **Unit tests**: Inline code encouraged (test individual functions)
- **Integration tests**: Use judgment (small inline OK, large use examples/)
- **End-to-end tests**: MUST reference `examples/` directory

**Example Usage**:
```fortran
! UNIT TEST - inline code is PERFECT
program test_parse_assignment
    node = parse_assignment("x = 42")
end program

! END-TO-END TEST - MUST use examples/
program test_transform_lazy_fortran
    call read_example('examples/lf/square_function.lf', source)
    call transform_lazy_fortran_string(source, output, errors)
end program
```

**Test Organization**
- Tests grouped by subsystem (lexer, parser, semantic, etc.)
- Integration tests organized by feature or issue number
- Issue-specific tests: `test_issue_NNNN_description.f90`
- Feature tests in subdirectories: `integration/array_tests/`, `integration/type_tests/`

**Test Naming Convention**
- `test_<subsystem>_<feature>.f90` - Feature tests
- `test_issue_NNNN_<description>.f90` - Issue regression tests
- Descriptive names that explain what's being tested
- Example: `test/integration/issue_tests/test_issue_2563_dot_notation_member_access.f90`

**Test Execution**
```bash
# Run all tests
fpm test

# Run specific test
fpm test test_issue_2016_nested_call_types

# Run with small stack (Windows simulation)
make test-small-stack TEST_STACK_KB=1024
```

**CI Validation**
- All tests must pass before merge
- Tests run on Linux and Windows
- Small stack tests validate Windows compatibility
- Duplication check warns on inline code violations (see `make check-duplication`)

**Test Utilities**
- `fixtures/` - Shared test helpers
- `read_example()` - Read example files
- Assertion helpers - Validate results
- Mock objects - Stub dependencies

## Dependencies

**Test Framework**
- `test-drive` - Testing framework

**All Fortfront Modules**
- Tests depend on all fortfront subsystems

**Examples**
- `examples/` - Canonical example source files (referenced by end-to-end tests)
