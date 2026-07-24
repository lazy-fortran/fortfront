# Common Test Utilities

## Purpose

This directory provides shared helpers used by the test suite. It contains
shared helper modules, small include files for CLI-style I/O handling, and a
few focused tests for UID generation utilities.

## File Index

- `test_filesystem_helpers.f90` - Filesystem helpers for tests: temp directory
  creation and cleanup, path joining and normalization, example path handling,
  and locating the built `fortfront` executable.
- `test_shell_commands.f90` - Shell command helpers for tests: building compile
  commands and quoting arguments for the host shell.
- `read_example.inc` - `read_example()` helper that reads `examples/` files and
  fails tests with a consistent error message (includes CLI reader helpers).
- `cli_system_tests.inc` - Shared helpers for system-level CLI test programs.
- `test_ast_uid_integration.f90` - Integration coverage for AST UID behavior.
- `test_debug_uid.f90` - Focused tests for debug UID utilities.
- `test_simple_uid.f90` - Focused tests for simple UID generation.
- `test_uid_generator.f90` - Focused tests for UID generator behavior.

## Key Concepts

- Module helpers are used via `use` from test programs anywhere under `test/`.
  They live here rather than in `src/` so that test scaffolding is not compiled
  into the shipped `fortfront` library.
- Include-file helpers are used via Fortran `include` inside test program
  `contains` sections to avoid duplicating internal subroutines across many
  tests.
- Tests that need `read_example()` should include only `read_example.inc`.
- `read_example()` is used by end-to-end tests to keep `examples/` as the single
  canonical source of full-program inputs.

## Dependencies

- `iso_fortran_env` (intrinsic) - `error_unit`, `input_unit`, and iostat codes.
- `fortfront_constants` - path and search-line length limits used by
  `test_filesystem_helpers`.
