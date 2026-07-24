# Utilities

## Purpose

This directory provides general-purpose utility functions used throughout the fortfront compiler: string manipulation, debug tracing, CLI environment handling, input validation, intrinsic function registry, procedure classification, and path validation. These are foundational services that don't fit into other specific subsystems.

## File Index

| File | Description |
|------|-------------|
| fortfront_utils.f90 | General fortfront utilities |
| fortfront_node_constants.f90 | Constants for AST node types |
| frontend_utilities.f90 | Frontend-specific utility functions |
| string_utils.f90 | Basic string manipulation utilities |
| string_types.f90 | String type definitions |
| string_builder.f90 | Efficient string building (concatenation) |
| type_string_utils.f90 | Type-to-string conversion utilities |
| debug_trace.f90 | Debug tracing and logging infrastructure |
| cli_env.f90 | Command-line environment handling |
| input_validation.f90 | Input validation facade (includes two parts) |
| input_validation_part1.inc | Input validation part 1: syntax checking |
| input_validation_part2.inc | Input validation part 2: semantic validation |
| path_validation.f90 | File path validation and normalization |
| intrinsic_registry.f90 | Registry of Fortran intrinsic functions |
| ieee_intrinsic_module.f90 | F2003 IEEE_ARITHMETIC/IEEE_EXCEPTIONS/IEEE_FEATURES module and procedure recognition |
| procedure_classification.f90 | Classify procedures (function vs subroutine, pure, etc.) |
| process_exit.f90 | Process exit handling |
| stdout_sanitizer.f90 | Sanitize stdout output (remove control characters) |
| stdout_sanitizer.c | C implementation of stdout sanitization |
| test_filesystem_helpers.f90 | Filesystem utilities used by tests (temp dirs, cleanup, path ops) |
| test_shell_commands.f90 | Shell command helpers used by tests (build compile/run commands) |

## Key Concepts

**String Utilities**
- Case conversion (upper/lower)
- Trimming whitespace
- String splitting and joining
- Substring search and replace
- Efficient concatenation via string builder

**Debug Tracing**
- Configurable trace levels (error, warning, info, debug)
- Trace to file or stderr
- Conditional tracing (compile-time or runtime)
- Performance impact minimal when disabled

**CLI Environment**
- Parse command-line arguments
- Environment variable access
- Working directory handling
- Standard input/output management

**Input Validation**
- Syntax validation: Check for common syntax errors before parsing
- Semantic validation: Check for semantic issues before analysis
- Early error detection: Fail fast with clear messages
- Helpful suggestions: Suggest fixes for common mistakes

**Intrinsic Function Registry**
- Complete list of Fortran intrinsics
- Type signatures for each intrinsic
- Return type rules
- Elemental vs transformational classification
- Used by semantic analyzer for type checking

**Procedure Classification**
- Determine if procedure is function or subroutine
- Check for prefix attributes (pure, elemental, recursive)
- Identify entry points (legacy Fortran)
- Used by parser and semantic analyzer

**Path Validation**
- Normalize file paths (remove `..`, `.`, etc.)
- Check file existence
- Validate directory paths
- Handle relative vs absolute paths
- Cross-platform path handling

**Stdout Sanitization**
- Remove ANSI escape codes
- Remove control characters
- Ensure clean output for piping
- Platform-specific handling (Windows vs UNIX)

## Dependencies

**Standard Library**
- `stdlib` - String utilities, I/O

**Common Utilities**
- `common/identifier_table` - Identifier management (for intrinsic names)

**Error Handling**
- `error_handling` - Error reporting infrastructure
