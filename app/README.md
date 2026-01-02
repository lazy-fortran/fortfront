# App

## Purpose

This directory contains the CLI(command - line interface) application for &
    fortfront.The CLI provides a user - friendly interface for transforming lazy &
    Fortran to standard Fortran and for debugging the compilation pipeline.

## File Index

|File|Description|
|------|-------------|
|fortfront.f90|Main CLI application:argument parsing, file I / O, transformation &
    orchestration|
|debug_ast.f90|Debug utilities for printing AST structure(development tool) |

## Key Concepts

**CLI Usage**
```bash
# Transform lazy Fortran file to standard Fortran
fortfront input.lf > output.f90

# Transform from stdin
echo "x = 5"|fortfront > output.f90

# With options
fortfront - -trace input.lf > output.f90  # Enable debug tracing
fortfront - -std = lf input.lf > output.f90 &
# Strict mode (requires explicit program unit)
                   ```

**Command - Line interface**
-**Input**:File path or stdin
-**Output**:Stdout(for piping) or file
-**Errors**:Stderr with clear messages
-**exit codes**:0 for success, non - zero for errors

**File Handling**
-read input files(`.lf`or`.f90`)
-write output to stdout or file
-Handle stdin piping(UNIX and Windows)
-Proper error messages for missing files

**Transformation Pipeline**
1.read input source
2.Lex→Parse→Semantic→Codegen
3.write standardized output
4.Report errors if any phase fails

**Debug Mode**
-print AST structure for debugging
-Show type inference results
-Display call graph
-Useful for development and bug reports

**Error Reporting**
-Parse errors with line / column information
-type errors with context
-Helpful suggestions for fixes
-exit with appropriate error code

**CLI Options**
-`-h, --help`-Show help message
-`-v, --version`-Show version information
-`--infer`-Infer mode(default):accepts top - level statements and infers &
    missing structure
-`--std = lf`, `--std lf`-Strict mode:require explicit`program`/`module`/procedure &
    units(rejects bare statements)
-`--trace[=on|off] `-Enable / disable debug tracing(overrides FORTFRONT_TRACE env)
-`--trace - file < path > `-Trace output file path(overrides FORTFRONT_TRACE_FILE env)

Planned CLI features are tracked in the GitHub issue tracker.

## Dependencies

**Frontend**
-`frontend/`-Transformation pipeline

**Transformation API**
-`transformation_api`-High - level transformation interface

    **Error Handling**
    -`error_handling`-Error reporting

    **Utilities**
    -`utilities / cli_env`-CLI environment handling
    -`utilities / path_validation`-File path validation
