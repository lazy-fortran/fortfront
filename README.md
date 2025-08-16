![fortfront](media/logo.svg)

Core analysis frontend for lazy fortran - transforms lazy Fortran to standard Fortran via CLI.

## Overview

fortfront is a pure CLI tool that transforms lazy Fortran code to standard Fortran:
- **Input**: Reads lazy fortran from stdin
- **Output**: Writes standard fortran to stdout
- **Pipeline**: 4-phase transformation (lexer → parser → semantic → codegen)
- **Type Inference**: Automatic variable type detection using Hindley-Milner algorithm
- **Integration**: Designed for use with fortrun build orchestrator (as fortfront)

## Features

- **Pure CLI Interface**: No API dependencies, works as standalone command
- **High Performance**: <0.05ms average transformation time
- **Robust Error Handling**: Graceful fallback for invalid syntax
- **Comprehensive Testing**: Unit tests for transformation function + CLI system tests
- **Standard Compliant**: Generates clean, standard Fortran code
- **Type Inference**: Hindley-Milner algorithm for automatic variable typing

## Building

```bash
fpm build
```

## Testing

```bash
fpm test
```

## Usage

### Command Line Interface

Transform lazy Fortran to standard Fortran:

```bash
# Basic usage - pipe lazy fortran through fortfront
echo "x = 42" | fortfront

# With file input/output
fortfront < input.lf > output.f90

# Example transformation
echo -e "x = 42\ny = 3.14\nprint *, x + y" | fortfront
```

**Output:**
```fortran
program main
    implicit none
    integer :: x
    real(8) :: y

    x = 42
    y = 3.14d0
    print *, x + y
end program main
```

### Input Validation

fortfront uses a dedicated `input_validation` module to validate input and provide enhanced error reporting before attempting transformation. This modular design separates validation concerns from the main frontend processing pipeline.

#### ✅ Accepted Input Types

**Comments and empty input:**
```bash
echo "! This is a comment" | fortfront
echo "" | fortfront  # Empty input generates minimal program
```

**Variable assignments:**
```bash
echo "x = 42" | fortfront
echo "result = 3.14 * radius" | fortfront
```

**Mathematical expressions:**
```bash
echo "a + b" | fortfront          # Simple operations
echo "(x + y) * z" | fortfront    # Expressions with parentheses
echo "x * y + z / 2.0" | fortfront # Mixed identifier/number expressions
```

**Function calls:**
```bash
echo "sqrt(x) + y" | fortfront
echo "result = sin(angle)" | fortfront
```

**Array operations:**
```bash
echo "arr(i)" | fortfront
echo "matrix(i, j) = value" | fortfront
```

#### ❌ Rejected Input Types

Input without clear Fortran structure is rejected with helpful error messages:

```bash
echo "random text" | fortfront
# Error: Input does not appear to be valid Fortran code. No recognized Fortran patterns found.

echo "123 ++ 456" | fortfront  
# Error: Invalid syntax patterns detected.
```

#### Validation Architecture

Input validation is handled by the dedicated `input_validation` module, which provides:

**Modular Design:**
- Clean separation from frontend processing logic
- Reusable validation functions for different use cases
- Independent error reporting with enhanced diagnostics
- No circular dependencies with other modules

**Core Validation Functions:**
- `validate_basic_syntax()` - Main validation entry point
- `check_missing_then_statements()` - Detects if-without-then errors
- `check_incomplete_statements()` - Finds dangling operators and incomplete expressions
- `check_for_fortran_content()` - Validates input contains recognizable Fortran patterns
- `check_missing_end_constructs()` - Detects missing end statements
- `contains_invalid_patterns()` - Identifies invalid syntax patterns
- `has_only_meaningless_tokens()` - Filters out non-Fortran content

**Multi-Phase Validation Process:**
1. **Phase 1:** Accept comments-only or empty input
2. **Phase 2:** Accept input containing Fortran keywords (implicit, program, etc.)
3. **Phase 3:** Check for likely Fortran patterns:
   - Must contain identifiers
   - Must have assignments, function calls, OR mathematical operators with operands
   - Limited unknown tokens allowed
4. **Phase 4:** Reject input without recognizable structure

This modular approach ensures lazy Fortran expressions are properly accepted while preventing meaningless input from causing confusing transformation failures.

#### Mathematical Expression Validation

**Fixed (Issue #259):** The validation logic now correctly accepts mathematical expressions with operators, whether they contain numbers or only identifiers:

```bash
# All mathematical expressions now work correctly:
echo "a + b" | fortfront          # ✅ Works: identifier-only expressions accepted
echo "x * y" | fortfront          # ✅ Works: identifier-only expressions accepted
echo "(a + b) * c" | fortfront    # ✅ Works: complex identifier expressions accepted
echo "x + 1" | fortfront          # ✅ Works: expressions with numbers accepted
echo "a * 2.0" | fortfront        # ✅ Works: expressions with numbers accepted
```

**Implementation:** The validation functions were extracted from `src/frontend.f90` into the dedicated `src/input_validation.f90` module. The main validation logic now accepts mathematical expressions based on the presence of operators alone, rather than requiring both operators and numbers. This supports lazy Fortran's goal of allowing concise mathematical expressions with identifiers only.

#### Standalone Validation Usage

The `input_validation` module can be used independently of the main fortfront frontend:

```fortran
use lexer_core, only: token_t, lex_source
use input_validation, only: validate_basic_syntax

character(len=:), allocatable :: source, error_msg
type(token_t), allocatable :: tokens(:)

! Example: Validate Fortran code without transformation
source = "if x > 0" // new_line('A') // "  print *, x"
call lex_source(source, tokens, error_msg)
call validate_basic_syntax(source, tokens, error_msg)

if (error_msg /= "") then
    print *, "Validation error: ", error_msg
    ! Error: Missing 'then' statement at line 1, column 9
    ! Source: if x > 0
    !         --------^
    ! Suggestion: Add 'then' after the condition
else
    print *, "Input is valid Fortran"
end if
```

**Use Cases for Standalone Validation:**
- **Editor Integration**: Real-time syntax checking in code editors
- **Build Tools**: Pre-compilation validation in build systems
- **Code Quality Tools**: Lint-style checking for Fortran code
- **Educational Tools**: Teaching Fortran syntax with immediate feedback

**Validation API:**
```fortran
! Primary validation interface
call validate_basic_syntax(source, tokens, error_msg)

! Specific validation checks (can be used individually)
call check_missing_then_statements(tokens, source_lines, error_msg)
call check_incomplete_statements(tokens, source_lines, error_msg)
call check_for_fortran_content(tokens, error_msg)
call check_missing_end_constructs(tokens, source_lines, error_msg)

! Utility functions
logical :: has_invalid_patterns = contains_invalid_patterns(tokens)
logical :: meaningless_input = has_only_meaningless_tokens(tokens)
```

### Integration with fortrun

fortfront is designed to work seamlessly with [fortrun](https://github.com/lazy-fortran/fortrun):

```bash
# fortrun automatically uses fortfront for .lf files
fortrun hello.lf

# Explicit usage in build scripts
fortfront < lazy_source.lf > standard_source.f90
gfortran standard_source.f90 -o program
```

### Library Usage (Advanced)

For direct integration in Fortran applications:

```fortran
use frontend, only: transform_lazy_fortran_string

character(len=:), allocatable :: input, output, error_msg

input = "x = 42" // new_line('A') // "print *, x"
call transform_lazy_fortran_string(input, output, error_msg)

if (error_msg == "") then
    print *, output
else
    print *, "Error: ", error_msg
end if
```

#### Error Handling in Library Usage

The `transform_lazy_fortran_string` subroutine performs input validation and returns appropriate error messages:

```fortran
! Valid mathematical expression (now works with identifiers only)
input = "a + b * c"
call transform_lazy_fortran_string(input, output, error_msg)
! error_msg will be empty, output contains transformed code

! Invalid input 
input = "random text without fortran structure"
call transform_lazy_fortran_string(input, output, error_msg)
! error_msg = "Input does not appear to be valid Fortran code. No recognized Fortran patterns found."

! Comments-only input (valid)
input = "! This is just a comment"
call transform_lazy_fortran_string(input, output, error_msg)
! error_msg is empty, output contains minimal valid program
```

## License

MIT License - see LICENSE file for details.

