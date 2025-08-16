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

fortfront validates input to ensure it contains recognizable Fortran patterns before attempting transformation:

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

#### Validation Logic

fortfront uses a multi-phase validation approach:

1. **Phase 1:** Accept comments-only or empty input
2. **Phase 2:** Accept input containing Fortran keywords (implicit, program, etc.)
3. **Phase 3:** Check for likely Fortran patterns:
   - Must contain identifiers
   - Must have assignments, function calls, OR mathematical operators with operands
   - Limited unknown tokens allowed
4. **Phase 4:** Reject input without recognizable structure

This ensures lazy Fortran expressions are properly accepted while preventing meaningless input from causing confusing transformation failures.

#### Known Issue: Overly Strict Mathematical Expression Validation

**Current Bug (Issue #259):** The validation logic incorrectly requires BOTH operators AND numbers for mathematical expressions, causing valid identifier-only expressions to be rejected:

```bash
# These SHOULD work but are currently rejected:
echo "a + b" | fortfront          # ❌ Currently fails: no numbers present
echo "x * y" | fortfront          # ❌ Currently fails: no numbers present  
echo "(a + b) * c" | fortfront    # ❌ Currently fails: no numbers present

# These work correctly (contain numbers):
echo "x + 1" | fortfront          # ✅ Works: has both operators and numbers
echo "a * 2.0" | fortfront        # ✅ Works: has both operators and numbers
```

**Planned Fix:** The validation logic will be refined to accept mathematical expressions with operators even when they contain only identifiers (no numeric literals). This supports lazy Fortran's goal of allowing concise mathematical expressions without requiring explicit numeric constants.

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
! Valid mathematical expression
input = "a + b * c"
call transform_lazy_fortran_string(input, output, error_msg)
! After Issue #259 fix: error_msg will be empty, output contains transformed code

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

