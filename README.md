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

### Integration with fortrun

fortfront is designed to work seamlessly with [fortrun](../fortrun):

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

## License

MIT License - see LICENSE file for details.

