![fortfront](media/logo.svg)

Core analysis frontend for lazy fortran - transforms lazy Fortran to standard Fortran via CLI.

## Overview

fortfront transforms lazy Fortran code to standard Fortran:
- **Input**: Reads lazy fortran from stdin  
- **Output**: Writes standard fortran to stdout
- **Pipeline**: 4-phase transformation (lexer → parser → analysis → codegen)
- **Type Inference**: Automatic variable type detection 
- **Integration**: Designed for use with [fortrun](https://github.com/lazy-fortran/fortrun) build orchestrator

## Features

- **Pure CLI Interface**: No API dependencies, works as standalone command
- **High Performance**: <0.05ms average transformation time  
- **Enhanced Error Reporting**: Clear error messages with line/column info
- **Mixed Construct Support**: Handles modules with implicit main programs
- **Standard Compliant**: Generates clean, standard Fortran code
- **Type Inference**: Automatic variable typing algorithm

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

```bash
# Basic usage - simple statements
echo "x = 42" | fortfront

# Mixed constructs - module with main program
echo -e "module math\ninteger :: pi = 3\nend module\n\ninteger :: x\nx = pi * 2\nprint *, x" | fortfront
```

**Expected Output (simple):**
```fortran
program main
    implicit none
    integer :: x
    x = 42
end program main
```

**Expected Output (mixed constructs):**
```fortran
module math
    integer :: pi = 3
end module math
program main
    implicit none
    integer :: x

    x = pi*2
    print *, x
end program main
```

### Integration with fortrun

fortrun automatically uses fortfront for .lf files: `fortrun hello.lf`

## Documentation

See `docs/` folder for detailed guides, API reference, and build instructions.

## License

MIT License - see LICENSE file for details.

