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
- **Static Library**: `libfortfront.a` for integration into external projects
- **Multi-Language Support**: C, C++, Fortran, and Rust interfaces
- **High Performance**: <0.05ms average transformation time  
- **Enhanced Error Reporting**: Clear error messages with line/column info
- **Mixed Construct Support**: Handles modules with implicit main programs
- **Standard Compliant**: Generates clean, standard Fortran code
- **Type Inference**: Automatic variable typing algorithm with enhanced character type handling

## Building

```bash
# Build CLI tool
fpm build && fpm test

# Build static library
make libfortfront.a
```

## Usage

Basic transformation pipeline:

```bash
# Simple usage
echo "x = 42" | fortfront

# Character handling  
echo 'name = "hello" // " world"' | fortfront
```

**Output:**
```fortran
program main
    implicit none
    integer :: x
    character(len=11) :: name
    x = 42
    name = "hello" // " world"
end program main
```

### Integration with fortrun

fortrun automatically uses fortfront for .lf files: `fortrun hello.lf`

### Static Library Integration

```bash
# Install system-wide
sudo make install

# Use in C projects
gcc -o myapp myapp.c $(pkg-config --cflags --libs fortfront)

# Use in C++ projects  
g++ -o myapp myapp.cpp $(pkg-config --cflags --libs fortfront)
```

## Documentation

- **[Static Library Integration](docs/STATIC_LIBRARY_INTEGRATION.md)** - Complete guide for using `libfortfront.a`
- **API Reference** - See `docs/` folder for detailed guides and build instructions

## License

MIT License - see LICENSE file for details.

