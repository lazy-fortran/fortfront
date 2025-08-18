![fortfront](media/logo.svg)

Core analysis frontend for lazy fortran - transforms lazy Fortran to standard Fortran via CLI.

## Overview

fortfront transforms lazy Fortran code to standard Fortran:
- **Input**: Reads lazy fortran from stdin  
- **Output**: Writes standard fortran to stdout
- **Pipeline**: 4-phase transformation (lexer → parser → analysis → codegen)
- **Type Inference**: Automatic variable type detection 
- **Integration**: Designed for use with fortrun build orchestrator

## Features

- **Pure CLI Interface**: No API dependencies, works as standalone command
- **High Performance**: <0.05ms average transformation time  
- **Enhanced Error Reporting**: Clear error messages with line/column info
- **Comprehensive Testing**: Unit tests for transformation + CLI system tests
- **Standard Compliant**: Generates clean, standard Fortran code
- **Type Inference**: Automatic variable typing algorithm
- **Extensible Architecture**: Plugin-based analysis pipeline

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
# Basic usage
echo "x = 42" | fortfront

# With file input/output  
fortfront < input.lf > output.f90
```

**Expected Output:**
```fortran
program main
    implicit none
    integer :: x
    x = 42
end program main
```

### Integration with fortrun

```bash
# fortrun automatically uses fortfront for .lf files
fortrun hello.lf

# Explicit usage in build scripts  
fortfront < lazy_source.lf > standard_source.f90
gfortran standard_source.f90 -o program
```

## Error Reporting

fortfront provides comprehensive error reporting:
- **Precise Location**: Line and column numbers for each error
- **Clear Descriptions**: Specific problem identification
- **Fix Suggestions**: Actionable advice when possible
- **Source Context**: Shows problematic source lines

## Documentation

- **Detailed Guides**: See `docs/` folder for comprehensive documentation
- **API Reference**: `docs/SEMANTIC_EXTENSIBILITY_GUIDE.md` for plugin development
- **Error Handling**: `docs/ERROR_HANDLING_GUIDE.md` for library integration
- **Build System**: `CLAUDE.md` for development setup and build instructions

## License

MIT License - see LICENSE file for details.