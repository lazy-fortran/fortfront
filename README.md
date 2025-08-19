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
- **Enhanced Type Safety**: Comprehensive type validation preventing runtime errors
- **Comprehensive Testing**: Unit tests for transformation + CLI system tests
- **Standard Compliant**: Generates clean, standard Fortran code
- **Automatic Variable Declarations**: Generates proper Fortran declarations for function parameters and variables
- **Type Inference**: Automatic variable typing using Fortran implicit rules
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

# Function with automatic variable declarations
echo "function twice(x) result(y)
y = 2*x
end function" | fortfront
```

**Expected Output:**
```fortran
program main
    implicit none
contains
    function twice(x) result(y)
        implicit none
        real(8), intent(in) :: x
        real(8) :: y
        y = 2*x
    end function twice
end program main
```

### Type Safety and Variable Declaration Examples

```bash
# Enhanced type inference with validation
echo "function calculate(a, b) result(sum)
sum = a + b  ! Types validated automatically
end function" | fortfront

# Mixed type operations handled safely
echo "function mixed_calc(i, x) result(y)
y = i + x  ! Integer + real -> real result
end function" | fortfront

# Integer variables (i,j,k,l,m,n) 
echo "function count(i) result(n)
n = i + 1
end function" | fortfront
```

### Integration with fortrun

```bash
# fortrun automatically uses fortfront for .lf files
fortrun hello.lf

# Explicit usage in build scripts  
fortfront < lazy_source.lf > standard_source.f90
gfortran standard_source.f90 -o program
```

## Enhanced Type Safety and Error Reporting

fortfront provides comprehensive type safety and error reporting:

### Type Safety Features
- **Comprehensive Validation**: All type assignments go through validation
- **Safe Type Inference**: Prevents type mismatches in generated code
- **Mixed Type Handling**: Safely handles integer/real combinations
- **Error Prevention**: Catches type issues before code generation

### Error Reporting Features  
- **Precise Location**: Line and column numbers for each error
- **Clear Descriptions**: Specific problem identification
- **Fix Suggestions**: Actionable advice when possible
- **Source Context**: Shows problematic source lines

## Documentation

- **Type Safety**: `docs/TYPE_SAFETY_GUIDE.md` for enhanced type validation features
- **Variable Declarations**: `docs/VARIABLE_DECLARATIONS.md` for automatic declaration generation
- **Error Handling**: `docs/ERROR_HANDLING_GUIDE.md` for library integration
- **API Reference**: `docs/SEMANTIC_EXTENSIBILITY_GUIDE.md` for plugin development
- **Build System**: `CLAUDE.md` for development setup and build instructions

## License

MIT License - see LICENSE file for details.