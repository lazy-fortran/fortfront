![fortfront](media/logo.svg)

# fortfront

Modern Fortran compiler frontend with lazy syntax support and type inference.

## Overview

fortfront transforms lazy Fortran code to standard Fortran through a 4-phase compilation pipeline: Lexer → Parser → Semantic Analysis → Code Generation.

**Key Features**:
- CLI interface for stdin/stdout processing
- Automatic variable type detection  
- Standard Fortran output generation
- Static library for integration

## Quick Start

```bash
# Transform lazy Fortran
echo "x = 42" | ./build/gfortran_*/app/fortfront
# Outputs: integer :: x declaration with assignment
```

## Building

### FPM/Make (Primary)
```bash
make         # Build project via fpm
make test    # Run the full test suite
```

### CMake
Removed to avoid confusion and build fragmentation. Use FPM/Make.

## Project Status

**Active Development**: Continuously maintained and improved.

**Current State**:
- ✅ FPM build system operational
- ✅ Test suite passes locally (<=120s)
- ❌ CMake build system removed (use FPM)

**Known Limitations**:
- String/array type inference incomplete
- Some large files exceed size targets
- Error handling migration in progress

## Architecture

**Core Components**:
1. **Lexer** - Tokenizes source code
2. **Parser** - Builds AST
3. **Semantic** - Type checking 
4. **Codegen** - Emits standard Fortran

## Integration

### With fortrun
[fortrun](https://github.com/lazy-fortran/fortrun) automatically uses fortfront for `.lf` files:
```bash
fortrun hello.lf
```

### As Static Library
```bash
sudo make install  # System-wide installation
gcc myapp.c $(pkg-config --cflags --libs fortfront)
```

## Documentation

- [Static Library Integration](docs/STATIC_LIBRARY_INTEGRATION.md)
- API Reference in `docs/` folder

## Contributing

Modern Fortran development practices with comprehensive testing, CI, and code review.

## License

MIT License - see LICENSE file for details.
