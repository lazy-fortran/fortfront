![fortfront](media/logo.svg)

# fortfront

Modern Fortran compiler frontend with lazy syntax support and type inference.

## Overview

fortfront is a functional Fortran compiler frontend that transforms lazy Fortran code to standard Fortran. The project provides a CLI tool and library for processing Fortran source code through a complete 4-phase compilation pipeline.

**Key Features**:
- **CLI Interface**: Processes lazy Fortran from stdin to standard Fortran on stdout
- **4-Phase Pipeline**: Lexer → Parser → Semantic Analysis → Code Generation
- **Type Inference**: Automatic variable type detection for lazy Fortran syntax
- **Standard Compliant**: Generates valid Fortran code compatible with standard compilers
- **Library Integration**: Available as static library for integration with other tools

## Quick Start

### Basic Usage

```bash
# Transform lazy Fortran code
echo "x = 42" | fortfront

# Output:
# program main
#     implicit none
#     integer :: x
#     x = 42
# end program main
```

### Character Handling

```bash
echo 'name = "hello" // " world"' | fortfront

# Output:
# program main
#     implicit none
#     character(len=11) :: name
#     name = "hello" // " world"
# end program main
```

## Building

### FMP Build System (Primary)

```bash
# Build the project
./build.sh

# Run tests (30s timeout)  
./test.sh

# Or use fpm directly with required flags
fpm build --flag "-cpp -fmax-stack-var-size=524288"
fmp test --flag "-cpp -fmax-stack-var-size=524288"
```

### CMake Build System (Secondary)

```bash
make  # Note: Currently has Fortran module path issues
```

## Project Status

**Active Development**: The project is actively maintained and under continuous improvement.

**Build Status**:
- ✅ FMP build system: Fully operational
- ⚠️ CMake system: Module path issues under investigation

**Test Status**:
- ✅ Test suite executes successfully
- ⚠️ Some test failures being addressed in ongoing development sprints

**Known Limitations**:
- Test suite has logical failures (but runs successfully)
- Some large files exceed 1000-line target (8 files, largest: 1302 lines)
- error_stop usage being migrated to proper error handling (81 remaining)

## Architecture

### Core Components

1. **Lexer** (`src/lexer/`) - Tokenizes Fortran source code
2. **Parser** (`src/parser/`) - Builds Abstract Syntax Tree (AST)  
3. **Semantic** (`src/semantic/`) - Type checking and analysis
4. **Codegen** (`src/codegen/`) - Emits standard Fortran code

### Integration with fortrun

fortrun automatically uses fortfront for `.lf` (lazy Fortran) files:

```bash
fortrun hello.lf
```

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

## Contributing

The project follows modern Fortran development practices with:
- Comprehensive test suite
- Continuous integration
- Code review process  
- Architectural sprint planning

## License

MIT License - see LICENSE file for details.