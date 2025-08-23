# Static Library Integration Guide

This guide explains how to use libfortfront.a as a static library in external Fortran programs.

## Quick Start

### Building the Static Library

```bash
# Option 1: Using make
make libfortfront.a

# Option 2: Using the build script
./build_static_lib.sh
```

This creates:
- `libfortfront.a` - The static library containing all fortfront functionality
- `fortfront_modules/` - Directory containing all Fortran module files (.mod)

### Using in External Programs

```fortran
program my_tool
    use fortfront_external_interface
    use error_handling
    implicit none
    
    type(fortfront_result_t) :: result
    character(len=:), allocatable :: source_code
    
    source_code = "program test; print *, 'Hello'; end program"
    result = fortfront_transform_source(source_code)
    
    if (result%success) then
        print *, "Transformed code:"
        print *, result%output
    else
        print *, "Error:", result%error_message
    end if
end program my_tool
```

### Compilation

```bash
gfortran -I fortfront_modules/ my_tool.f90 libfortfront.a -o my_tool
```

## Available Interfaces

### Main Module: fortfront_external_interface

This module provides the primary interface for external tools:

```fortran
! Transform source code string
function fortfront_transform_source(input_source) result(result_data)
    character(len=*), intent(in) :: input_source
    type(fortfront_result_t) :: result_data
end function

! Transform with custom formatting options
function fortfront_transform_source_with_format(input_source, format_options) result(result_data)
    character(len=*), intent(in) :: input_source
    type(fortfront_format_options_t), intent(in) :: format_options
    type(fortfront_result_t) :: result_data
end function

! Compile file to output
function fortfront_compile_file(input_file, options) result(result_data)
    character(len=*), intent(in) :: input_file
    type(fortfront_compilation_options_t), intent(in) :: options
    type(fortfront_result_t) :: result_data
end function
```

### Result Type

```fortran
type :: fortfront_result_t
    logical :: success = .false.
    character(len=:), allocatable :: error_message
    character(len=:), allocatable :: output
end type
```

### Format Options

```fortran
type :: fortfront_format_options_t
    integer :: indent_size = 4
    logical :: use_tabs = .false.
    character(len=1) :: indent_char = ' '
    logical :: standardize_types = .true.
    integer :: line_length = 130
end type
```

### Compilation Options

```fortran
type :: fortfront_compilation_options_t
    logical :: debug_tokens = .false.
    logical :: debug_ast = .false.
    logical :: debug_semantic = .false.
    logical :: debug_standardize = .false.
    logical :: debug_codegen = .false.
    character(len=:), allocatable :: output_file
end type
```

## Lower-Level Modules

For advanced use cases, you can directly access internal modules:

- `lexer_core` - Tokenization
- `parser_core` - Parsing to AST
- `ast_core` - AST manipulation
- `semantic_analyzer` - Type checking and inference
- `codegen_core` - Code generation
- `error_handling` - Error management
- `scope_manager` - Scope tracking
- `type_system_unified` - Type system

## Example: Building a Custom Tool

See `examples/external_tool_example.f90` for a complete example.

To build and run:

```bash
make example
```

## Installation

To install the library system-wide:

```bash
sudo make install
```

This installs:
- `/usr/local/lib/libfortfront.a` - Static library
- `/usr/local/include/fortfront/*.mod` - Module files
- `/usr/local/lib/pkgconfig/fortfront.pc` - pkg-config file

Then compile with:

```bash
gfortran my_tool.f90 $(pkg-config --cflags --libs fortfront) -o my_tool
```

## Integration Examples

### Fluff (Linter)

```fortran
program fluff
    use fortfront_external_interface
    use semantic_analyzer
    implicit none
    
    ! Perform static analysis on Fortran code
    ! Access AST directly for custom rules
end program
```

### Fortrun (Build Tool)

```fortran
program fortrun
    use fortfront_external_interface
    use parser_core
    implicit none
    
    ! Parse modules for dependency analysis
    ! Cache compilation results
end program
```

### Custom Formatter

```fortran
program formatter
    use fortfront_external_interface
    implicit none
    
    type(fortfront_format_options_t) :: fmt_opts
    
    fmt_opts%indent_size = 2
    fmt_opts%use_tabs = .true.
    fmt_opts%line_length = 80
    
    ! Apply custom formatting
end program
```

## Requirements

- gfortran 9.0 or later (tested with 15.2.1)
- No external dependencies beyond Fortran runtime
- Supports Linux, macOS, Windows (with appropriate toolchain)

## Troubleshooting

### Module Not Found

Ensure module path is specified:
```bash
gfortran -I /path/to/fortfront_modules/ ...
```

### Undefined References

Link the static library after source files:
```bash
gfortran source.f90 libfortfront.a  # Correct
gfortran libfortfront.a source.f90  # Wrong order
```

### Stack Size Issues

For large source files, increase stack size:
```bash
gfortran -fmax-stack-var-size=65536 ...
```

## Architecture Benefits

- **Pure Fortran** - No C/C++ dependencies
- **Static Linking** - Single self-contained executable
- **Module-based** - Clean Fortran interfaces
- **Zero Runtime Dependencies** - Only Fortran stdlib required
- **Cross-platform** - Works on any system with Fortran compiler

## Support

For issues or questions about static library integration, please open an issue on GitHub.