# FortFront

> **Note**: This project is experimental. The main implementation of a Fortran variant for "lazy" developers is now in [LFortran](https://github.com/lfortran/lfortran) via its interactive mode and planned `infer` mode.

A Fortran frontend that transforms Lazy Fortran to standard-conforming Fortran.

## Features

- End-to-end pipeline: lexing, parsing, semantic checks, and Fortran emission
- Lazy Fortran to standard Fortran conversion with automatic type inference
- CLI and library APIs for scripting, pipelines, and embedding in larger tools

## Lazy Fortran vs Standard Fortran

Lazy Fortran omits boilerplate that fortfront infers automatically:

- **Type declarations**: `x = 5` becomes `integer :: x`
- **Function return types**: inferred from usage
- **Array bounds**: automatic shape inference for allocatables
- **Program structure**: wraps bare statements in `program main ... end program`

Example transformation:
```fortran
! input.lf
function add(a, b)
    result = a + b
end function
x = add(5, 3)

! output.f90
program main
    implicit none
    integer :: x
contains
    function add(a, b)
        implicit none
        integer, intent(in) :: a, b
        integer :: add
        add = a + b
    end function
    x = add(5, 3)
end program
```

## Building
```sh
fpm build
make clean  # Remove build artifacts, logs, .mod/.o/.a files
```

## Usage

```sh
fortfront input.lf > output.f90
echo "x = 5" | fortfront > output.f90
```

### Options
- `-h, --help` Show help
- `-v, --version` Show version
- `--infer` Infer mode (default): accepts top-level statements and infers missing structure
- `--std=lf`, `--std lf` Strict mode: require explicit `program`/`module`/procedure units (rejects bare statements)
- `--trace[=on|off]` Enable/disable tracing
- `--trace-file <path>` Trace output path
- `--` End of options (for filenames starting with `-`)

## Library API

FortFront provides a modular API for integration into downstream tools such as linters, compilers, and formatters.

### API Modules

- `fortfront_lexer` - Tokenization and lexical analysis
- `fortfront_parser` - Token parsing and AST construction
- `fortfront_ast` - AST node types and traversal utilities
- `fortfront_semantic` - Type inference and semantic validation
- `fortfront_codegen` - Standard Fortran code generation
- `fortfront_error` - Error handling and reporting
- `fortfront_transform` - High-level transformation pipeline
- `fortfront_tooling` - Convenience functions for tool developers

### Quick Example

```fortran
use fortfront_transform, only: transform_lazy_fortran_string
character(len=:), allocatable :: input, output, error_msg
input = "x = 5"
call transform_lazy_fortran_string(input, output, error_msg)
if (len(error_msg) == 0) then
    print '(a)', output
else
    print '(a)', 'Transformation failed: ' // error_msg
end if
```

See docs/LIBRARY_USAGE.md for worked examples (API spec tracked in issue #1642).

## Links
- [fortrun](https://github.com/lazy-fortran/fortrun) | [examples/](examples/) | [docs/](docs/)
