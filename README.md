# FortFront

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
make
```

## Usage

```sh
fortfront input.lf > output.f90
echo "x = 5" | fortfront > output.f90
```

### Options
- `-h, --help` Show help
- `-v, --version` Show version
- `--trace[=on|off]` Enable/disable tracing
- `--trace-file <path>` Trace output path
- `--` End of options (for filenames starting with `-`)

## Library API

FortFront provides a modular API for integration into downstream tools such as linters, compilers, and formatters.

### API Modules

- `lexer_api` - Tokenization and lexical analysis
- `parser_api` - Token parsing and AST construction
- `ast_api` - AST node types and traversal utilities
- `semantic_api` - Type inference and semantic validation
- `codegen_api` - Standard Fortran code generation
- `error_api` - Error handling and reporting
- `transformation_api` - High-level transformation pipeline
- `frontend_tooling_api` - Convenience functions for tool developers

### Quick Example

```fortran
use transformation_api, only: transform_lazy_fortran_string
character(len=:), allocatable :: input, output, error_msg
input = "x = 5"
call transform_lazy_fortran_string(input, output, error_msg)
if (len(error_msg) == 0) then
    print '(a)', output
else
    print '(a)', 'Transformation failed: ' // error_msg
end if
```

### Documentation
See docs/API.md for the full API reference and docs/LIBRARY_USAGE.md for worked examples.

## Links
- Fortrun integration: https://github.com/lazy-fortran/fortrun
- Examples: `examples/`
- Architecture docs: `docs/`
