# FortFront

A Fortran frontend that transforms Lazy Fortran to standard-conforming Fortran.

## Lazy Fortran vs Standard Fortran

Lazy Fortran omits boilerplate that fortfront infers automatically:

- **Type declarations**: `x = 5` becomes `integer :: x` with assignment
- **Function return types**: `function add(a, b)` infers types from usage
- **Array bounds**: automatic shape inference for allocatable arrays
- **Program structure**: wraps bare statements in `program main ... end program`

Example transformation:
```fortran
! input.lf (Lazy Fortran)
function add(a, b)
    result = a + b
end function
x = add(5, 3)

! output.f90 (Standard Fortran)
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
# or
make
```

## Usage

### File mode
```sh
fortfront input.lf > output.f90
```

### Stdin mode
```sh
echo "x = 5" | fortfront > output.f90
```

### Options
- `-h, --help` Show help
- `-v, --version` Show version
- `--trace[=on|off]` Enable/disable tracing
- `--trace-file <path>` Trace output path
- `--` End of options (for filenames starting with `-`)

## Library API

Main entry points from the `frontend` module:

- `transform_lazy_fortran_string(input, output)` - High-level transformation
- `compile_source(input, options)` - Full compilation pipeline
- `lex_source(input, tokens)` - Lexical analysis only
- `parse_tokens(tokens, ast)` - Parsing only
- `analyze_semantics(ast)` - Semantic analysis only
- `emit_fortran(ast, output)` - Code generation only

Example:
```fortran
use frontend, only: transform_lazy_fortran_string
character(len=:), allocatable :: input, output
input = "x = 5"
call transform_lazy_fortran_string(input, output)
```

## Links
- Fortrun integration: https://github.com/lazy-fortran/fortrun
- Examples: `examples/`
- Architecture docs: `docs/`
