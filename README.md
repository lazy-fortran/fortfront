# FortFront

A Fortran frontend that transforms Lazy Fortran to standard-conforming Fortran.

## Features

- End-to-end pipeline: lexing, parsing, semantic checks, and Fortran emission
- Lazy Fortran to standard Fortran conversion with automatic type inference
- CLI and library APIs for scripting, pipelines, and embedding in larger tools

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
character(len=:), allocatable :: input, output
input = "x = 5"
call transform_lazy_fortran_string(input, output)
```

### Full Pipeline Example

```fortran
use lexer_api, only: tokenize_core, token_t
use parser_api, only: parse_tokens, create_compiler_arena, compiler_arena_t
use ast_api, only: ast_arena_t
use codegen_api, only: generate_code_from_arena

character(len=*), parameter :: source = "x = 5"
type(token_t), allocatable :: tokens(:)
type(compiler_arena_t) :: compiler_arena
integer :: root_index
character(len=512) :: error_msg
character(len=:), allocatable :: code

call tokenize_core(source, tokens)
compiler_arena = create_compiler_arena()
call parse_tokens(tokens, compiler_arena%ast, root_index, error_msg)
code = generate_code_from_arena(compiler_arena%ast)
```

### Documentation

- API Reference: docs/API.md
- Usage Guide with Examples: docs/LIBRARY_USAGE.md

## Links
- Fortrun integration: https://github.com/lazy-fortran/fortrun
- Examples: `examples/`
- Architecture docs: `docs/`
