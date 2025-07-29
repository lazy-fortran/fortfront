# fortfront

Core analysis frontend for lazy fortran - lexer, parser, semantic analysis, and AST operations.

## Overview

fortfront is a standalone Fortran frontend package that provides:
- Lexical analysis (tokenization)
- Parsing to Abstract Syntax Tree (AST)
- Semantic analysis with type inference
- AST transformations and standardization
- Code generation to standard Fortran

## Features

- Clean, modular architecture
- Comprehensive test suite (69/72 tests passing)
- Support for modern Fortran features
- Type inference using Hindley-Milner algorithm
- AST visitor pattern for transformations
- JSON serialization for AST and tokens

## Building

```bash
fpm build
```

## Testing

```bash
fpm test
```

## Usage

```fortran
use frontend, only: compile_source, compilation_options_t

type(compilation_options_t) :: options
character(len=:), allocatable :: code, error_msg

! Configure options
options%debug_ast = .true.
options%debug_tokens = .true.

! Compile source file
call compile_source("input.f90", options, code, error_msg)

if (error_msg == "") then
    print *, code
else
    print *, "Error: ", error_msg
end if
```

## License

MIT License - see LICENSE file for details.