# FortFront

> **Note**: This project is experimental. The main implementation of a Fortran variant for "lazy" developers is now in [LFortran](https://github.com/lfortran/lfortran) via its interactive mode and planned `infer` mode.

A Fortran frontend that transforms Lazy Fortran to standard-conforming
Fortran and exposes AST/tooling APIs for downstream tools.

FortFront is not a complete compiler backend. It does not emit backend IR,
objects, or executables. Compiler work belongs in downstream drivers such as
`ffc`, whose current direction is LIRIC.

## Features

- End-to-end pipeline: lexing, parsing, semantic checks, and Fortran emission
- Lazy Fortran to standard Fortran conversion with automatic type inference
- CLI and library APIs for scripting, pipelines, and embedding in larger tools

Current limits:

- Cross-file inference and specialization are not implemented here.
- The C API is a small validation/status bridge, not a full AST or semantic API.
- Nested internal procedures and some newer/legacy Fortran constructs remain
  unsupported.
- The compiler-facing API should be improved before a native backend consumes
  FortFront directly.

## Lazy Fortran vs Standard Fortran

Lazy Fortran omits boilerplate that fortfront infers automatically:

- **Type declarations**: `x = 5` becomes `integer :: x`
- **Function return types**: inferred from usage
- **Array bounds**: automatic shape inference for allocatables
- **Program structure**: wraps bare statements in `program main ... end program`

Example:
```fortran
! input.lf
function add(a, b)
    result = a + b
end function
x = add(5, 3)
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

FortFront provides a modular API for integration into downstream tools such as
linters and formatters. Compiler drivers can use the current Fortran APIs, but
the stable compiler-facing contract is still a roadmap item.

### API Modules

- `fortfront_lexer` - Tokenization and lexical analysis
- `fortfront_parser` - Token parsing and AST construction
- `fortfront_ast` - AST node types and traversal utilities
- `fortfront_semantic` - Type inference and semantic validation
- `fortfront_codegen` - Standard Fortran code generation
- `fortfront_transform` - High-level transformation pipeline
- `fortfront_tooling` - Convenience functions for tool developers

See docs/guides/LIBRARY_USAGE.md for worked examples.

## Links
- [ffc](https://github.com/lazy-fortran/ffc) | [fluff](https://github.com/lazy-fortran/fluff) | [fortrun](https://github.com/lazy-fortran/fortrun) | [examples/](examples/) | [docs/](docs/)
