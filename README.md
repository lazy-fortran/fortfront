# FortFront

A small Fortran frontend that lexes, parses, performs light semantic checks, and emits standard-conforming Fortran. It is designed for simple, copy‑pasteable workflows and clear examples.

## Features
- Clean pipeline: lex → parse → semantic → codegen
- Intrinsic recognition and lightweight type inference
- Optional standardization step for consistent output
- Lean analysis hooks: direct call-graph/CFG APIs without legacy wrappers

## Building
- fpm: `fpm build`
- make: `make`

## Usage
- Transform Lazy Fortran to standard Fortran:
  - CLI/tools are demonstrated in tests and examples
  - Library API: see `frontend` module (e.g., `transform_lazy_fortran_string`, `compile_source`)

## Links
- Fortrun project: https://github.com/lazy-fortran/fortrun
- Docs and examples are in `docs/` and `examples/`

```sh
# Build and run tests
make test
```
