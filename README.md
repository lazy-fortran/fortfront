# FortFront

A small Fortran frontend that lexes, parses, performs light semantic checks, and emits standard-conforming Fortran. It is designed for simple, copy‑pasteable workflows and clear examples.

## Features
- Clean pipeline: lex → parse → semantic → codegen
- Intrinsic recognition and lightweight type inference
- Optional standardization step for consistent output
- Minimal analysis hooks: direct call-graph API without extra wrappers

## Building
- fpm: `fpm build`
- make: `make`

## Usage
- Transform Lazy Fortran to standard Fortran:
  - CLI/tools are demonstrated in tests and examples
  - Library API: see `frontend` module (e.g., `transform_lazy_fortran_string`, `compile_source`)

## Tracing
- CLI and library tracing are opt-in via environment variables:
  - `FORTFRONT_TRACE`: enable when set to a truthy value (`1`, `true`, `on`, `yes`);
    falsey values (`0`, `false`, `off`, `no`) disable it.
  - `FORTFRONT_TRACE_FILE`: optional path to write trace messages.
- On Windows, internal tracing is disabled to avoid stack issues when piping.

## Links
- Fortrun project: https://github.com/lazy-fortran/fortrun
- Docs and examples are in `docs/` and `examples/`

```sh
# Build and run tests
make test
```
