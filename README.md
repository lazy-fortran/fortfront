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

### Exit Codes
- Success: `0` when transformation succeeds and output is produced.
- Failure: non‑zero when a syntax/parser/validation error occurs or when no output
  is generated. Diagnostics are written to `stderr`. If any partial output was
  produced, it is written to `stdout` before the error message to preserve
  pipeline behavior.

## Tracing
- CLI and library tracing are opt-in via environment variables:
  - `FORTFRONT_TRACE`: enable when set to a truthy value (`1`, `true`, `on`, `yes`);
    falsey values (`0`, `false`, `off`, `no`) disable it.
  - `FORTFRONT_TRACE_FILE`: optional path to write trace messages; traces append to this file.
- On Windows, internal tracing is disabled to avoid stack issues when piping.

Trace markers commonly include:
- `cli:main`, `cli:read_input`, `cli:transform`
- `phase:lexer`, `phase:syntax`, `phase:parser`, `phase:final`

Manual checks (POSIX shells):

```sh
# No trace file by default
rm -f cli_trace.txt && echo "x = 1" | fpm run --quiet --target fortfront && test ! -f cli_trace.txt && echo "no trace by default"

# Enable tracing and write to a specific file (ensure env applies to the fortfront process)
echo "x = 1" > t.lf
FORTFRONT_TRACE=1 FORTFRONT_TRACE_FILE=my_log.txt fpm run --quiet --target fortfront -- t.lf && test -f my_log.txt && echo "trace enabled via env"
rm -f t.lf my_log.txt
```

## Links
- Fortrun project: https://github.com/lazy-fortran/fortrun
- Docs and examples are in `docs/` and `examples/`

```sh
# Build and run tests
make test
```
