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

### CLI
- Synopsis: `fortfront [OPTIONS] [--] [FILE]`
- If `FILE` is omitted, `fortfront` reads from stdin and writes the transformed Fortran to stdout.
- Options:
  - `-h, --help` Show help and exit 0
  - `-v, --version` Show version and exit 0
  - `--trace[=on|off]` Enable/disable internal tracing (overrides env)
  - `--trace-file <path>` Path to append trace messages (overrides env)
  - `--` End of options; treat the next token as `FILE` even if it starts with `-`

```sh
fortfront input.lf > output.f90
echo "x = 5" | fortfront > output.f90
fortfront -- -file.lf > output.f90  # filename starts with '-'
```

### Exit Codes
- Success: `0` when transformation succeeds and output is produced (including
  empty input, which yields a minimal `program main`).
- Failure: non‑zero for invalid options (e.g., unknown flags) and for specific
  syntax forms that are rejected by the parser (e.g., `func ...` shorthand).
  Diagnostics are written to `stderr`. When any partial output is produced, it
  is written to `stdout` first to preserve pipeline behavior.

## Tracing
- Tracing can be enabled via CLI flags or environment variables:
  - CLI flags (override environment):
    - `--trace[=on|off]` enable/disable tracing explicitly
    - `--trace-file <path>` set the trace output file path
  - Environment variables:
  - `FORTFRONT_TRACE`: enable when set to a truthy value (`1`, `true`, `on`, `yes`);
    falsey values (`0`, `false`, `off`, `no`) disable it.
  - `FORTFRONT_TRACE_FILE`: optional path to write trace messages; traces append to this file.
- On Windows, internal tracing is limited/disabled in some paths to avoid
  stack issues when piping.

Trace markers commonly include:
- `CLI:start`, `CLI: read input done (...)`, `CLI: transform begin/end` in the
  trace file, and `phase:*` entries from internal tracing. Use a case‑insensitive
  search when scanning logs.

Manual checks (POSIX shells):

```sh
# No trace file by default
rm -f cli_trace.txt && echo "x = 1" | fpm run --target fortfront && test ! -f cli_trace.txt && echo "no trace by default"

# Enable tracing and write to a specific file (ensure env applies to the fortfront process)
echo "x = 1" > t.lf
FORTFRONT_TRACE=1 FORTFRONT_TRACE_FILE=my_log.txt fpm run --target fortfront -- t.lf && test -f my_log.txt && echo "trace enabled via env"
rg -in "^(cli:|phase:)" my_log.txt || true
rm -f t.lf my_log.txt
```

## Links
- Fortrun project: https://github.com/lazy-fortran/fortrun
- Docs and examples are in `docs/` and `examples/`

```sh
# Build and run tests
make test
```

### Running CLI System Tests
- The CLI integration tests invoke external tools and are disabled by default.
- Enable them with environment variable `RUN_SYSTEM_TESTS=1`.

- POSIX shells:
  - `RUN_SYSTEM_TESTS=1 make test`
  - or `RUN_SYSTEM_TESTS=1 fpm test`

- Windows (PowerShell):
  - `$env:RUN_SYSTEM_TESTS=1; fpm test`

- Notes:
  - These tests build the `fortfront` CLI and exercise stdin/pipe behavior.
  - Timeouts are applied in the test harness to keep runs bounded.
  - Regular `make test` (without the env var) runs the fast unit/integration suite only.
