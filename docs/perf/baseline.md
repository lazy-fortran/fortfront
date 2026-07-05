# CLI Profiling Baseline (2026-01-02)

## Perf Changelog
### 2026-07-05
- Added LSP-scale full reparse benchmarks (200/1000/5000 lines) via
  `test/system/test_reparse_benchmark.f90`. Measures
  `tooling_load_ast_from_string` with `reuse_arena` to simulate debounced
  `didChange` cycles. 200L ~3 ms, 1000L ~40 ms, 5000L ~819 ms.
- Documented `didChange` budget: debounced full reparse viable up to ~800 lines
  at 100 ms budget. Beyond that, incremental parsing is required.
### 2026-01-02
- Added `FORTFRONT_PROFILE=1` timing output for CLI runs (written to stderr on
  exit), using existing `trace_enter` and `trace_leave` scope names.
- Refreshed baseline measurements against current pipeline phase names
  (`phase:lexer`, `phase:syntax`, `phase:parser`, `phase:semantic`,
  `phase:codegen`).
### 2025-09-19
- Introduced `frontend_tooling_api::tooling_load_ast_from_string`, letting
  tooling reuse the Pratt arena without semantics; cold-start latency for
  `tooling_lightweight_ast.f90` dropped by ~38% compared to the full pipeline.
- Documented `tooling_parse_options_t%reuse_arena` so downstream tools can
  recycle arenas instead of allocating per request.
### 2025-09-18
- Pratt parser is now the default path for CLI transforms and library calls,
  eliminating the recursive dispatcher and its duplicate token scans.
- `frontend_transformation` reuses a shared `compiler_arena_t`, cutting arena
  creation costs by ~35% across repeated transforms.
- Call graph construction is decoupled from the hot path and runs only when a
  consumer explicitly opts in, reducing default traversals.

## Remaining Bottlenecks
- For small `.lf` inputs, `phase:parser` and `phase:codegen` are the dominant
  phases in the profile report.
- Nested expression inputs increase `phase:semantic` time due to additional
  semantic passes and constant folding.

## Baseline Metrics

Environment: local fpm build, `FORTFRONT_PROFILE=1`

Profile output is emitted to stderr at process exit and looks like:

```
=== Fortfront Profile ===
total_ms:      1.520
phase:lexer 1 self_ms:     0.080 total_ms:     0.080
...
```

All per-phase values below use `self_ms` (exclusive time), so nested scopes do
not double-count time.

| Input | Description | phase:lexer (ms) | phase:syntax (ms) | phase:parser (ms) | phase:semantic (ms) | phase:codegen (ms) | total (ms) |
|-------|-------------|-----------------:|------------------:|------------------:|--------------------:|-------------------:|-----------:|
| inline snippet | `program p; x = 1` (stdin) | 0.065 | 0.028 | 0.129 | 0.105 | 0.118 | 1.111 |
| examples/lf/api_complex_transform.lf | function + assignments + print | 0.080 | 0.032 | 0.248 | 0.116 | 0.184 | 1.520 |
| examples/lf/issue_1238_nested_expressions.lf | nested expression sample | 0.107 | 0.046 | 0.320 | 0.265 | 0.288 | 1.885 |

## Notes
- `total_ms` is the wall time captured for the `cli:main` scope, from entry to
  exit.
- `total_ms` and per-phase times are captured via `system_clock` and rendered
  as milliseconds with millisecond precision.
- Future improvements should aim to shrink the setup cost and expose additional
  sub-stages as needed.

## LSP-Scale Full Reparse Benchmarks

Measures `tooling_load_ast_from_string` with `reuse_arena = .true.` and
`run_semantics = .false.` -- the path used by `fo` keystroke diagnostics and
LSP `didChange` handlers. Each size run 10 times; best/worst/mean reported.

Run: `fo test test_reparse_benchmark`

| Lines | Best (ms) | Worst (ms) | Mean (ms) | Within 100 ms budget |
|------:|----------:|-----------:|----------:|---------------------:|
| 200   | 3         | 4          | 4         | yes                  |
| 1000  | 40        | 84         | 60        | yes                  |
| 5000  | 819       | 1871       | 1342      | **no**               |

### didChange Budget Guideline

Debounced full reparse stays under 100 ms for files up to ~800 lines. At 1000
lines the mean (60 ms) is within budget but the worst case (84 ms) approaches
the limit. At 5000 lines the best case (819 ms) exceeds the budget by 8x.

**Recommendation:** Use debounced full reparse for files under 800 lines. For
larger files, either increase the debounce interval proportionally or implement
incremental node-level reparse.

### Benchmark Files

Canonical benchmark inputs in `examples/f90/`:
- `benchmark_200_lines.f90` -- 200 lines, ~13 functions
- `benchmark_1000_lines.f90` -- 1000 lines, ~66 functions
- `benchmark_5000_lines.f90` -- 5000 lines, ~333 functions

Each file is a valid Fortran program with a `contains` section of small
functions exercising declarations, loops, and arithmetic expressions.
