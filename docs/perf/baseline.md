# CLI Profiling Baseline (2026-01-02)

## Perf Changelog
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
