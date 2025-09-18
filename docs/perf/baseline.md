# CLI Profiling Baseline (2025-09-18)

## Perf Changelog
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
- `setup:*` stages still dominate end-to-end time because `initialize_codegen`
  loads formatting tables on every run.
- Deep Pratt ranges allocate temporary slices while materializing array
  literals; profiling shows transient spikes that merit a dedicated scratch pool.
- Semantic strict-mode toggles still rebuild type environments; threading the
  flag earlier would avoid the extra copy.
- Lightweight tooling API still copies entire files into memory; streaming
  reads and shared token buffers remain follow-up items.

## Baseline Metrics

Environment: local fpm build (default profile), `FORTFRONT_PROFILE=1`

| Input | Description | setup:arena (ms) | lex:tokenize (ms) | phase:syntax (ms) | parser:parse_tokens (ms) | final:semantic (ms) | total (ms) |
|-------|-------------|-----------------:|------------------:|------------------:|------------------------:|--------------------:|-----------:|
| inline snippet | `program p; x = 1` (stdin) | 109 | 0 | 0 | 0 | 1 | 108 |
| test_semicolons_simple.lf | small multi-statement sample | 110 | 0 | 0 | 0 | 0 | 110 |
| test_expression_iterative_extreme.lf | deep-nesting stress | 110 | 5 | 3 | 8 | 0 | 126 |

## Notes
- `setup:*` captures trace initialization, env queries, codegen initialization,
  and arena reset (currently dominating cost).
- `total` is measured from entry to exit; sub-stage times may not sum to total
  because only selected stages are recorded.
- Values are rendered with microsecond precision (milliseconds with six
  decimals) using the high-resolution `system_clock` counter.
- Future improvements should aim to shrink the setup cost and expose additional
  sub-stages as needed.
