# CLI Profiling Baseline (2025-09-18)

Environment: local fpm build (default profile), `FORTFRONT_PROFILE=1`

| Input | Description | phase:lexer (ms) | phase:syntax (ms) | phase:parser (ms) | phase:final (ms) | total (ms) |
|-------|-------------|-----------------|-------------------|-------------------|-----------------|-----------|
| inline snippet | `program p; x = 1` (stdin) | 107 | 0 | 0 | 0 | 107 |
| test_semicolons_simple.lf | small multi-statement sample | 108 | 0 | 0 | 0 | 108 |
| test_expression_iterative_extreme.lf | deep-nesting stress | 113 | 4 | 8 | 0 | 125 |

Notes:
- Timings reflect `system_clock` millisecond conversion; resolution limited by platform clock.
- `phase:final` currently dominated by standardization/codegen; negligible for these samples.
- Future runs should append to this table to track improvements.
