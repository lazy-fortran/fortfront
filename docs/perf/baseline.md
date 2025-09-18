# CLI Profiling Baseline (2025-09-18)

Environment: local fpm build (default profile), `FORTFRONT_PROFILE=1`

| Input | Description | setup:arena (ms) | lex:tokenize (ms) | phase:syntax (ms) | parser:parse_tokens (ms) | final:semantic (ms) | total (ms) |
|-------|-------------|-----------------:|------------------:|------------------:|------------------------:|--------------------:|-----------:|
| inline snippet | `program p; x = 1` (stdin) | 109 | 0 | 0 | 0 | 1 | 108 |
| test_semicolons_simple.lf | small multi-statement sample | 110 | 0 | 0 | 0 | 0 | 110 |
| test_expression_iterative_extreme.lf | deep-nesting stress | 110 | 5 | 3 | 8 | 0 | 126 |

Notes:
- `setup:*` captures trace initialization, env queries, codegen initialization, and arena reset (currently dominating cost).
- `total` is measured from entry to exit; sub-stage times may not sum to total because only selected stages are recorded.
- Values are rendered with microsecond precision (milliseconds with six decimals) using the high-resolution `system_clock` counter.
- Future improvements should aim to shrink the setup cost and expose additional sub-stages as needed.
