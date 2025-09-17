# CLI Profiling Baseline (2025-09-18)

Environment: local fpm build (default profile), `FORTFRONT_PROFILE=1`

| Input | Description | setup (ms) | lex:tokenize (ms) | syntax (ms) | parse (ms) | semantic (ms) | standardize (ms) | codegen (ms) | total (ms) |
|-------|-------------|-----------:|------------------:|------------:|-----------:|--------------:|-----------------:|-------------:|-----------:|
| inline snippet | `program p; x = 1` (stdin) | 105 | 0 | 0 | 0 | 0 | 0 | 0 | 105 |
| test_semicolons_simple.lf | small multi-statement sample | 109 | 0 | 0 | 0 | 0 | 0 | 0 | 109 |
| test_expression_iterative_extreme.lf | deep-nesting stress | 105 | 5 | 4 | 8 | 1 | 0 | 0 | 137 |

Notes:
- Setup includes arena reset, environment checks, and trace initialization.
- `lex:tokenize` measures the direct `tokenize_core` call; other lexer overhead remains in `setup`.
- Values <1 ms show as 0.000 due to timer resolution; repeated runs can be averaged for higher precision.
- Future runs should append to this table to track improvements.
