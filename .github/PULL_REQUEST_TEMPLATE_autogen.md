## Summary
Fix parser double-allocation of `stmt_indices` in basic-statement helpers across `do`, `if`, `array` modules, and shared basic statement module. Prevents a runtime error when encountering certain DO/WHILE bodies where a blank/non-meaningful statement or single-var declaration was short-circuited after a prior allocation.

## Changes
- Guard allocatable `stmt_indices` allocations with `if (.not. allocated(...))` in:
  - `src/parser/parser_do_constructs.f90`
  - `src/parser/parser_if_constructs.f90`
  - `src/parser/parser_array_constructs.f90`
  - `src/parser/parser_basic_statement_module.f90`

## Rationale
`fpm test` failed with a Fortran runtime error: attempting to allocate already allocated variable 'stmt_indices' in `parse_basic_stmt_local` (DO constructs). The guard avoids double-allocs while preserving behavior.

## Testing
- Ran `make test` locally; suite passes (0 exit code). Verified the previously failing target `test_ast_introspection_complete_coverage` now succeeds.

## Risks
- Low. Only affects local allocatable result preparation in basic-statement helpers. No changes to parsing semantics or public APIs.

## Checklist
- [x] Tests pass locally via `make test`
- [x] Minimal, focused changes aligned with repo guidelines
- [x] No new dependencies

