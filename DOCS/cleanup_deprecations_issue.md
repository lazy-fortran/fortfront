# Deprecation Cleanup Roadmap: Remove Umbrellas, Legacy Arenas, Stale Docs/Tests

Goal: reduce compile time, speed up tests, and remove bloat by eliminating
deprecated/legacy layers (umbrella modules, legacy arenas, compatibility
shims), removing stale documentation, and pruning or updating related tests.

This issue proposes a concrete, staged plan with checklists, file-level scope,
verification commands, and acceptance criteria.

## Objectives
- Eliminate umbrella re-export modules that expand compile dependencies:
  - Replace `use ast_core` with scoped imports everywhere.
  - Minimize or deprecate top-level `frontend` re-export module.
- Remove legacy arena artifacts not used by the build:
  - Delete `src/ast/arena/ast_arena.f90` (legacy).
  - Evaluate `ast_arena_compat` usage; migrate callers to core or modern API.
- Prune stale docs and internal analyses; consolidate to a minimal set.
- Prune or update tests that cover deleted features/paths.
- Measure and document compile/test time improvements.

## Current State (as of branch fix-1286-parser-binary-guard)
- Parser: completed — all `use ast_core` removed and imports scoped.
- Other subsystems still rely on `ast_core`:
  - codegen/, semantic/, standardizers/, analysis/, some ast/ modules.
  - Quick scan: `rg -n "^\s*use\s+ast_core\b" src | wc -l` shows many hits.
- Modern arena in use (`ast_arena_modern`); legacy `ast_arena` file still
  present but unused by build and not imported.
- Documentation includes multiple migration and analysis documents that predate
  recent refactors.

## Proposed Plan

### Phase 1 — Remove legacy arena code (safe delete)
- Delete file: `src/ast/arena/ast_arena.f90`.
- Keep `ast_arena_modern` (current) and its core/compat units.
- Verification:
  - `rg -n "^\s*use\s+ast_arena\b" src` returns no results.
  - `fpm build && fpm test` passes on Linux and Windows (CI).
- Acceptance:
  - No compile references to legacy arena remain.
  - CI green on both platforms.

### Phase 2 — Purge umbrella re-exports (ast_core) usage
- Replace `use ast_core` with scoped imports across these trees:
  - `src/codegen/`: codegen_* modules
  - `src/semantic/`: analyzers, core, query API
  - `src/standardizers/`: all standardizer_* modules
  - `src/analysis/`: analysis/* utilities
  - `src/utilities/` (if any)
  - `src/ast/` modules that import their own umbrella
- For each file, import only:
  - exact node types (`ast_nodes_*.f90`),
  - `ast_arena_modern::ast_arena_t`,
  - specific factory functions used (`ast_factory_*`),
  - literal kinds from `ast_types`.
- Keep `ast_core` only where constructors live (e.g., `create_comment`,
  `create_blank_line`) until those factories are surfaced in narrower modules.
- Verification:
  - `rg -n "^\s*use\s+ast_core\b" src | wc -l` trends to 0.
  - Build/tests pass locally and in CI after each subdir batch.
- Acceptance:
  - 0 files in src/ import `ast_core`, except possibly a small whitelist that
    still hosts constructors (tracked below under Phase 4).

### Phase 3 — Consolidate/trim `frontend` umbrella
- `src/frontend.f90` currently re-exports high-level API for backward
  compatibility.
- Options:
  1) Keep `frontend` as stable public API but restrict its `use` set to top
     entry points only (done already); document it as the public surface.
  2) Deprecate `frontend` in favor of direct imports (`frontend_*` modules);
     emit deprecation note in release notes and adjust internal callers.
- Decision needed: keep or deprecate. If deprecated:
  - Add a deprecation banner to `src/frontend.f90`.
  - Update internal imports to direct modules.
- Acceptance:
  - If kept: it exports only intended public API.
  - If deprecated: no internal module uses `frontend`.

### Phase 4 — Extract remaining constructors from ast_core
- Some constructors (e.g., `create_comment`, `create_blank_line`) are still in
  `ast_core` (ref: `src/ast/ast_core.f90:520,531`).
- Action:
  - Move those constructors into their own narrow modules, e.g.,
    `ast_nodes_misc_constructors.f90`, or expose them through
    `ast_nodes_misc` directly.
  - Update all imports to use the narrow module; remove `ast_core` from import
    sites such as `src/parser/parser_dispatcher.f90`.
- Acceptance:
  - `rg -n "create_comment|create_blank_line" src` shows no usage via ast_core.

### Phase 5 — Reduce or remove `ast_arena_compat` layer
- `ast_arena_modern` currently extends a compatibility type; cost: extra
  fields/method calls and rebuild churn.
- Action:
  - Identify entry points using compat-only surface (e.g., direct `push` of
    class(ast_node)).
  - Swap to core handle-based API where feasible, or keep compatibility only in
    `ast_factory_*` surfaces to isolate cost.
- Verification:
  - `rg -n "ast_arena_compat_t|compat_size|entries\(" src` to locate usage.
  - No hot paths depend on compat-only fields; factories can hide the change.
- Acceptance:
  - Measurable compile time improvements (see Benchmarks) and no functionality
    loss. This phase can be incremental per package.

### Phase 6 — Documentation cleanup (minimal docs goal)
- Remove or archive internal analysis docs that are obsolete or superseded:
  - Candidates to delete: 
    - `DOCS/AST_MIGRATION_ANALYSIS.md`
    - `DOCS/AST_ARENA_ARCHITECTURE_ANALYSIS.md`
    - `DOCS/AST_ARENA_MIGRATION_GUIDE.md`
    - `DOCS/TODO_AUDIT_ANALYSIS.md`
    - `DOCS/MOVE_ALLOC_PERFORMANCE_ANALYSIS.md`
    - `docs/CODEGEN_ARCHITECTURE_ANALYSIS.md`
  - Keep minimal, user-facing docs and a short DEV_NOTES.md with current
    design pointers.
- Update remaining docs to remove references to deprecated modules (e.g.,
  `ast_core` “god module” notes), or keep a brief deprecation summary.
- Acceptance:
  - Only succinct, useful documentation remains; no stale migration analyses.

### Phase 7 — Tests review and pruning
- Remove tests targeting deleted features (already removed legacy slow-path
  tests in PR #1287). Audit for similar cases.
- Candidates:
  - Any tests that rely on umbrella imports indirectly.
  - Tests around legacy arena APIs (none currently found in test/ but confirm).
- Add regression tests for narrow modules if constructors are moved (Phase 4).
- Acceptance:
  - Test suite remains green; total test time improves or remains stable.

## Benchmarks & Measurement
- Add a simple timing script (make targets):
  - `make clean && time make` for build time.
  - `make clean && time make test` for end-to-end time.
- Record baseline on main and after each phase.
- Acceptance: show reduction in compile graph (fewer files rebuilt on small
  changes) and faster local CI runs.

## Risks
- Large refactors may cause subtle import/visibility regressions.
- Moving constructors out of `ast_core` requires careful dependency untangling.
- Windows build differences; verify CI on Windows and Linux after each phase.

## Rollout Strategy
- Land phases as separate PRs; keep changesets focused per directory to ease
  review.
- Require green CI and compile/test time snapshot updates per PR.

## Initial Checklists

- [ ] Phase 1: Delete `src/ast/arena/ast_arena.f90`; CI green
- [ ] Phase 2: Remove `use ast_core` from:
  - [ ] src/codegen/*
  - [ ] src/semantic/*
  - [ ] src/standardizers/*
  - [ ] src/analysis/*
  - [ ] src/ast/* (self-imports)
- [ ] Phase 3: Decide on `frontend` deprecation vs keep; implement
- [ ] Phase 4: Move comment/blank-line constructors out of `ast_core` and fix imports
- [ ] Phase 5: Reduce `ast_arena_compat` usage in hot paths
- [ ] Phase 6: Prune stale docs; leave minimal, current docs
- [ ] Phase 7: Prune/adjust tests; keep suite fast and behavioral

## Useful Greps (for verification)
- Umbrella imports: `rg -n "^\s*use\s+ast_core\b" src`
- Legacy arena use: `rg -n "^\s*use\s+ast_arena\b" src`
- Constructors to move: `rg -n "create_comment|create_blank_line" src`
- Compat hotspots: `rg -n "ast_arena_compat_t|compat_size|entries\(" src`

