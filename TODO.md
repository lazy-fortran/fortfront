# Current Work: Declaration Standardizer Bugfix

## Status
- [x] Preserve `intent`/kind attributes when standardizer regenerates declarations (currently loses `real(8)` on dummy args).
- [x] Skip auto-generated declarations for parameters collected during assignment inference (still emitting duplicates).
- [x] Normalize existing parameter declarations (multi-variable and single) before new declarations are inserted.

## Immediate Plan
1. ✅ Track parameter names (and their intent/optional metadata) when traversing subprogram bodies, and propagate that context into `collect_assignment_vars`/`add_variable` so parameters never get re-added.
2. ✅ Update `create_declaration_nodes` to short-circuit if the candidate name is a parameter, and add tight unit coverage around `test_multi_param_intent` to confirm kind specifiers survive.
3. ✅ Extend `standardize_subprograms` handling to normalize multi-variable parameter declarations in situ (set `intent`, upgrade `real`→`real(8)` when enabled) before the declaration generator runs.
4. ✅ Once logic is stable, remove debug logging, run full `make test`, and re-check CLI transformations on sample inputs.

# Expression Parser Modernization (2025 Roadmap)

## Goals
- Replace the recursive descent expression parser with a table-driven, stack-safe Pratt parser.
- Eliminate legacy helpers and duplicated traversal logic across parser, standardizer, and validation layers.
- Align memory usage with arena slabs to guarantee deterministic stack consumption on all platforms (Windows pipe CI foremost).
- Maintain or improve throughput across existing system/regression suites.

## Work Breakdown

1. **Legacy Audit & Removal**
   - [x] Scan parser, standardizer, and utilities for unused recursive helpers (`parse_*`, `traverse_*`, legacy validation shims). (2025-09-18, commit d9a59db, perf n/a)
     Audit found no unused helpers in parser/standardizer/utilities; removed stale semantic interfaces exposed by the scan.
   - [ ] Drop stale tests/docs covering removed behaviour; replace with canonical SOT parser expectations.

2. **Iterative Pratt Core**
   - [x] Design operator tables (binding powers, prefix/infix handlers).
   - [x] Allocate contiguous SoA token views for parser hot paths.
   - [x] Implement iterative Pratt loop with explicit operand/operator stacks (spill to arena when exceeding on-stack capacity).
   - [x] Integrate unary/postfix handling without recursion (component access, array subscripts, call dispatch).

3. **Pipeline Integration**
   - [ ] Wire new parser through `frontend_parsing` and CLI transform; remove compatibility glue.
   - [ ] Update validation to share token buffers; delete redundant rescan code.
   - [ ] Ensure AST emission stays stable; adjust standardizer/type utilities as needed.

4. **Verification & Performance**
   - [x] Expand deep-expression regression tests (Windows + low-stack Linux) to guard against regressions.
   - [x] Run full `RUN_SYSTEM_TESTS=1` suites; capture perf baselines.
   - [ ] Document final architecture in `DOCS/` and remove obsolete notes.

## Acceptance Criteria
- No recursive expression parsing remains in the tree.
- CLI + system tests pass on Windows CI without stack reserve hacks.
- Deep nested expression regression (≥4k nesting) parses successfully.
- Perf delta within ±5% on representative workloads.

# Stack-Safe Traversal & CFG Overhaul

## Status (2025-09-18)
- [x] Variable usage collector and expression visitor now run on explicit stacks (no recursion). (2025-09-18, commit 2ae3bf4, perf n/a)
- [x] Call graph AST traversal and cycle detection converted to iterative algorithms. (2025-09-18, commit 2ae3bf4, perf n/a)
- [x] AST traversal helpers (`traverse_preorder`/`traverse_postorder`) rewritten to explicit stack walkers; existing public entry points now iterate internally. (2025-09-18, commit f5d10d3, perf n/a)
- [x] Removed CFG builder/analyzer stack and associated plugins/tests; call-graph utilities are the only remaining analysis path (2025-09-18, commits f3aebec, db29f53, 575881a).
- [x] Audit standardizer/semantic helpers (e.g., `standardize_ast`) for residual recursion and convert to shared iterative utilities where warranted. (2025-09-18)

## Challenges
- Ensure remaining standardizer helpers adopt the shared iterative traversal stack (no hidden recursion).
- Sweep documentation/tests to remove references to deleted CFG/semantic analyzer infrastructure.
- Confirm the lean call-graph API is the only exported analysis surface and is adequately documented.

## Plan of Record
1. **Standardizer & Semantic Cleanup**
   - [x] Catalogue remaining recursive helpers in standardizer modules (`standardize_ast`, declaration visitors); convert to the iterative stack pattern when feasible or explain exceptions. Remaining recursion limited to type-string conversion (`get_fortran_type_string`).
   - [x] Remove unused tracker structures from semantic context (already dropped in code) and update query APIs/tests accordingly. (2025-09-18)

2. **Documentation & Test Sweep**
   - [ ] Rewrite or delete docs referencing removed CFG/semantic analyzer pipelines (e.g., `CODEGEN_ARCHITECTURE_ANALYSIS` mentions).
   - [ ] Trim integration tests that assume analyzer pipelines (e.g., “complete pipeline integration”) or update messaging to reflect the lean build.

3. **Lean Analysis Surface**
   - [x] Remove analyzer factory/pipeline infrastructure; keep single HM path (2025-09-18, commit 575881a).
   - [ ] Provide updated tooling guidance (fortfc/fluff) describing how to obtain call-graph data via the simplified API.

4. **Validation & Rollout**
   - [ ] Maintain tests green (`make test`) after each pruning pass; track any required fixture updates.
   - [ ] Capture perf snapshots post-cleanup to confirm there is no regression in CLI timings.
   - [ ] Refresh architecture docs summarising the minimal traversal + call-graph setup, including limitations.

## Notes
- Keep progress logged here after each milestone (design complete, prototype merged, etc.).
- Landing order should minimise risk: finish CFG builder before touching analyzer/standardizer layers so downstream work can assume the new API.
- Reuse the existing explicit stacks (variable usage, call graph) as references for frame handling and guard patterns (visited sets, capacity growth).

# Performance-Driven Frontend Streamlining

## Rationale
- The CLI now runs a single HM path; remaining work focuses on ensuring that path stays lean and well-documented.
- Call-graph utilities remain for tooling; other analyzers/pipelines have been excised. We should confirm the surface area stays minimal and well tested.
- Dynamic string-heavy data structures and repeated arena traversals still warrant attention for performance.

## Guiding Principles
1. **Single-Pass CLI Pipeline** – Lex → parse → HM type inference → standardise/codegen. Optional analyzers must be disable-able at build time.
2. **Arena-Centric Data** – Keep AST/CST ownership in arenas; avoid per-pass allocations and dynamic strings where intern tables suffice.
3. **Feature Flags** – Analyzer/CFG/call-graph tooling should be pluggable so production builds can drop them entirely.
4. **Metrics First** – Establish timing/memory baselines before/after each change to ensure real gains.

## Work Breakdown

1. **Baseline & Instrumentation**
   - [x] Add lightweight timers around CLI stages (setup, lexer, parser, semantics, codegen) gated by `FORTFRONT_PROFILE=1` with nested stage support.
   - [x] Capture current wall-clock timings on representative inputs (small, medium, stress) and store results in `docs/perf/baseline.md`.
   - _2025-09-18_: Baseline timings recorded; setup time is dominated by arena creation/reset. Allocation tracking remains future work.

2. **Lean Call Graph Utilities**
   - [ ] Merge remaining builder helpers into `call_graph_module`; expose only the minimal API used by consumers.
   - [ ] Replace string-based scope tracking with arena indices to reduce allocation.
   - [ ] Re-baseline call-graph tests to cover the simplified implementation.

3. **Semantic Path Optimisation**
   - [ ] Profile HM inference to identify hot allocators; migrate temporary structures to arena-backed slabs / interning tables.
   - [ ] Confirm `frontend_transformation` performs only the required passes for CLI transforms; remove stale hooks.
   - [ ] Document how tooling can request call-graph data without triggering extra passes.

5. **Identifier Interning & SoA Refactor**
   - [ ] Introduce a global identifier table (string interning) so AST/semantic phases store integer handles.
   - [ ] Audit modules that pass raw `character(:)` (e.g., variable usage, call graph) and convert them to ID-based lookups.
   - [ ] Measure memory/time improvements post-refactor.

6. **CST/AST Export Interface**
   - [ ] Define lightweight API to expose CST/AST handles to downstream tools (fortfc/fluff) without triggering the full CLI pipeline.
   - [ ] Provide examples/tests showing direct AST retrieval and serialization performance.

7. **Documentation & Rollout**
   - [ ] Update architecture notes with the streamlined pipeline and feature toggles.
   - [ ] Maintain a changelog summarising perf wins and remaining bottlenecks for future sessions.

## Acceptance Criteria
- CLI translation path performs no unnecessary analyzer passes and is measurably faster (target ≥20% speedup on current benchmarks).
- Memory usage becomes predictable (no large spikes from dynamic strings or recursive depth stacks).
- Downstream tooling APIs can request AST/CST/analysis results independently.
- TODO.md and `docs/perf/baseline.md` stay updated with progress logged per milestone.

# Codebase Simplification Roadmap

## Keep & Optimise
- **Lexer/Parser Core** (`src/lexer/*`, `src/parser/*`, `src/frontend_parsing.f90`): central to Lazy Fortran support; continue investing in SoA data structures, iterative parsing, and token interning.
- **Hindley–Milner Type Inference** (`src/semantic/types`, `src/semantic/semantic_analyzer.f90`): maintain as the canonical inference engine; profile and micro-optimise but preserve feature completeness.
- **Arena Infrastructure** (`src/memory/compiler_arena.f90`, `src/ast/arena_*`): crucial for deterministic lifetime and performance; keep focused on contiguous slabs and low-overhead reset semantics.
- **Standardiser & Codegen** (`src/standardizers/*`, `src/codegen/*`): required for CLI and future backend integrations; refactor for performance but retain behaviour.
- **API Boundaries** (`src/interfaces/fortfront_*`): necessary for tooling integrations; ensure they expose upcoming AST/CST handles cleanly.

## Simplify / Replace
- **CFG Builder & Control Flow Analyzer** (`src/analysis/cfg_builder_*`, `src/semantic/analyzers/control_flow_analyzer_plugin.f90`): refactor into iterative, frame-based engines with optional execution. Consider merging analyser output into lightweight summaries to avoid deep recursion and per-node allocations.
- **Call Graph Infrastructure** (`src/analysis/call_graph*.f90`, `src/analysis/call_graph_builder.f90`): unify into a single high-performance module using interned identifiers and arena indices. Drop redundant symbol tables once unified.
- **AST Traversal Utilities** (`src/ast/traversal/*.f90`, `src/utilities/fortfront_utils.f90`): replace recursive visitors with shared iterative helpers; rationalise duplicated traversal code across modules.
- **Semantic Pipeline** (dual path in `frontend_transformation.f90` calling both legacy analyzers and pipeline analyzers): collapse to one HM pipeline with feature toggles for optional analyses.
- **Identifier Handling** (multiple modules storing `character(:)` names): implement global interning and propagate integer identifiers to reduce string churn.

## Remove / Gate
- **Performance Analyzer Plugin & Metrics** (`src/semantic/analyzers/control_flow_analyzer_plugin.f90`, `src/performance/*`): behind feature flags; exclude from default CLI builds to avoid overhead.
- [x] Legacy Call Graph Analysis layer removed (2025-09-18, commit 8faf2a1); fortfront now re-exports call graph APIs directly without intermediary modules.
- **Excessive Tracing & Logging** (`src/utilities/debug_trace.f90` and pervasive trace hooks): keep minimal instrumentation, but disable or compile out verbose tracing in release builds.
- **Redundant Docs/Modules**: retire outdated TODO files or migration guides once new architecture is documented (e.g., `DOCS/AST_MIGRATION_ANALYSIS.md` if superseded).

## Action Plan
1. **Audit & Tag**
   - [ ] Annotate modules with `! @slow-path` or similar to flag opt-in analyzers; update build system to conditionally compile.
   - [ ] Catalogue obsolete docs/code paths for removal after refactors.

2. **Iterative Refactors**
   - [ ] Execute CFG/control-flow redesign per above Stack-Safe roadmap.
   - [ ] Merge call graph implementations and remove legacy analysis module.
   - [ ] Introduce identifier interning and propagate to dependent modules.

3. **Pipeline Slimming**
   - [ ] Update CLI path to skip optional analyzers; measure perf gains.
   - [ ] Adjust APIs so fortfc/fluff can request data without triggering extra passes.

4. **Cleanup & Documentation**
   - [ ] Delete deprecated modules/docs after replacements stabilize.
   - [ ] Record updated architecture diagrams and performance figures.

## Logging
- Update this TODO after each completed subtask with date, commit reference, and perf delta where applicable.
- 2025-09-18 (2ae3bf4, perf n/a): Converted call graph traversal and variable usage walkers to
  iterative stacks to eliminate recursion on analysis hot paths.
- 2025-09-18 (ec9c94d, perf n/a): Added cfg frame stack scaffolding to prepare control-flow builder
  for iterative execution; procedure nodes now scheduled via explicit frames.
- 2025-09-18 (2593977, perf n/a): Removed legacy call-graph demo/tests and routed APIs directly through
  `fortfront` to keep the surface lean.
- 2025-09-18 (550501f, perf n/a): Dropped `control_flow_analysis` wrapper; control-flow utilities now
  map straight to core CFG primitives.
- 2025-09-18 (db29f53, perf n/a): Removed semantic analyzer/pipeline infrastructure and related docs/tests
  to keep the frontend focused on the single-pass HM path.
- 2025-09-18 (local, perf n/a): Replaced recursive integer-expression walker with iterative stack logic,
  removed redundant module recursion, and added regression coverage for declaration inference heuristics.
- 2025-09-18 (local, perf n/a): Removed unused semantic query tracker hooks, trimmed unused-variable APIs,
  and normalized symbol metadata to reflect the lean context.
