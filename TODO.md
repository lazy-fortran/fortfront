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
   - [ ] Scan parser, standardizer, and utilities for unused recursive helpers (`parse_*`, `traverse_*`, legacy validation shims).
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
- [x] Variable usage collector and expression visitor now run on explicit stacks (no recursion).
- [x] Call graph AST traversal and cycle detection converted to iterative algorithms.
- [ ] CFG builder (`cfg_builder_control_handlers.f90`) still recursive; design for staged frame stack pending.
- [ ] Control-flow analyzer plugin continues to recurse through AST while building CFG snapshots.
- [ ] Standardizer/semantic helpers (e.g., `standardize_ast`, `collect_identifiers_recursive` call proxies) need auditing for residual recursion.

## Challenges
- CFG handlers mutate shared builder state (`builder%current_block_id`, buffered statements, block IDs) mid-call; iterative rewrite requires explicit frame records capturing pending continuations, temporary IDs, and branch metadata.
- Control-flow analyzer currently assumes recursive CFG construction; refactor must expose an iterative API without breaking existing analyzer contracts.
- Remaining semantic/standardizer utilities reuse AST traversal helpers that still rely on `traverse_preorder` recursion; must introduce stack-based alternatives and retire recursive entry points.

## Plan of Record
1. **Design Iterative CFG Frame Model**
   - [ ] Specify a `cfg_frame_t` structure capturing node index, continuation state (e.g., WHICH branch is pending), and any temporary arrays currently produced via allocation (elseif wrappers, loop bodies).
   - [ ] Prototype push/pop helpers with bounded scratch buffers (reuse builder statement buffer where possible) and clear lifecycle semantics.
   - [ ] Document control-flow invariants (edge ordering, block creation rules) to use as acceptance criteria.

2. **Refactor `process_node` & Helpers**
   - [ ] Replace `process_node` recursion with stack-driven dispatcher; ensure all helper routines (`process_if_statement`, `process_do_loop`, etc.) enqueue follow-up work instead of recursing directly.
   - [ ] Eliminate per-call allocatables inside hot loops by reusing frame-owned scratch storage; avoid repeated `allocate/deallocate` churn.
   - [ ] Add targeted regression tests for deep/nested constructs (elseif ladders, nested where/forall) to validate identical CFG output.

3. **Update Control-Flow Analyzer Plugin**
   - [ ] Adjust analyzer to rely on the iterative CFG builder API; remove `traverse_ast_for_cfg` recursion.
   - [ ] Introduce perf metrics frame cache (reuse builder frames) so iterative traversal does not regress existing instrumentation.
   - [ ] Re-run Windows/Linux CI stress cases, verifying absence of stack reserve hacks.

4. **Audit Remaining Semantic/Standardizer Recursion**
   - [ ] Identify and replace recursive helpers in standardizer modules (`standardize_ast`, declaration walkers) with the shared stack utilities.
   - [ ] Migrate AST traversal call sites to iterative variants (optionally move legacy recursive procedures behind compatibility wrappers slated for removal).
   - [ ] Update docs/tests to reflect the new traversal utilities and deprecate recursive entry points.

5. **Validation & Rollout**
   - [ ] Maintain tests green after each stage (`make test`, `RUN_SYSTEM_TESTS=1 fpm test`); add Windows-focused pipeline runs before merging.
   - [ ] Capture before/after performance metrics for CLI transform and CFG-intensive analyzers.
   - [ ] When final stage lands, remove residual references to recursion in code comments/docs and note stack guarantees in `DOCS/architecture.md`.

## Notes
- Keep progress logged here after each milestone (design complete, prototype merged, etc.).
- Landing order should minimise risk: finish CFG builder before touching analyzer/standardizer layers so downstream work can assume the new API.
- Reuse the existing explicit stacks (variable usage, call graph) as references for frame handling and guard patterns (visited sets, capacity growth).

# Performance-Driven Frontend Streamlining

## Rationale
- Current command-line flow executes multiple analysis passes (legacy `analyze_program` plus pipeline analyzers, CFG builder, call graph) even when we only need a typed AST for codegen. This burns time and allocation, obscuring the core goal of a fast, portable Lazy Fortran frontend.
- Several subsystems (dual call-graph implementations, performance analyzer plugins, verbose tracing) were added for future tools but are not required for CLI translation. They increase coupling and make it harder to optimise hot paths.
- Dynamic string-heavy data structures and repeated arena traversals work against cache locality; we should favour SoA layouts, interned identifiers, and single-pass pipelines.

## Guiding Principles
1. **Single-Pass CLI Pipeline** – Lex → parse → HM type inference → standardise/codegen. Optional analyzers must be disable-able at build time.
2. **Arena-Centric Data** – Keep AST/CST ownership in arenas; avoid per-pass allocations and dynamic strings where intern tables suffice.
3. **Feature Flags** – Analyzer/CFG/call-graph tooling should be pluggable so production builds can drop them entirely.
4. **Metrics First** – Establish timing/memory baselines before/after each change to ensure real gains.

## Work Breakdown

1. **Baseline & Instrumentation**
   - [x] Add lightweight timers around CLI stages (lexer, parser, semantics, codegen) gated by `FORTFRONT_PROFILE=1`.
   - [x] Capture current wall-clock timings on representative inputs (small, medium, stress) and store results in `docs/perf/baseline.md`.
   - _2025-09-18_: Baseline timings recorded (see table) using new profiling flag; allocation tracking remains future work.

2. **Prune Non-Essential Analyzers From CLI Hot Path**
   - [ ] Introduce build/runtime flag to skip control-flow, call-graph, and performance analyzers during CLI transforms.
   - [ ] Ensure `frontend_transformation` only constructs CFG/call graphs when explicitly requested (e.g., via tooling API).
   - [ ] Validate CLI regressions remain green after analyzers are removed from default flow.

3. **Unify Call Graph Infrastructure**
   - [ ] Merge `call_graph_builder` functionality into the leaner `call_graph_module`; delete redundant traversal/analysis layers.
   - [ ] Replace string-based scope tracking with integer IDs referencing arena entries to cut allocation and comparisons.
   - [ ] Rebuild tests to exercise the unified implementation.

4. **Semantic Pipeline Simplification**
   - [ ] Stop running both legacy `analyze_program` and the new pipeline; choose one extensible HM path.
   - [ ] Profile HM inference to identify hot allocators; migrate temporary structures to arena-backed slabs.
   - [ ] Document future hooks for fortfc/fluff so backend exports bypass unneeded formatting passes.

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
- **Legacy Call Graph Analysis Layer** (`src/analysis/call_graph_analysis.f90` once unified implementation lands): likely redundant after unification; plan removal.
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
