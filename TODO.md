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
