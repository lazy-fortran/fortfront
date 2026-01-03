# Semantic Pipeline Architecture (2025)

## Current Responsibilities
- The semantic layer runs on top of the Pratt pipeline output and keeps a lean
  focus on type inference, shape checks, and implicit typing enforcement.
- `semantic_analyzer::analyze_program` is the single entry point. It receives an
  `ast_arena_t` plus the root program index and returns updates in place; no
  control-flow graph builders are involved in the fast path.
- The pipeline only escalates to strict mode when `implicit none` is present,
  mirroring the historical lazy Fortran defaults.

## Phase Breakdown
1. **Context setup** – `create_semantic_context` builds a scope stack, installs
   intrinsic bindings, and allocates the substitution table used by Hindley–
   Milner inference.
2. **Program walk** – `analyze_program_node_arena` iterates the program body
   using a lightweight stack (`infer_frame_t`) that records traversal state
   without resorting to recursion.
3. **Statement inference** – Specialized analyzers in
   `semantic_assignment_inference`, `semantic_binary_operations`, and
   `semantic_function_analysis` infer or refine types, emitting diagnostics via
   `semantic_context_t%errors` when inference fails.
4. **Array safety checks** – `validate_array_bounds` and
   `check_shape_conformance` run after inference to guard against inconsistent
   dimensions introduced during transformation.
5. **Post-processing** – Constant folding (`constant_transformation`) and
   optional arena compaction execute once per traversal; they operate on the same
   arena produced by the Pratt parser.

## Interaction with Call Graph and Tooling
- The semantic pass no longer orchestrates CFG builders. Instead the optional
  call graph walks reuse the finalized arena via
  `call_graph_module::build_call_graph` after semantics completes.
- Tooling that needs type data (e.g. `fortfront_types`, variable usage trackers)
  queries `semantic_context_t` helpers such as `get_type_for_node` and
  `update_identifier_type_in_arena` rather than re-running analysis.
- Because semantic updates happen in place, downstream passes observe the latest
  substitutions without extra copying; arena indices remain stable for the call
  graph and variable usage modules.

## Diagnostics and Error Reporting
- Errors accumulate inside `semantic_context_t%errors` so callers can emit or
  serialize diagnostics without aborting the run.
- Each analyzer validates indices before touching arena entries, preventing
  malformed ASTs from crashing the pass.
- Standardization and code generation read the diagnostic collection to decide
  whether to continue emitting Fortran code.

## AST Location Validation (Issue #2383)
- `frontend_location_validation` module walks the AST after parsing and standardization
  to verify all nodes have valid source locations (`line` > 0, `column` > 0).
- Enabled via `FORTFRONT_VALIDATE_LOCATIONS` environment variable (debug/test builds).
- Set the environment variable to `strict` to flag nodes that keep default
  line/column values (1:1) after parsing.
- Missing locations result in degraded diagnostics (errors appear at "line 1").
- Synthesized nodes (e.g., `contains` wrappers, `implicit none` statements) are
  allowed to have default locations in non-strict mode.
- Validation reports violations to stderr but does not abort compilation.

## Remaining Work
- Nested procedure scoping still depends on post-walk fixups in
  `call_graph_module::build_call_graph`; unscoped procedure bodies can hide free variables
  from the analyzer.
- Large array constructors allocate temporary buffers during shape checks. A
  dedicated scratch allocator would shrink the final hotspot visible in perf
  traces.
- Strict semantic mode is toggled inside the context today. Future work should
  thread explicit strictness flags through `frontend_transformation` so callers
  can opt in earlier.
