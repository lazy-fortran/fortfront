# Pratt Pipeline Architecture

## Overview
- The Pratt-driven parser defined in `parser_expressions_module` now backs every
  expression entry point exposed through `frontend_parsing::parse_tokens`.
- `frontend_transformation` reuses a module-level `compiler_arena_t` so lexing,
  Pratt parsing, semantic analysis, and code generation share a contiguous slab.
- Tooling surfaces reuse the same arena; `fortfront`, `frontend_tooling_api`,
  `call_graph_module`, and `variable_usage` avoid secondary parsing or CFG
  builders in the hot path.

## Execution Flow
1. Source text is tokenized by `lexer_core::tokenize_core`, producing a shared
   token buffer that carries line and column metadata for diagnostics.
2. `parser_state_module::create_parser_state` seeds the Pratt loop with a
   structure-of-arrays view (`token_text`, `token_kind`, `token_line`,
   `token_column`) that avoids copying while keeping the authoritative token
   storage intact for tooling.
3. `parser_dispatcher_module::parse_statement_dispatcher` drives statement-level
   helpers and delegates every expression boundary to the Pratt loop hosted in
   `parser_expressions_module`.
4. AST nodes are allocated inside `compiler_arena%ast`, an `ast_arena_t` owned by
   `compiler_arena_t`; indices returned from Pratt parsing flow straight into
   later semantic and codegen phases.
5. `semantic_analyzer::analyze_program` runs on demand; lazy mode keeps implicit
   typing unless `implicit none` toggles strict analysis inside the semantic
   context.
6. `frontend_transformation::transform_lazy_fortran_string` standardizes the AST
   and feeds `codegen_core::generate_code_from_arena` to emit standard Fortran.

## Pratt Core Highlights
- Operator precedence tables and postfix handlers live alongside the Pratt loop
  inside `parser_expressions_module`, ensuring CLI and library consumers share a
  single dispatch table.
- Operand, operator, and prefix stacks grow geometrically; when a stack exceeds
  its on-stack capacity the Pratt loop borrows scratch buffers from the active
  arena to avoid heap churn.
- Postfix chains (`%`, `()`, `[]`) reuse the Pratt loop through helpers such as
  `parse_component_access_postfix` and
  `push_call_or_subscript_with_slice_detection`.
- Range syntax is implemented as a ternary postfix that never lands on the
  operator stack; colon dispatch collapses immediately so nested ranges keep the
  correct precedence.
- Prefix handling folds unary operators eagerly by collapsing the prefix stack
  before operands are pushed, keeping AST shape predictable for later passes.

## Diagnostics and Recovery
- Every arena access is bounds checked so malformed input cannot corrupt the AST
  or crash the pipeline.
- Invalid tokens materialize literal nodes with diagnostic payloads, matching the
  legacy lazy Fortran behaviour while letting the Pratt loop continue.
- `frontend_parsing::parse_tokens_safe` returns a `parse_result_with_index_t`
  bundle that includes the root program handle plus syntax errors for tooling.

## Integration with Call Graph and Tooling
- Optional analysis surfaces operate solely on the Pratt output; call graph
  walkers traverse `ast_arena_t` via `ast_traversal::traverse_ast`, resolving
  procedure handles with arena-backed symbol tables.
- `frontend_tooling_api::tooling_load_ast_from_string` exposes the Pratt arena
  and optional token buffer directly to tooling, skipping semantics and
  standardization unless toggled via `tooling_parse_options_t`.
- Library consumers can fetch CST handles through `cst_arena` without
  re-tokenizing because Pratt preserves the original slice metadata.
- CLI commands expose both AST and CST handles in a single traversal, now routing
  through the lightweight tooling API so downstream tools can measure parse
  latency without triggering extra passes.

## Tooling Usage
- `fortfront::build_call_graph_from_arena` now accepts the arena/root pair
  directly so tooling (fortfc, fluff) can produce call graphs without invoking
  semantic passes. Scope resolution uses arena indices internally, eliminating
  the string churn from the legacy walkers.
- After parsing, call `graph = build_call_graph_from_arena(arena, root_index)`
  and inspect `graph%procedures(1:graph%proc_count)` / `graph%calls` to iterate
  procedures and edges respectively.
- The returned `call_graph_t` de-duplicates procedure names and exposes helpers
  such as `get_all_procedures`, `get_procedure_callers`, and
  `get_recursive_cycles` so tooling can answer common queries without running
  additional passes.

## Limitations and Follow-ups
- Nested procedure discovery still relies on arena sweeps in call graph walkers;
  `call_graph_module::build_call_graph` can miss indirect recursion when
  declarations are absent.
- Deeply nested array literals force temporary scratch buffers; profiling shows
  these allocations remain the Pratt hotspot worth revisiting.
- Strict semantic mode is currently toggled inside `semantic_context_t`. A
  future refinement should thread strictness flags through `parser_state_t` so
  CLI callers can opt in earlier in the pipeline.
- `tooling_parse_options_t` currently exposes only `run_semantics` and
  `reuse_arena`; future iterations should add standardization toggles plus arena
  pooling heuristics for reuse-heavy tooling.
