# Codegen Architecture (2025)

## Summary
- Code generation is now an arena-first pass driven by
  `codegen_core::generate_code_from_arena`; the legacy stub from
  `codegen_utilities` has been removed and no longer shadows the main entry.
- `frontend_transformation` standardizes the AST before invoking codegen, so the
  backend operates on normalized nodes only.
- Call graph data is optional and built on top of the same arena; the codegen
  pass itself no longer coordinates control-flow graph builders.

## Data Flow
1. Pratt parsing and semantic analysis populate `compiler_arena%ast`.
2. `standardizer::standardize_ast` normalizes lazy constructs (array literals,
   implicit typing shims, modern literal syntax) into standard Fortran forms.
3. Codegen initializes formatting options through `codegen_indent` and type
   standardization through `codegen_type_utils`.
4. `codegen_core::initialize_codegen` wires dispatch tables for expressions,
   statements, and declarations before emitting any code.
5. `codegen_core::generate_code_from_arena` walks the arena, delegating to
   specialized helpers in `codegen_statements`, `codegen_expressions`, and
   `codegen_declarations` to produce buffered source lines.
6. `codegen_basic_utils::add_line_continuations` finalizes formatting (indent and
   continuation markers) before text is returned to the caller.

## Module Responsibilities
- `codegen_core` owns the traversal stack, orchestrates node-specific handlers,
  and exposes the public API used by both CLI and library entry points.
- `codegen_arena_interface` adapts arena nodes into the polymorphic dispatcher;
  it is now a thin layer because the stubbed helper has been removed.
- `codegen_statements` converts executable constructs, leveraging shared helpers
  to emit labels, indentation, and optional I/O specifiers.
- `codegen_expressions` renders literals, operator precedence, and call syntax
  using the same precedence ordering that the Pratt parser applies, guaranteeing
  round-tripping fidelity.
- `codegen_declarations` covers program units, modules, interfaces, and derived
  types, relying on the semantic pass to surface intent and kind information.

## Error Handling and Diagnostics
- Codegen assumes semantics succeeded; if diagnostics remain in the collection
  the caller decides whether to emit code. This keeps codegen side-effect free.
- All arena accesses are bounds checked to avoid dereferencing freed entries.
- Formatting options travel through `format_options_t`, allowing CLI and library
  surfaces to control indent width, line length, and continuation policy.

## Remaining Work and Observability
- The pass still allocates temporary buffers while lowering deeply nested
  `call_or_subscript` nodes. Profiling shows the allocations are rare but worth
  auditing after the Pratt pipeline settles.
- Indentation defaults to four spaces and line length 130 characters. Future
  enhancements may expose CLI flags that override these defaults without
  recompiling.
- Integration tests in `test/codegen` cover statement and expression generation;
  no dedicated CFG fixtures remain, aligning tests with the current architecture.
