# Frontend

## Purpose

The frontend orchestrates the complete transformation pipeline from source text to standardized Fortran output. It manages the high-level flow through lexing, parsing, semantic analysis, and code generation, while handling special cases like mixed constructs (`.lf` files with embedded standard Fortran), program structure detection, and statement boundary identification.

The frontend provides the primary API entry point for transforming lazy Fortran to standard Fortran and for round-trip validation of standard Fortran.

## File Index

| File | Description |
|------|-------------|
| frontend_compiler_api.f90 | Public compiler-facing API returning AST, semantic context, tokens, and diagnostics without codegen |
| frontend_compiler_queries.f90 | Safe compiler-facing AST queries for backend consumers |
| frontend_compiler_type_queries.f90 | Resolved expression category, exact kind, storage size, rank, and derived-type queries |
| frontend_compiler_node_queries.f90 | Compiler-facing declaration, derived-type, label, and GOTO node metadata queries |
| frontend_tooling_api.f90 | Public API for tool integration (linters, language servers) |
| frontend_transformation_pipeline.f90 | Main transformation orchestration: lex → parse → semantic → codegen |
| frontend_transformation_pipeline_helpers.inc | Private pipeline helpers (option resolution, pre-parse early exit, arena pipeline, leading-comment prepend) included by frontend_transformation_pipeline.f90 |
| frontend_pass_manager.f90 | Configurable pass manager for transformation phases |
| frontend_final_passes.f90 | Pass implementations for semantic, standardization, monomorphization, codegen |
| frontend_transformation_structure.f90 | Include wrapper for program structure transformation |
| frontend_transformation_structure.inc | Program structure transformation implementation (wrap bare statements in program) |
| frontend_transformation_analysis.f90 | Semantic analysis integration and coordination |
| frontend_transformation_semantics.f90 | Semantic phase coordination |
| frontend_transformation_common.f90 | Shared transformation utilities |
| frontend_mixed_constructs.f90 | Handle `.lf` files with embedded standard Fortran blocks |
| frontend_program_structure.f90 | Detect program structure (module vs program vs bare statements) |
| frontend_program_units.f90 | Program unit identification and extraction |
| frontend_program_unit_detection.f90 | Shared strict-mode program-unit detection helpers |
| frontend_program_unit_scanner.f90 | Scan for program unit boundaries |
| frontend_statement_boundary.f90 | Statement boundary detection across program units |
| frontend_statement_processing.f90 | Statement-level processing and normalization |
| frontend_statement_contains_section.f90 | Implicit contains section parsing helpers |
| frontend_statement_contains_section_helpers.f90 | Helper module for implicit contains section scanning |
| frontend_statement_token_parsing.f90 | Statement token preparation and parsing helpers |
| frontend_statement_spec_section.f90 | Spec-section tracking and statement function conversion |
| frontend_token_normalization.f90 | Token stream normalization before parsing |
| frontend_diagnostics.f90 | Diagnostic message formatting and error reporting |
| frontend_location_validation.f90 | AST source location validation |
| frontend_analysis_helpers.f90 | Analysis helper functions |
| frontend_program_builders.f90 | Program structure building utilities |

## Key Concepts

**Transformation Pipeline**

The transformation pipeline uses a configurable pass manager system (inspired by GCC's `gfc_run_passes`) to orchestrate the transformation phases:

1. **Lexing**: Tokenize source text
2. **Parsing**: Build AST from tokens
3. **Semantic Analysis**: Type inference, scope resolution
4. **Standardization**: Normalize AST structure
5. **Monomorphization**: Specialize generic procedures
6. **Code Generation**: Emit standardized Fortran

**Pass Manager Architecture**

The pass manager (`frontend_pass_manager.f90`) provides a flexible framework for orchestrating transformation phases:

- **Configurable Pipeline**: Passes can be enabled/disabled individually
- **Early Stopping**: Support for stopping after specific passes (e.g., stop_after_semantic)
- **Named Passes**: Each pass has metadata (name, trace key) for debugging
- **Extensible**: New passes can be registered without modifying core pipeline
- **Traceability**: Automatic tracing integration for performance analysis

Example configuration:
```fortran
use frontend_pass_manager, only: pass_config_t, create_default_config

! Create custom configuration
type(pass_config_t) :: config
config = create_default_config()
config%stop_after_semantic = .true.  ! Stop after type inference
config%enable_monomorphization = .false.  ! Skip generic specialization
```

**Registering Custom Passes**

Tools can extend the pipeline by registering custom passes:

```fortran
use frontend_pass_manager, only: pass_manager_t, pass_context_t
use frontend_pass_manager, only: PASS_SEMANTIC, PASS_CODEGEN

! Create pass manager
manager = create_pass_manager()

! Register standard passes
call manager%add_pass(PASS_SEMANTIC, "Semantic Analysis", &
                     "phase:semantic", .true., semantic_pass)

! Register custom warning pass
call manager%add_pass(100, "Loop Warnings", "phase:loop_warn", &
                     .false., custom_loop_warning_pass)

! Register codegen last
call manager%add_pass(PASS_CODEGEN, "Code Generation", &
                     "phase:codegen", .true., codegen_pass)

! Run pipeline
call manager%run(context)
```

Custom passes must implement the `pass_proc` interface:
```fortran
subroutine custom_loop_warning_pass(context)
    use frontend_pass_manager, only: pass_context_t
    type(pass_context_t), intent(inout) :: context

    ! Access AST via context%compiler_arena%ast
    ! Set context%error_msg on fatal errors
    ! Write warnings to error_unit for non-fatal issues
end subroutine
```

**Mixed Construct Handling**
- `.lf` files may contain both lazy and standard Fortran
- Standard blocks wrapped in special markers: `!fortfront:standard_begin` / `!fortfront:standard_end`
- Standard blocks passed through unchanged
- Lazy blocks transformed via full pipeline
- See `docs/guides/MIXED_CONSTRUCTS_GUIDE.md`

**Program Structure Detection**
- **Bare statements**: Wrap in `program main ... end program`
- **Single procedure**: Keep as standalone procedure
- **Module**: Preserve module structure
- **Complete program**: Use existing structure

**Statement Boundary Detection**
- Identify statement boundaries in free-form source
- Handle continuation lines (ampersand)
- Detect statement keywords in various contexts
- Support legacy fixed-form (optional)

**Token Normalization**
- Normalize keyword case
- Handle operator variants (`==` vs `.eq.`)
- Collapse whitespace patterns
- Prepare clean token stream for parser

**API Interfaces**
- **CLI**: `fortfront input.lf > output.f90`
- **Library**: `transform_lazy_fortran_string(input, output, errors)`
- **Tooling**: `tooling_load_ast_from_string(source, arena, root, errors)`
- **Compiler**: `compile_frontend_from_string(source, result)` returns a
  `compiler_frontend_result_t` that owns the AST arena, root index, semantic
  context, token stream, diagnostics, and source text. The compiler API stops
  after parsing and semantic analysis by default; it does not run standardization
  or Fortran code generation unless a separate caller invokes those APIs.
- **Compiler queries**: `is_subroutine_call_statement`,
  `get_subroutine_call_name`, and `get_subroutine_call_arg_indices` expose
  explicit `CALL` statements without requiring backend consumers to inspect
  concrete AST node storage. `is_binary_op` and `get_binary_op_info`
  (operator text, left/right operand indices, node location) do the same for
  binary operations. `is_literal` and `get_literal_info` (value text,
  literal type) do the same for literals. `is_identifier` and
  `get_identifier_name` do the same for identifier references.
- **Resolved type query**: `query_resolved_type` returns the semantic intrinsic
  category, exact Fortran kind value, storage size in bits, rank, and
  derived-type identity for an analyzed expression. The compiler pipeline
  resolves numeric and named kind selectors once after semantic analysis;
  backend callers do not inspect literal or declaration spelling.
- **Ownership and dispatch queries**: `query_storage` reports allocatable,
  pointer, target, contiguous, SAVE, module, and COMMON storage facts.
  `query_ownership_events` reports `ALLOCATE`, `DEALLOCATE`, `MOVE_ALLOC`,
  and pointer-assignment nodes with their operand indices.
  `query_component_path` returns the ordered component names and AST indices
  for chained `%` access. `query_type_binding_resolution` resolves a binding
  through `EXTENDS`, records inherited/deferred/generic/PASS facts, and
  includes concrete dynamic target type indices and implementations. Finally,
  `query_active_global_references` reports identifier references bound to
  module entities or COMMON members. These are facts, not AD policy: a
  consumer may deliberately reject active mutable global state.

## Dependencies

**Lexer**
- `lexer/` - Tokenization of source text

**Parser**
- `parser/` - AST construction from tokens

**Semantic Analysis**
- `semantic/` - Type inference and validation
- `semantic/analyzers/` - Semantic analyzers

**Code Generation**
- `codegen/` - Emit standardized Fortran

**Analysis**
- `analysis/call_graph` - Procedure analysis for type inference

**AST**
- `ast/` - AST data structures and traversal

**Utilities**
- `utilities/` - String utilities, debug tracing
