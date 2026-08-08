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
- **Call argument mapping**: `query_call_arguments` resolves a same-arena
  function or subroutine call into formal-parameter order. Each
  `call_argument_query_t` preserves the original actual node and its value
  expression, reports whether the actual was supplied positionally or by
  keyword, and marks omitted optional dummies. Unresolved procedures,
  ambiguous generic calls, array accesses, and invalid argument lists return
  `found=.false.` rather than guessing.
- **Generic candidate query**: `query_generic_call` enumerates the concrete
  procedures in a same-arena named generic interface and exposes each formal's
  semantic type category, exact kind, rank, and derived-type identity. A
  candidate is marked `is_match` only for a complete exact signature match;
  `selected_procedure_node_index` is populated only when exactly one candidate
  matches. Conversions, extension-type compatibility, elemental expansion,
  and procedure-pointer dispatch remain explicit boundaries, so ambiguous or
  unsupported generic calls are never guessed.
- **Resolved type query**: `query_resolved_type` returns the semantic intrinsic
  category, exact Fortran kind value, storage size in bits, rank, and
  derived-type identity for an analyzed expression. The compiler pipeline
  resolves numeric and named kind selectors once after semantic analysis;
  backend callers do not inspect literal or declaration spelling.
- **Ownership and dispatch queries**: `query_storage` reports allocatable,
  pointer, target, contiguous, SAVE, module, and COMMON storage facts.
  `query_ownership_events` reports `ALLOCATE`, `DEALLOCATE`, `MOVE_ALLOC`,
  pointer-assignment, `NULLIFY`, and whole-allocatable assignment nodes with
  their operand indices. Allocation events expose explicit shape-expression
  indices, rank when resolved, and the existing `SOURCE=`/`MOLD=` expression
  indices. Assignment events expose `lhs_owner_path` and `rhs_owner_path`,
  resolved `lhs_rank`/`rhs_rank`, and the
  `OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE` /
  `OWNERSHIP_REALLOCATION_POTENTIAL` classifications. The older
  `owner_path`, `source_path`, and `destination_path` fields remain aliases
  for compatibility; unresolved ranks are `-1` and absent expression lists
  are empty.
  `query_component_path` returns the ordered component names and AST indices
  for chained `%` access, plus the resolved component declaration indices,
  base and result rank, terminal storage class, and explicit
  `is_array_element`, `is_array_section`, `is_concrete_derived`,
  `is_allocatable`, `is_pointer`, and polymorphism facts. A fully indexed
  designator such as `items(i,j)%payload` sets `is_array_element`; a
  non-strided section whose range AST facts are available, such as
  `items(:, :)%payload`, sets `is_array_section` instead and retains its
  rank. `query_storage` exposes the same terminal facts
  for a component, together with its declaration identity and rank. A direct
  component whose base is an `ASSOCIATE` name is unresolved (`found=.false.`)
  rather than guessed; pointer components remain visible as `STORAGE_POINTER`
  and are never reported as owned. Existing storage and global-state fields
  are preserved. For a directly resolved polymorphic allocatable target,
  `polymorphic_allocation` (or `query_polymorphic_allocation`) bundles the
  owner node/path, owner declaration index and declared `class(...)` type,
  source expression index/path, and the resolved concrete source type. Its
  `is_bounded` flag is true only for one scalar `SOURCE=` data designator
  whose declared type is concrete. `source_classification` is one of
  `POLYMORPHIC_SOURCE_CONCRETE`, `POLYMORPHIC_SOURCE_POLYMORPHIC`, or
  `POLYMORPHIC_SOURCE_UNKNOWN`; factories, dynamic sources, repeated
  acquisition, and alias paths remain unbounded and retain their explicit
  refusal flags. `query_type_binding_resolution` resolves a binding
  through `EXTENDS`, records inherited/deferred/generic/PASS facts, and
  includes concrete dynamic target type indices, implementation names, and
  implementation procedure node indices.
  `query_type_binding_hierarchy` is the bounded alternative for one declared
  type: it reports the local-to-parent chain and effective binding metadata,
  without scanning descendants or guessing an ambiguous, deferred, or
  unresolved implementation. For an effective statically resolvable binding,
  the hierarchy summary and each entry also expose the implementation
  procedure node, effective PASS dummy name and position, passed-object
  declared type, and `implementation_signature_resolved`. Deferred inherited
  entries and ambiguous generics deliberately expose no implementation target
  or signature. Finally,
  `query_type_bound_call` combines a call site's receiver and binding with
  that resolution metadata. It reports the declared receiver type, effective
  implementation, PASS, interface, and concrete descendant dispatch targets.
  For expression-form calls, `receiver_path` preserves the receiver's
  component names and AST nodes (for example the `inner` component in
  `outer%inner%method`); explicit `CALL` statements retain the exact nested
  designator in `receiver_name` when their syntax has no receiver AST node.
  Generic, ambiguous, deferred, and unresolved/refused cases remain marked
  without selecting a runtime procedure. Finally,
  `query_active_global_references` reports identifier references bound to
  module entities or COMMON members. These are facts, not AD policy: a
  consumer may deliberately reject active mutable global state.
- **ASSOCIATE selector query**: `query_associate_selectors` returns one
  `associate_selector_query_t` per association. The record identifies the
  ASSOCIATE node, association ordinal, selector expression, storage
  declaration, base node, and component path. It also reports resolved
  semantic type kind, declared and known dynamic type names, array-element
  rank, alias status, pointer and polymorphic boundaries, and aggregate
  read/write use in the construct body. `query_associate_selector` accepts
  an ASSOCIATE node plus an optional ordinal or a selector expression node.
  Direct component and array-element designators are aliases only when their
  storage identity resolves. Expressions have no storage identity and are
  read-only. Pointer targets, polymorphic dynamic types, unresolved selector
  storage, and ambiguous call accesses set explicit boundary flags and do not
  receive a guessed target or dynamic type.
- **Type-bound dispatch signatures**: `query_type_bound_call` preserves the
  existing dispatch target type and implementation arrays and adds parallel
  implementation procedure node indices, effective PASS names, positions,
  passed-object declared types, and a signature-resolved flag for each
  concrete target. Refused generic or unresolved calls expose no target facts.
- **Procedure-pointer target query**: `query_procedure_target` reports one
  direct `=>` assignment whose left side is a declared procedure pointer. It
  preserves the assignment, pointer declaration, target expression, and
  lexical scope indices; reports the target procedure name and binding when
  the resolver has one; and distinguishes a resolved internal procedure,
  an external declaration binding, `NULL()`, and an unresolved target. A
  non-identifier target (other than `NULL()`) remains unresolved, and the
  query does not infer flow-sensitive callback state, generic dispatch, or
  any AD policy.
- **Bounded procedure-pointer call target query**:
  `query_procedure_call_target` reports a complete fact only for a direct
  call through a declared procedure pointer with exactly one unconditional,
  same-scope, direct `=>` assignment before the call. The result preserves
  the call/pointer occurrence, pointer declaration, assignment, target
  expression, lexical scope, and resolved internal/external target identity
  (the AST stores a direct call's callee name on the call node, so the call
  and pointer occurrence share that node index). `found=.false.` with
  `is_unresolved=.true.` is returned when that proof is unavailable, including
  branch-local assignments, reassignment or `NULLIFY`, `NULL()`, and other
  flow-sensitive cases; generic calls are not callback facts. This is a
  bounded identity query, not general callback analysis or AD policy.

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
