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
  after parsing and semantic analysis by default. It does not run standardization
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
  keyword, marks omitted optional dummies, and carries the formal intent,
  value/pointer/allocatable/target attributes, semantic type/kind/rank, and
  the resolved actual type/storage facts. `has_type_mismatch` and
  `has_unknown_argument_types` are explicit boundaries; the query does not
  invent conversions. At the call level, `has_global_mutable_state`,
  `has_unresolved_alias`, `has_procedure_callback`, and `is_refused` make
  calls that are not closed over ordinary differentiable storage explicit.
  A mapping may still be `found` when one of these flags is set, so FortAD
  can report the reason instead of silently differentiating global state,
  aliases, or an unresolved callback. Unresolved procedures, ambiguous
  generic calls, array accesses, and invalid argument lists return
  `found=.false.` rather than guessing.
- **Passed-procedure mapping**: `query_procedure_actual_argument` joins one
  named procedure dummy from `query_call_arguments` to a direct same-arena
  function or subroutine actual. It also resolves a procedure-pointer actual
  when the pointer has exactly one unconditional, same-scope direct assignment
  before the call and that assignment has a resolved target signature. The
  result preserves the assignment and target node identities alongside the
  target procedure and `procedure_signature_query_t`; `has_contextual_target`
  remains true to show that the actual was a pointer. Branch-local targets,
  reassignment, `NULL()`, unresolved targets, procedure dummies, generic names,
  and non-identifiers remain refusal-only facts with `is_unresolved` and the
  corresponding `has_branch_target`, `has_reassignment`, `has_null_target`,
  `has_unresolved_target`, or ambiguity flag. No pointer state is inferred
  across a branch or an assignment after the call.
- **Generic candidate query**: `query_generic_call` enumerates the concrete
  procedures in a same-arena named generic interface and exposes each formal's
  semantic type category, exact kind, rank, and derived-type identity. A
  candidate is marked `is_match` only for a complete exact signature match.
  `selected_procedure_node_index` is populated only when exactly one candidate
  matches. Conversions, extension-type compatibility, elemental expansion,
  and procedure-pointer dispatch remain explicit boundaries, so ambiguous or
  unsupported generic calls are never guessed.
- **Resolved type query**: `query_resolved_type` returns the semantic intrinsic
  category, exact Fortran kind value, storage size in bits, rank, and
  derived-type identity for an analyzed expression. The compiler pipeline
  resolves numeric and named kind selectors once after semantic analysis.
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
  for compatibility. Unresolved ranks are `-1` and absent expression lists
  are empty.
  Events are returned in source order within the queried scope and carry a
  one-based `sequence_index`. `ALLOCATE` and `DEALLOCATE` expose owner
  pre/post allocation states. An allocatable assignment exposes an allocated
  owner post-state plus `has_potential_implicit_reallocation`. `MOVE_ALLOC`
  exposes an unallocated source post-state, a destination state equal to the
  source state, and `has_implicit_destination_deallocation` for the required
  destination-before-transfer step. These facts do not infer runtime state,
  aliases, or mutable global ownership.
  Ownership events additionally expose direct whole-allocatable source and
  destination declaration identities, storage classes, and bounded dynamic
  type names before/after `MOVE_ALLOC` or allocatable reallocation. A concrete
  array moved into `class(base_t)` retains its known dynamic type through a
  straight-line sequence. Module/SAVE/COMMON state, pointer/TARGET,
  component, array-element/section, ASSOCIATE, control-flow, and unresolved
  paths remain explicit refusal or dynamic-type-boundary facts.
  `query_component_path` returns the ordered component names and AST indices
  for chained `%` access, plus the resolved component declaration indices,
  base and result rank, terminal storage class, and explicit
  `is_array_element`, `is_array_section`, `is_concrete_derived`,
  `is_abstract_type`, `is_allocatable`, `is_pointer`, and polymorphism facts.
  `is_abstract_type` identifies the declared terminal derived type, including
  through a `class(base_t)` declaration; it does not claim a runtime dynamic
  type. A fully indexed
  designator such as `items(i,j)%payload` sets `is_array_element`. A
  non-strided section whose range AST facts are available, such as
  `items(:, :)%payload`, sets `is_array_section` instead and retains its
  rank. `query_storage` exposes the same terminal facts
  for a component, together with its declaration identity and rank. A direct
  component whose base is an `ASSOCIATE` name is unresolved (`found=.false.`)
  rather than guessed. Pointer components remain visible as `STORAGE_POINTER`
  and are never reported as owned. Existing storage and global-state fields
  are preserved. Derived-type component declarations are parsed into one
  declaration node per entity, including compound declarations and legacy
  `TYPE name` headers, so `component_declaration_indices` and terminal
  storage facts remain available for every component name.
  For a directly resolved polymorphic allocatable target,
  `polymorphic_allocation` (or `query_polymorphic_allocation`) bundles the
  owner node/path, owner declaration index and declared `class(...)` type,
  source expression index/path, and the resolved concrete source type. Its
  `is_bounded` flag is true only for one scalar `SOURCE=` data designator
  whose declared type is concrete. `source_classification` is one of
  `POLYMORPHIC_SOURCE_CONCRETE`, `POLYMORPHIC_SOURCE_POLYMORPHIC`, or
  `POLYMORPHIC_SOURCE_UNKNOWN`. Factories, dynamic sources, repeated
  acquisition, and alias paths remain unbounded and retain their explicit
  refusal flags. `query_type_binding_resolution` resolves a binding
  through `EXTENDS`, records inherited/deferred/generic/PASS facts, and
  includes concrete dynamic target type indices, implementation names, and
  implementation procedure node indices.
  Its dispatch-target arrays also carry parallel
  `dispatch_target_declaring_type_indices` and
  `dispatch_target_is_inherited` facts. These identify the effective binding
  declaration when a concrete leaf inherits an implementation through an
  abstract intermediate type; they do not add a target or resolve a deferred,
  generic, ambiguous, unresolved, loop, ownership, or global-state case.
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
  implementation, PASS, interface, and concrete descendant dispatch targets;
  abstract descendants are excluded even when they provide a concrete
  binding.
  `query_select_type_owned_array(arena, arm_node_index)` provides the bounded
  `CLASS IS` alternative for a local `STORAGE_OWNED` allocatable
  `class(abstract_type)` array. A resolved result carries the selector
  declaration identity, positive rank, abstract declared type, and exact
  declared/dynamic type indices and names for the concrete guard type. It
  refuses `TYPE IS`, `CLASS DEFAULT`, abstract guards, associate or
  pointer/TARGET aliases, borrowed dummies, module/SAVE/COMMON state, and
  control-flow-nested arms; `has_unresolved_alias`,
  `has_global_mutable_state`, and `has_control_flow_boundary` identify those
  boundaries without exposing a guessed dynamic type.

  Explicit `CALL` statements with an indexed receiver, such as
  `call values(i)%run()`, retain the full receiver designator while resolving
  storage through its declared array object; no receiver AST node is invented.
  For an explicit `CALL` with a nested component receiver, such as
  `call outer%inner%apply()`, `receiver_path` now exposes the semantically
  resolved component names and declaration identities even when the parser
  retains only source text. Its component AST indices remain zero and its
  rank/section flags remain unknown (`-1`/false); consumers must use the
  receiver's actual expression AST, when available, for shape facts.
  For expression-form calls, `receiver_path` preserves the receiver's
  component names and AST nodes (for example the `inner` component in
  `outer%inner%method`). Explicit `CALL` statements retain the exact nested
  designator in `receiver_name` when their syntax has no receiver AST node.
  Generic, ambiguous, deferred, and unresolved/refused cases remain marked
  without selecting a runtime procedure. Finally,
  `query_active_global_references` reports identifier references bound to
  module entities or COMMON members. These are facts, not AD policy: a
  consumer may deliberately reject active mutable global state.

- **Owned-array CLASS IS binding identity query**:
  `query_select_type_owned_array_binding(arena, arm_node_index, binding_name)`
  composes the owned-array proof with the static binding hierarchy. Its
  `declared_binding` preserves an abstract/deferred binding, while
  `dynamic_binding` and the top-level implementation fields identify the
  concrete local or inherited implementation only when it is source-resolved.
  Generic, ambiguous, deferred, unresolved, alias, global-state, and
  control-flow boundaries remain explicit refusals; no runtime target is
  guessed.
- **Owned-array CLASS IS generic/PASS dispatch query**:
  `query_select_type_owned_array_generic_dispatch(arena, arm_node_index,
  call_node_index)` resolves an exact generic call on an array element such as
  `values(i)%choose(value)`. It maps the receiver to the owned selector's
  storage declaration and exposes the unique selected specific, ordered
  signature, and PASS position only for one exact type/kind/rank match. PASS
  metadata is resolved per specific: candidate records expose
  `pass_metadata_resolved`, `pass_arg`, `pass_name`, and `pass_position`, while
  `selected_pass_*` records identify the selected specific's effective PASS
  mapping, including a named passed-object dummy that is not first.
  Ambiguous, zero-match, deferred, non-element, global, alias, and
  control-flow cases remain explicit refusals; no runtime target is guessed.
- **Owned-array CLASS IS direct dispatch query**:
  `query_select_type_owned_array_dispatch(arena, arm_node_index,
  call_node_index)` resolves one explicit non-generic call on an indexed owned
  array receiver. It maps the receiver to the selector's storage declaration
  and exposes the concrete or inherited implementation, ordered signature,
  and effective PASS metadata. `receiver_storage` describes the array owner;
  `is_array_element_receiver` identifies the scalar designator. Generic,
  global, alias, and control-flow cases remain explicit refusals.
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
  lexical scope indices, reports the target procedure name and binding when
  the resolver has one, and distinguishes a resolved internal procedure,
  an external declaration binding, `NULL()`, and an unresolved target. A
  non-identifier target (other than `NULL()`) remains unresolved, and the
  query does not infer flow-sensitive callback state, generic dispatch, or
  any AD policy. For an internal procedure target, `signature` exposes a
  bounded `procedure_signature_query_t`: the ordered `dummies` array and
  `dummy_count`, plus result category/type-kind/kind/rank facts for functions.
  Each `procedure_dummy_query_t` carries its name, category, type-kind,
  kind, rank, intent, and OPTIONAL/VALUE flags. Consumers must check the
  corresponding `*_known` or `has_intent` flag before using a value. Rank 0
  is scalar. `rank=-1` means that rank was not proved. External, generic,
  ambiguous, NULL, and unresolved targets leave `signature%found` false rather
  than manufacturing an interface.
- **SELECT RANK arm query**: `query_control_statement` now returns one
  `select_rank_arm_query_t` per explicit, `RANK (*)`, or `RANK DEFAULT` arm.
  Each record preserves the selector and declaration identity, selected rank,
  arm source location, body entry/exit nodes, and dispatch kind. Existing
  storage and component-path facts are reused where available. Pointer
  selectors, unresolved dynamic ownership, and unsupported selector forms set
  explicit boundary flags and refusal reasons. The query does not invent an AD
  lowering model.
- **SELECT TYPE arm query**: `query_control_statement` also returns one
  `select_type_arm_query_t` per `TYPE IS`, `CLASS IS`, or `CLASS DEFAULT` arm.
  Each record preserves selector identity, one-based arm ordinal, arm kind,
  source location, and body entry/exit boundaries. Derived guard identities and
  the selector's declared derived identity are reported only when they resolve.
  For `SELECT TYPE (alias => selector)`, `is_selector_associate` is true,
  `selector_associate_name` and `selector_associate_node_index` identify the
  source alias, and `selector_expression_node_index` identifies the original
  selector expression. `selector_node_index` remains the parser's real
  pointer-assignment node; `selector_name` and storage facts describe the
  source selector when it resolves. Direct selectors set the expression index
  to their existing selector node and leave the associate-name fields unset.
  No AST node, declaration, rank, or shape fact is fabricated for an unresolved
  alias target.
  intrinsic guards, ambiguous or unresolved names, non-polymorphic selectors,
  and guards outside the selector's `EXTENDS` hierarchy carry explicit
  `is_intrinsic`, `is_ambiguous`, `is_unresolved`, `is_invalid`, or
  `is_out_of_hierarchy` flags and a refusal reason. No runtime type or guard
  relationship is guessed.
- **SELECT TYPE branch type query**: `query_select_type_branch(arena,
  arm_node_index)` exposes the narrowing predicate needed by a differentiation
  backend. `SELECT_TYPE_MATCH_EXACT` means `TYPE IS` and an exact dynamic type;
  `SELECT_TYPE_MATCH_EXTENSION` means `CLASS IS` and the named type or one of
  its extensions; `SELECT_TYPE_MATCH_DEFAULT` is `CLASS DEFAULT`. The query
  also reports whether the guard is the selector's declared type, an extension,
  or an invalid base/out-of-hierarchy relation, plus abstract guard identity.
  Intrinsic, ambiguous, unresolved, and unsupported relations remain
  `is_refused` with `match_kind=SELECT_TYPE_MATCH_UNKNOWN`; no concrete runtime
  type is invented.
- **SELECT TYPE component-path query**:
  `query_select_type_component_path(arena, arm_node_index,
  component_node_index)` maps a direct component access in a resolved branch
  body to ordered component declaration and terminal storage facts, including
  when the root is a `SELECT TYPE (alias => selector)` associate. It requires
  a concrete guard and refuses `CLASS DEFAULT`, accesses from a different or
  nested SELECT TYPE arm, unknown components, and pointer, allocatable, or
  polymorphic intermediate storage; no alias ownership or dynamic type is
  inferred.
- **SELECT TYPE component-binding query**:
  `query_select_type_component_binding(arena, arm_node_index,
  component_node_index, binding_name)` composes a resolved narrowed component
  path with the terminal component's effective `EXTENDS` binding. It reports
  inherited concrete implementations and their declaring type, while refusing
  unresolved, generic, ambiguous, or deferred bindings and pointer,
  allocatable, polymorphic, non-derived, or abstract component boundaries. It
  does not perform generic argument matching or runtime dispatch.
- **SELECT TYPE component direct-dispatch query**:
  `query_select_type_component_dispatch(arena, arm_node_index, call_node_index)`
  resolves one explicit non-generic `CALL` through a narrowed component path.
  It preserves the source-backed receiver path and exposes the terminal
  component type, inherited or local implementation, effective PASS metadata,
  and ordered implementation signature for a scalar concrete component or one
  contiguous rank-one component section with integer-literal bounds and unit
  stride. The section result exposes `is_array_section_receiver`,
  `array_section_rank`, `array_section_lower_bound`,
  `array_section_upper_bound`, and `array_section_stride`. Generic, nested,
  unresolved, pointer, TARGET, allocatable, polymorphic, dynamic, non-unit-
  stride, higher-rank, array-element, alias, mutable-global, and
  ownership-changing cases remain explicit refusals without a guessed target.
- **Concrete SELECT TYPE dispatch query**:
  `query_select_type_dispatch(arena, arm_node_index, call_node_index)`
  composes one direct `CALL selector%binding(...)` in a concrete `TYPE IS` or
  `CLASS IS` arm. It returns selector and guard identities, the concrete and
  declaring types, inherited status, binding and implementation nodes, PASS or
  NOPASS metadata, and the implementation's ordered `signature` facts. Arm
  body and source boundaries remain available alongside the call location;
  `dispatch_boundary_known` copies the existing SELECT TYPE arm boundary fact.
  An inherited implementation is accepted when its passed-object type is an
  ancestor of the concrete guard type, while an unresolved hierarchy or
  ownership-changing selector returns no implementation target.
  `is_refused` and `is_unresolved` retain explicit reasons for `CLASS DEFAULT`,
  deferred, generic, ambiguous, unresolved, incompatible-PASS, nested,
  dynamic, array, and ownership-changing cases. The query requires the call
  to be the arm's sole direct statement and never invents a runtime target.
- **Owned-array CLASS IS dynamic identity query**:
  `query_select_type_owned_array(arena, arm_node_index)` proves one direct
  `CLASS IS` arm's exact mapping from a local `STORAGE_OWNED` allocatable
  `class(abstract_type)` array to its concrete guard type. The result carries
  selector declaration identity, positive rank, declared and dynamic type
  indices/names, and the selector storage record. `TYPE IS`, `CLASS DEFAULT`,
  abstract guards, associate or pointer/TARGET aliases, borrowed dummies,
  module/SAVE/COMMON state, and control-flow-nested arms remain refusal-only
  boundaries through `has_unresolved_alias`,
  `has_global_mutable_state`, and `has_control_flow_boundary`; no dynamic
  type is guessed for them.
- **SELECT TYPE type-bound generic dispatch query**:
  `query_select_type_generic_dispatch(arena, arm_node_index, call_node_index)`
  enumerates the specifics of a generic `CALL selector%generic(...)` after a
  concrete `TYPE IS` or `CLASS IS` narrowing. Each candidate carries its
  implementation node/name, exact-match status, and ordered procedure
  signature. `is_resolved` is set only for one exact type/kind/rank match;
  ambiguous, zero-match, deferred, unresolved, dynamic, array, pointer, and
  allocatable cases remain explicit refusals with no selected implementation.
- **SELECT TYPE component type-bound generic dispatch query**:
  `query_select_type_component_generic_dispatch(arena, arm_node_index,
  call_node_index)` is the downstream bridge for an explicit call such as
  `CALL typed%leaf%choose(value)` in a concrete narrowed arm. It preserves the
  source-backed component path and terminal static type, enumerates every
  specific with implementation node/name, exact-match status, and ordered
  signature facts, and copies the selected candidate and signature only when
  exactly one specific matches the actual arguments by semantic type, kind,
  and rank. Ambiguous, zero-match, deferred, unresolved, pointer,
  allocatable, polymorphic, array, and dynamic paths remain explicit refusals;
  no runtime implementation is guessed.
- **Bounded procedure-pointer call target query**:
  `query_procedure_call_target` reports a complete fact only for a direct
  call through a declared procedure pointer with exactly one unconditional,
  same-scope, direct `=>` assignment before the call. The result preserves
  the call/pointer occurrence, pointer declaration, assignment, target
  expression, lexical scope, and resolved internal/external target identity
  (the AST stores a direct call's callee name on the call node, so the call
  and pointer occurrence share that node index). `found=.false.` with
  `is_unresolved=.true.` is returned when that proof is unavailable, including
  branch-local assignments, `NULLIFY`, `NULL()`, and other flow-sensitive
  cases. `has_reassignment=.true.` identifies the narrower refusal where two
  or more direct same-scope pointer assignments touch the pointer; `NULLIFY`
  does not set that flag. Generic calls are not callback facts. Resolved calls
  carry the same `signature` record as `query_procedure_target`, so a backend
  can compare ordered formal metadata without re-walking the AST. External
  targets remain identity-only when their interface is unavailable. This is a
  bounded identity/signature query, not general callback analysis or AD policy.
- **Branch-merged procedure-pointer callback flow**:
  `query_procedure_callback_flow` accepts only a direct pointer call after one
  same-scope `IF/ELSE`: each arm must contain exactly one direct assignment to
  the declared pointer, and both targets must be directly resolved internal
  procedures with matching ordered signatures. The result preserves the
  pointer/call identity, IF arm and merge boundaries, and source-ordered target
  records. `is_unresolved` and `is_refused` distinguish an incomplete proof.
  explicit flags cover loops, nested or missing branches, reassignment,
  missing arm assignments, `NULL()`/`NULLIFY`, generic or ambiguous targets,
  incompatible signatures, and calls inside an arm. `has_missing_assignment`
  identifies an arm with no direct assignment; `has_reassignment` is reserved
  for two or more assignments. Multi-line counted, concurrent, and `DO WHILE`
  constructs inside either arm are retained by the procedure parser and set
  `has_loop`/`is_refused` before any callback target is exposed. No callback
  target is guessed. The alias
  `query_procedure_pointer_callback_flow` exposes the same contract.

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
