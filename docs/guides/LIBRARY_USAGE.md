# FortFront Library Usage Guide

## Quick Start

FortFront has three practical integration levels:

- Transformation APIs: Lazy Fortran or standard Fortran input to emitted
  standard Fortran text.
- Tooling APIs: parse source to an arena/root index, optionally running
  semantic analysis.
- Compiler APIs: parse and analyze source into an owned frontend result without
  running standardization or Fortran code generation.

It does not expose backend IR, object emission, executable emission, or a
complete C ABI for AST traversal. Downstream compiler work should use the
Fortran compiler API and lower to LIRIC or another backend outside FortFront.

### Project Setup with fpm

```toml
name = "my-tool"
version = "0.1.0"

[dependencies]
fortfront = { path = "../fortfront" }
```

### Minimal Example

```fortran
program minimal_example
    use fortfront_transform, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg

    input = "x = 5"
    call transform_lazy_fortran_string(input, output, error_msg)

    if (.not. allocated(error_msg) .or. len_trim(error_msg) == 0) then
        print '(a)', output
    else
        print '(a)', 'Transformation failed: ' // error_msg
    end if
end program minimal_example
```

## AST Node Position API

Get source locations for AST nodes (useful for linters and diagnostics):

Example program: [examples/f90/library_usage_ast_node_position.f90](../../examples/f90/library_usage_ast_node_position.f90).

**Available functions**:
- `get_node_line(arena, index)` - Returns line number (1-based), 0 if invalid
- `get_node_column(arena, index)` - Returns column number (1-based), 0 if invalid
- `get_node_location(arena, index, line, col)` - Subroutine returning both

## CST Trivia Query API

Retrieve whitespace/comments/newlines adjacent to an AST node (for whitespace-aware
linting and formatting tools):

```fortran
use fortfront_tooling, only: tooling_load_ast_from_string, ast_arena_t, &
    get_trivia_for_ast_node, trivia_t

type(ast_arena_t) :: arena
integer :: root_index
character(len=:), allocatable :: error_msg
type(trivia_t), allocatable :: leading(:), trailing(:)
logical :: found
character(len=*), parameter :: source = "! header" // new_line('A') // &
    "   x = 1"

call tooling_load_ast_from_string(source, arena, root_index, error_msg)
call get_trivia_for_ast_node(source, arena, root_index, leading, trailing, found)
```

For repeated queries over the same source, tokenize once and reuse:

```fortran
use fortfront_tooling, only: tokenize_core_with_trivia, token_t, &
    get_trivia_for_ast_node_tokens

type(token_t), allocatable :: tokens(:)

call tokenize_core_with_trivia(source, tokens)
call get_trivia_for_ast_node_tokens(tokens, arena, root_index, leading, trailing, found)
```

Direct trivia query at an arbitrary source location:
`get_source_trivia_at(source, line, column)`.

## Example: AST Node Counter

Count nodes of each type using callback-based traversal:

Example program: [examples/f90/library_usage_ast_node_counter.f90](../../examples/f90/library_usage_ast_node_counter.f90).

## Structured Diagnostics API

```fortran
use frontend_diagnostics, only: make_diagnostic, format_diagnostic, &
    DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR
use fortfront_types, only: diagnostic_t, source_range_t

type(diagnostic_t) :: diag
type(source_range_t) :: location

location%start%line = 42
location%start%column = 15
diag = make_diagnostic(DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR, &
    "Unexpected token", location)
print '(a)', format_diagnostic(diag)
! Output: [F004] ERROR at line 42:15: Unexpected token
```

**Diagnostic Codes**: F001 (empty input), F002 (binary data), F003 (lexical error), F004 (syntax error), F005 (semantic error), F006 (parse error), F007 (no program unit)

**Severity Levels**: DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO, DIAGNOSTIC_HINT

## Error Handling Patterns

```fortran
! Pattern 1: Check allocatable error strings
character(len=:), allocatable :: error_msg
call some_api_function(..., error_msg)
if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
    print '(a)', "Error: " // trim(error_msg)
    return
end if

! Pattern 2: Use result types
type(result_t) :: result
result = some_operation(...)
if (.not. result%success) then
    print '(a)', "Failed: " // trim(result%error_message)
end if
```

## Performance Tips

**Reuse arenas for batch processing**:
```fortran
type(tooling_parse_options_t) :: options
options%reuse_arena = .true.
do i = 1, num_files
    call tooling_load_ast_from_file(files(i), arena, root_index, error_msg, options)
end do
```

**Thread safety**: APIs are reentrant with separate contexts. Use one arena per thread:
```fortran
!$omp parallel private(arena, root_index, error_msg)
    !$omp do
    do i = 1, num_files
        call tooling_load_ast_from_file(files(i), arena, root_index, error_msg)
    end do
    !$omp end do
!$omp end parallel
```

## Linking

**With fpm**: Add fortfront as dependency in fpm.toml

**Manual static library**:
```sh
cd fortfront && fpm build --flag "-fPIC"
ar rcs libfortfront.a build/gfortran_*/fortfront/lib/*.o
gfortran -o my_tool my_tool.f90 -L./fortfront -lfortfront
```

## C/Python Integration

Use the ISO_C_BINDING bridge in `src/interfaces/fortfront_c_interface.f90`:
- `fortfront_parse_source_c`
- `fortfront_get_last_error_c`
- `fortfront_get_version_c`

This bridge currently validates/parses via the transformation path and reports
status/errors. It does not return transformed source, AST handles, semantic
contexts, or diagnostics arrays.

## Compiler Integration Status

The intended compiler boundary is:

1. Parse source into an arena and root index.
2. Run semantic analysis and collect diagnostics.
3. Return the typed AST plus semantic data to the compiler driver through
   `fortfront_compiler`.
4. Let the compiler driver lower to LIRIC or another backend IR outside
   FortFront.

The compiler API is Fortran-only today. A full C ABI for typed AST traversal is
not implemented.

Consumers using the unified `fortfront` facade can query array-bound and range
nodes without importing concrete AST node types:

```fortran
use fortfront, only: array_bounds_query_t, query_array_bounds

type(array_bounds_query_t) :: bounds

bounds = query_array_bounds(result%arena, bounds_index)
if (bounds%found) print *, bounds%lower_bound_node_index
```

`query_range_expression` provides the corresponding lower, upper, and stride
indices for explicit ranges. These read-only records are the stable boundary
for consumers that simplify shapes or loop bounds.

## Procedure-pointer ASSOCIATED and NULLIFY state

`query_procedure_pointer_state(arena, node_index)` recognizes a unary
`ASSOCIATED(pointer)` expression or a `NULLIFY(pointer)` statement when the
operand resolves to a declared procedure pointer. A direct single-pointer
`NULLIFY` is a known disassociation transition. The unary `ASSOCIATED` result
sets `state_known` only for one direct same-scope pointer assignment, with at
most one direct `NULLIFY` before the observation and no nested mutation; its
`is_associated` value and source assignment/nullify node identities are then
safe for a backend to consume.

The query is deliberately refusal-first. Reassignment, branch-local or
indirect mutation, aliases (including host-associated and procedure-pointer
dummy bindings), global mutable state, control-flow observations, unresolved
targets, component pointers, non-procedure pointers, invalid arity, and the
two-argument `ASSOCIATED` form set `is_refused`/`is_unresolved` and the
corresponding boundary flags. `has_alias` identifies the alias boundary.
`found` means that the source operation was recognized; a consumer must also
require `state_known` before using the state.
See
`examples/f90/procedure_pointer_association_query.f90` and
`test/api/test_procedure_pointer_state_query.f90` for the GNU runtime and
independent expected-facts oracle.

## SELECT TYPE associate selectors

`query_control_statement` exposes `select_type_arm_query_t` records for each
`TYPE IS`, `CLASS IS`, and `CLASS DEFAULT` arm. For
`SELECT TYPE (typed => object)`, `is_selector_associate` is true;
`selector_associate_name` and `selector_associate_node_index` identify `typed`,
while `selector_expression_node_index` identifies the existing AST node for
`object`. `selector_node_index` remains the parser's pointer-assignment node,
and `selector_name` plus storage facts refer to the source selector when it is
resolved. Direct selectors use their existing node as the expression index and
leave associate fields unset. Unresolved targets do not receive invented AST,
declaration, rank, or shape facts.

## SELECT TYPE branch type predicates

`query_select_type_branch(arena, arm_node_index)` returns the source-backed
runtime predicate for one `SELECT TYPE` arm. `SELECT_TYPE_MATCH_EXACT` denotes
`TYPE IS`; `SELECT_TYPE_MATCH_EXTENSION` denotes `CLASS IS` and includes the
named type's extensions; and `SELECT_TYPE_MATCH_DEFAULT` denotes `CLASS
DEFAULT`. The result also identifies whether the guard is the selector's
declared type or an extension, and flags abstract guards. Intrinsic, unresolved,
ambiguous, and out-of-hierarchy cases are explicit refusals with
`SELECT_TYPE_MATCH_UNKNOWN`; consumers must not infer a concrete runtime type
from them.

## SELECT TYPE direct dispatch boundaries

`query_select_type_dispatch(arena, arm_node_index, call_node_index)` resolves
one direct type-bound call in a concrete `TYPE IS` or `CLASS IS` arm. For a
concrete leaf, the result preserves the effective implementation and its
declaring type, including an implementation inherited through an abstract
intermediate or base type. `is_inherited` and `declaring_type_name` identify
that provenance without requiring a consumer to rebuild the `EXTENDS` chain.

An abstract `CLASS IS` guard remains a refusal because it matches a set of
possible concrete extensions. Such a result sets `is_abstract_guard`,
`is_unresolved`, and `is_refused`, and deliberately leaves
`implementation_node_index` unset; FortFront never exposes the guard's static
binding as a guessed runtime target. The focused GNU runtime/API oracle is
`examples/f90/abstract_dispatch_runtime_boundary.f90` with
`test/api/test_abstract_dispatch_runtime_boundary.f90`.

## Owned-array CLASS IS dynamic identity

`query_select_type_owned_array(arena, arm_node_index)` is the bounded bridge
from a direct `CLASS IS` arm to an owned polymorphic array. A resolved result
proves the exact mapping from the selector declaration's abstract
`class(base_t), allocatable :: values(:)` storage to the concrete guard type,
including the selector declaration identity, rank, storage class, declared
type index/name, and dynamic type index/name. This is a source-order identity
fact for consumers such as FortAD; it does not inspect runtime elements or
infer a derivative.

The query resolves only a direct `CLASS IS` arm over a local, allocatable,
rank-positive `class(abstract_type)` array with `STORAGE_OWNED` storage and a
concrete guard type. `TYPE IS`, `CLASS DEFAULT`, abstract guards, associate or
pointer/TARGET aliases, borrowed dummies, module/SAVE/COMMON state, and arms
containing or nested in control flow remain explicit refusals through
`is_refused`, `is_unresolved`, and the corresponding alias, global-state, or
control-flow flags. No dynamic identity fields are populated as a guess at
those boundaries.

See `examples/f90/owned_array_class_is_dynamic_identity.f90` and
`test/api/test_owned_array_class_is_identity.f90` for the GNU syntax and
independent expected-facts oracle.

`query_select_type_owned_array_binding(arena, arm_node_index, binding_name)`
extends that proof to one type-bound binding. The result keeps
`declared_binding` (which can be the abstract type's deferred binding) and
`dynamic_binding` (the concrete guard type's local or inherited implementation)
separate. `is_resolved` is true only when the dynamic binding is non-generic,
non-ambiguous, non-deferred, and has a source-resolved implementation procedure;
`implementation_node_index`, `declaring_type_name`, and `is_inherited` identify
that target for consumers such as FortAD. The owned-array refusal flags and
storage boundaries are propagated unchanged, so aliases, globals, and
control-flow-nested arms never receive a guessed binding target.

See `examples/f90/owned_array_class_is_binding_identity.f90` and
`test/api/test_owned_array_class_is_binding.f90` for an independent oracle
covering a deferred abstract binding, a direct implementation, an inherited
implementation, and refusal boundaries.

`query_select_type_owned_array_generic_dispatch(arena, arm_node_index,
call_node_index)` resolves a type-bound generic called on an owned-array
element such as `values(i)%choose(value)`. It maps the source receiver back to
the selector's storage declaration, retains the array-element designator, and
exposes candidates, the unique selected procedure, ordered signature, and
default or named PASS position only for one exact type/kind/rank match.
Ambiguous, zero-match, deferred, unresolved, non-element, global, alias, and
control-flow cases remain explicit refusals. See
`examples/f90/owned_array_class_is_generic_dispatch.f90` and
`test/api/test_owned_array_class_is_generic_dispatch.f90` for the independent
oracle.

For an owned-array generic, PASS metadata is resolved per specific rather than
copied from the generic binding. Each candidate reports
`pass_metadata_resolved`, `pass_arg`, `pass_name`, and `pass_position`; the
selected candidate's facts are repeated as `selected_pass_*`. This preserves a
valid binding such as `PASS(self)` when the passed-object dummy is not the
first procedure dummy. The existing top-level `pass_*` fields continue to
describe the generic binding; consumers needing the callable specific should
use `selected_pass_*`. Missing specific PASS metadata remains an unresolved
candidate, and no target is selected through that boundary.

`query_select_type_owned_array_dispatch(arena, arm_node_index,
call_node_index)` is the direct-binding counterpart for an explicit call such
as `values(i)%run(value)`. It maps the indexed receiver to the owned selector's
storage declaration, then reports the concrete or inherited implementation,
ordered implementation signature, and effective PASS dummy/position. The
`receiver_storage` record intentionally remains the array owner; consumers use
`is_array_element_receiver` to distinguish the scalar element designator from
that owner. Generic bindings stay refused and must use the generic dispatch
query. Global, alias, and control-flow-nested arms retain their refusal flags
and never receive a guessed target.

See `examples/f90/owned_array_class_is_dispatch.f90` and
`test/api/test_owned_array_class_is_dispatch.f90` for the independent oracle.

## SELECT TYPE component paths

`query_select_type_component_path(arena, arm_node_index, component_node_index)`
maps a component access in a resolved `SELECT TYPE` arm to ordered component
declaration and terminal storage facts. This is the bounded query for a branch
associate such as `typed%payload`: the path is rooted in the arm's narrowed
concrete type even though the ordinary component query conservatively refuses
the associate name. The result is resolved only for a direct arm-body access
whose intermediate components are scalar, non-polymorphic, non-pointer, and
non-allocatable. `CLASS DEFAULT`, accesses from a different or nested
`SELECT TYPE` arm, unknown components, and ownership or dynamic-type
boundaries are explicit refusals.

## SELECT TYPE component bindings

`query_select_type_component_binding(arena, arm_node_index,
component_node_index, binding_name)` resolves the effective type-bound binding
of the terminal type in a previously resolved narrowed component path. For
example, a `typed%leaf` path whose `leaf_t` extends an implementation type can
report the inherited concrete implementation and its declaring type. The
query refuses unresolved, generic, ambiguous, or deferred bindings and
pointer, allocatable, polymorphic, non-derived, or abstract component
boundaries; it never guesses a runtime target or performs generic argument
matching.

## SELECT TYPE component direct dispatch

`query_select_type_component_dispatch(arena, arm_node_index, call_node_index)`
resolves one explicit non-generic call such as
`CALL typed%leaf%run(value)` in a concrete `SELECT TYPE` arm. The result keeps
the source-backed component path and reports the terminal component type,
inherited or local implementation, effective PASS metadata, and ordered
procedure signature. It requires the call to be the arm's sole direct
statement; generic bindings must use the generic-dispatch query instead.

The bounded array extension also resolves exactly one contiguous rank-one
component section with integer-literal lower and upper bounds and unit stride,
for example `CALL typed%leaf_section(2:4)%run(value)`. The result sets
`is_array_section_receiver`, `is_literal_array_section`, and
`is_contiguous_array_section`, reports `array_section_rank` and the three
`array_section_*` values, and retains the element type and inherited binding.
Pointer/TARGET aliases, allocatable or polymorphic components, dynamic or
non-unit-stride sections, higher-rank sections, array elements, mutable global
state, unresolved bindings, nested calls, and ownership-changing selectors
remain explicit `is_refused`/`is_unresolved` boundaries with no implementation
target. See `examples/f90/select_type_component_dispatch.f90` and
`test/api/test_select_type_component_dispatch.f90` for the independent oracle.

## Defined-operator exact dispatch

`query_defined_operator(arena, operator_node_index)` is the compiler-facing
query for a user-defined unary or binary operator expression. It accepts the
arena index of a `binary_op_node`, finds visible same-arena
`INTERFACE OPERATOR(...)` blocks, and exposes one
`defined_operator_candidate_query_t` per concrete specific. Each candidate's
`operands` array preserves the actual/formal node identities and the semantic
type category, exact kind, rank, and derived-type name on both sides.

`is_resolved` and `selected_procedure_node_index` are set only when exactly one
candidate has an exact operand signature. The query deliberately does not
apply Fortran implicit conversion or extension-type compatibility. Ambiguous
specifics, invalid arity, unknown or polymorphic types, pointer/TARGET
operands, and mutable global state in either the operands or the selected
procedure's body remain `is_refused`/`is_unresolved` facts through
`is_ambiguous`, `has_conversion`, `has_unknown_types`,
`has_pointer_operand`, `has_global_mutable_state`, and
`has_invalid_arity`, with `refusal_reason` identifying the first boundary.
This lets FortAD consume a concrete operator procedure without duplicating
generic resolution while keeping unsupported state explicit. See
`examples/f90/defined_operator_query.f90` and
`test/api/test_defined_operator_query.f90` for the GNU-checked independent
oracle.

## SELECT TYPE type-bound generic dispatch

`query_select_type_generic_dispatch(arena, arm_node_index, call_node_index)`
resolves a type-bound generic only after a concrete `TYPE IS` or `CLASS IS`
guard has narrowed the receiver. It enumerates every same-arena specific in
`candidates`, including its procedure node, implementation name, ordered
`signature`, and exact-match flag. `is_resolved` is true only when exactly one
specific matches every supplied actual by semantic type, kind, and rank; the
selected candidate and signature are then copied to the top-level result.

The query reports `is_ambiguous` for multiple exact specifics and retains
`is_unresolved`/`is_refused` with a reason for zero matches, missing
implementations, deferred or unresolved bindings, dynamic or array receivers,
and pointer or allocatable selector storage. Generic bindings are therefore
supported only through an independently provable specific selection; no
runtime target or implicit conversion is guessed.

## SELECT TYPE component type-bound generic dispatch

`query_select_type_component_generic_dispatch(arena, arm_node_index,
call_node_index)` is the downstream bridge from a narrowed component path to a
generic call. For an explicit call such as
`CALL typed%leaf%choose(value)` in a concrete `SELECT TYPE (typed => object)`
arm, `receiver_path` preserves the source-backed component names and
declaration identities, while `component_type_name` identifies the terminal
static type. `candidates` contains every same-arena specific with its
implementation node/name, exact-match flag, and ordered `signature`; the
selected candidate and signature are copied to the top-level result only when
exactly one candidate matches the actual arguments by semantic type, kind, and
rank.

The query refuses ambiguous or zero-match generics, deferred or unresolved
bindings, and component paths crossing pointer, allocatable, polymorphic,
array, or dynamic storage boundaries. `is_refused` and `is_unresolved` remain
set with a reason, and no runtime implementation is selected in those cases.

## Resolved Expression Type Query

`compile_frontend_from_string` and `compile_frontend_from_file` annotate the
analyzed arena with exact compiler-facing type metadata. Query an expression by
arena index through `fortfront_compiler`:

```fortran
use fortfront_compiler, only: resolved_type_query_t, query_resolved_type

type(resolved_type_query_t) :: resolved

resolved = query_resolved_type(result%arena, expression_index)
if (.not. resolved%found) then
    print '(a)', resolved%diagnostic
    return
end if
```

The result fields have the following contract:

- `type_kind` is the intrinsic category constant (`TINT`, `TREAL`, `TLOGICAL`,
  `TCOMPLEX`, or `TCHAR`) or `TDERIVED`. Double precision is reported as the
  real category with `kind_value == 8`.
- `kind_value` is the exact resolved Fortran kind selector. Numeric selectors
  and visible integer named constants remain distinct, including `real(8)` and
  `real(16)` or `integer, parameter :: wp = 16; real(wp) :: x`.
- `storage_size_bits` is the scalar storage size represented by FortFront's
  kind mapping. Complex storage includes both real components. A zero value
  means that FortFront has no storage mapping.
- `rank` is zero for a scalar and positive for an array expression.
- `derived_type_name` identifies a resolved derived type and is empty for an
  intrinsic type.
- `found` is false, with `diagnostic` populated, when exact semantic type
  resolution is unavailable.

The annotation covers literals, declaration and identifier references, unary
and binary expressions, function results, intrinsic calls whose result kind is
recorded, and component references whose declaration is visible in the arena.
Mixed numeric expressions use Fortran category promotion while preserving the
resolved real or complex operand kind. The query itself reads semantic metadata;
it does not parse source text. If compiler options disable semantic analysis,
exact type queries are unavailable.

## Bounded polymorphic allocation facts

Ownership events expose `event%polymorphic_allocation`; the same record is
available directly through `query_polymorphic_allocation`. For a direct scalar
`allocate(owner, source=concrete_child)`, it reports the owner node and
component path, declaration indices, declared owner type (`class(base_t)` or
`class(*)`), source expression index/path, resolved concrete source type, and
`POLYMORPHIC_SOURCE_CONCRETE`. `is_bounded` is true only for this bounded
case. Factories, dynamic polymorphic sources, repeated acquisition, and
aliases remain explicit unknown or refusal facts rather than being guessed;
use `source_classification` and the corresponding `is_*` flags to branch.

## Deep derived assignment ownership facts

`query_ownership_events(arena, scope_index)` includes intrinsic assignments of
whole, statically concrete derived objects when the declared type owns an
allocatable component, including one nested in another derived component or
inherited from a parent type. Such an event has
`is_deep_assignment=.true.`, `has_owned_components=.true.`, and
`assignment_kind == OWNERSHIP_ASSIGNMENT_DEEP_DERIVED`. Its source and
destination operand paths are available through `rhs_owner_path` and
`lhs_owner_path`; `reallocation_kind` remains
`OWNERSHIP_REALLOCATION_NONE` unless the destination itself is allocatable.

The query does not guess across mutable global state or possible aliases.
Assignments involving module, `SAVE`, or `COMMON` state set
`has_global_mutable_state` and `is_refused`; pointer, `TARGET`, and
`ASSOCIATE`-selector operands set `has_unresolved_alias` and `is_refused`.
The event remains visible so a consumer can distinguish a known deep-copy
operation from a transformation refusal. Polymorphic, array-section, and
unresolved operands are not classified as deep assignments.

See `examples/f90/ownership_deep_assignment_facts.f90` and
`test/api/test_ownership_deep_assignment.f90` for the GNU syntax and
independent expected-facts oracle.

## Polymorphic allocatable assignment and replay facts

`query_polymorphic_assignment_into(arena, assignment_node_index, query)` writes
the bounded fact for an intrinsic assignment whose destination is a
polymorphic allocatable derived object. The function form,
`query_polymorphic_assignment`, remains a compatibility wrapper. The same
record is attached to the corresponding `ownership_event_query_t` as
`polymorphic_assignment`. It retains exact
source/destination paths and declaration identities, reports recursively
whether the concrete source type owns allocatable components, and sets
`is_replayable` only when the source type is statically compatible with the
declared destination type and no global state, alias, control-flow, or
polymorphic-source boundary is present. For a replayable assignment,
`dynamic_type` is the concrete source type and the ownership event carries the
same type as its destination dynamic type. `class(*)` accepts any concrete
source; extension-to-base assignment is checked through the existing type
hierarchy. A polymorphic source or incompatible type remains an explicit
refusal with no guessed dynamic type.

See `examples/f90/polymorphic_assignment_replay_facts.f90` and
`test/api/test_polymorphic_assignment_replay.f90` for a runtime-backed GNU
semantic oracle that verifies both dynamic-type acquisition and deep-copy
isolation of an allocatable component. The out-argument form is the supported
choice across compiler/runtime boundaries because it avoids copying a result
with nested allocatable query fields.

## Ownership event sequence facts

`query_ownership_events` returns events in source order within the queried
scope. Each record's one-based `sequence_index` is stable for that result and
can be used to preserve ownership sequencing without relying on arena node
numbers. `ALLOCATE` has an unallocated owner precondition and an allocated
owner postcondition; `DEALLOCATE` has the inverse. An allocatable assignment
has an allocated owner postcondition and
`has_potential_implicit_reallocation=.true.` because automatic deallocation
and reallocation occur before assignment when required.

`MOVE_ALLOC(source, destination)` is an explicit transfer: the source is
unallocated afterward, the destination has the source's allocation state, and
`has_implicit_destination_deallocation=.true.` records that an allocated
destination is deallocated before the transfer. These are sequencing facts,
not runtime allocation-state guesses. The query adds no ownership for
pointer, `TARGET`, `ASSOCIATE`, mutable global, or unresolved alias paths.

See `examples/f90/ownership_event_sequence_facts.f90` and
`test/api/test_ownership_event_sequence.f90` for the independent sequence
oracle.

## Bounded ownership storage and dynamic-type identity

Ownership events also expose `source_declaration_index` and
`destination_declaration_index`, their resolved storage classes, and
`is_source_dynamic_type_known` / `is_destination_dynamic_type_known` with the
corresponding dynamic type names. For a direct whole-allocatable sequence,
these facts preserve the concrete type through `MOVE_ALLOC` and allocatable
reallocation even when the destination is declared `class(base_t)`; the
destination declaration identity is the storage owner after the transfer.
Array rank remains available through the existing `lhs_rank` and `rhs_rank`
facts.

This is a bounded source-order fact, not a runtime alias analysis. Module,
`SAVE`, and `COMMON` state, pointer or `TARGET` operands, component and
array-section/element paths, `ASSOCIATE` selectors, control-flow boundaries,
and unresolved operands set `is_refused` or `has_dynamic_type_boundary` and do
not receive a guessed destination type. A false dynamic-type-known flag is
therefore an explicit boundary for downstream ownership/lifetime consumers.

See `examples/f90/ownership_dynamic_identity_facts.f90` and
`test/api/test_ownership_dynamic_identity.f90` for the GNU fixture and
independent expected-facts oracle.

## ASSOCIATE selector facts

`query_associate_selectors(arena, associate_node_index)` returns facts for the
selector expressions in one ASSOCIATE construct. Each
`associate_selector_query_t` carries the selector expression index, association
name and ordinal, storage declaration identity, base node, component path, and
semantic type fields. `selector_storage` retains the existing storage facts,
while `selector_path` retains ordered component and component-declaration
indices.

The query is bounded at the construct body. `has_read_reference` and
`has_write_reference` describe direct uses of the association name. A call use
sets `has_ambiguous_access` and `is_alias_boundary`, because a downstream
transformer cannot infer the callee's dummy intent from this query. Direct
component and array-element designators set `is_alias`; pointer and
polymorphic selectors additionally set `is_alias_boundary` and leave dynamic
type facts unresolved. A non-designator expression has no storage identity and
is reported as `is_read_only`.

See `examples/f90/associate_selector_facts.f90` and
`test/api/test_associate_selector_facts.f90` for the GNU API contract and its
independent expected-facts oracle.

## See Also

- `examples/` - Additional code samples
- `src/interfaces/` - C API bindings
