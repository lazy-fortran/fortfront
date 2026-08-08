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
