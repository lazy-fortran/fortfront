# FortFront Library Usage Guide

## Quick Start

FortFront has two practical integration levels:

- Transformation APIs: Lazy Fortran or standard Fortran input to emitted
  standard Fortran text.
- Tooling APIs: parse source to an arena/root index, optionally running
  semantic analysis.

It does not yet expose a stable compiler IR, durable semantic-result object, or
complete C ABI for AST traversal. Downstream compiler work should use the
Fortran tooling APIs for now and track the compiler-facing API work before
building a large backend directly on FortFront internals.

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
use fortfront, only: tooling_load_ast_from_string, ast_arena_t, &
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
use fortfront, only: tokenize_core_with_trivia, token_t, get_trivia_for_ast_node_tokens

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
3. Return the typed AST plus semantic data to the compiler driver.
4. Let the compiler driver lower to its backend IR.

Today, step 3 is incomplete as a stable public contract. Existing APIs are
usable for experiments, but `ffc` should avoid depending on private AST layout
until that contract is formalized.

## See Also

- `examples/` - Additional code samples
- `src/interfaces/` - C API bindings
