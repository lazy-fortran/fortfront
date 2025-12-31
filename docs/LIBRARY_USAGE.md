# FortFront Library Usage Guide

## Quick Start

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

    if (len(error_msg) == 0) then
        print '(a)', output
    else
        print '(a)', 'Transformation failed: ' // error_msg
    end if
end program minimal_example
```

## AST Node Position API

Get source locations for AST nodes (useful for linters and diagnostics):

```fortran
program position_example
    use fortfront, only: ast_arena_t, create_ast_arena, get_node_line, &
                         get_node_column, get_node_location, &
                         tooling_load_ast_from_string
    implicit none
    type(ast_arena_t) :: arena
    integer :: root_index, line, col
    character(len=:), allocatable :: error_msg

    arena = create_ast_arena()
    call tooling_load_ast_from_string("x = 5", arena, root_index, error_msg)

    ! Standalone functions (for fluff integration)
    line = get_node_line(arena, root_index)
    col = get_node_column(arena, root_index)
    print '(a,i0,a,i0)', 'Position: line ', line, ', column ', col

    ! Alternative: get both at once
    call get_node_location(arena, root_index, line, col)

    ! Type-bound procedures also available
    line = arena%get_node_line(root_index)
    col = arena%get_node_column(root_index)
end program
```

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

Direct trivia query at an arbitrary source location:
`get_source_trivia_at(source, line, column)`.

## Example: AST Node Counter

Count nodes of each type using callback-based traversal:

```fortran
program count_nodes
    use fortfront, only: ast_arena_t, create_ast_arena, tooling_load_ast_from_string, &
                         traverse_ast, get_node_type_at, node_exists
    implicit none
    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg

    arena = create_ast_arena()
    call tooling_load_ast_from_string("x = 5 + 3", arena, root_index, error_msg)

    if (len(error_msg) > 0) then
        print '(a)', 'Error: ' // error_msg
        stop 1
    end if

    call traverse_ast(arena, root_index, count_callback)

contains
    subroutine count_callback(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: node_type

        if (node_exists(arena, node_index)) then
            node_type = get_node_type_at(arena, node_index)
            print '(a,i0,a,a)', 'Node ', node_index, ': ', node_type
        end if
    end subroutine count_callback
end program
```

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

## See Also

- `examples/` - Additional code samples
- `src/interfaces/` - C API bindings
