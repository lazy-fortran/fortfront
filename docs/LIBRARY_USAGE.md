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

## Example: Unused Variable Linter

```fortran
module unused_var_linter
    use fortfront_tooling
    use fortfront_ast
    implicit none
    private
    public :: check_unused_variables

    type, extends(ast_visitor_base_t) :: unused_var_visitor_t
        character(len=32), allocatable :: declared_vars(:), used_vars(:)
        integer :: n_declared, n_used
    contains
        procedure :: visit_variable_decl => visit_decl
        procedure :: visit_identifier => visit_ident
    end type

contains
    subroutine check_unused_variables(source_file, unused_names)
        character(len=*), intent(in) :: source_file
        character(len=32), allocatable, intent(out) :: unused_names(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(unused_var_visitor_t) :: visitor

        call tooling_load_ast_from_file(source_file, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) then
            allocate(unused_names(0))
            return
        end if

        allocate(visitor%declared_vars(100), visitor%used_vars(100))
        visitor%n_declared = 0
        visitor%n_used = 0
        call traverse_ast(arena, root_index, visitor)
        ! Compare declared vs used to find unused variables
    end subroutine

    subroutine visit_decl(this, node)
        class(unused_var_visitor_t), intent(inout) :: this
        type(variable_decl_node), intent(in) :: node
        this%n_declared = this%n_declared + 1
        this%declared_vars(this%n_declared) = node%name
    end subroutine

    subroutine visit_ident(this, node)
        class(unused_var_visitor_t), intent(inout) :: this
        type(identifier_node), intent(in) :: node
        this%n_used = this%n_used + 1
        this%used_vars(this%n_used) = node%name
    end subroutine
end module
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
