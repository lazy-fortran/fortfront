# FortFront Library Usage Guide

This guide provides complete, working examples of using FortFront as a library in various downstream tools such as linters, compilers, formatters, and analyzers.

## Table of Contents

1. Quick Start
2. Example 1: Simple Linter (Unused Variables)
3. Example 2: Code Formatter
4. Example 3: Custom Compiler Backend
5. Example 4: AST Analysis Tool
6. Structured Diagnostics API
7. Error Handling Best Practices
8. Performance Optimization
9. FAQ

## Quick Start

### Project Setup with fpm

Create an fpm.toml that depends on fortfront:

```toml
name = "my-tool"
version = "0.1.0"
license = "MIT"
author = "Your Name"

[dependencies]
fortfront = { path = "../fortfront" }

[build]
auto-executables = true
auto-tests = true
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

Build and run:

```sh
fpm run minimal_example
```

## Example 1: Simple Linter (Unused Variables)

This example implements a linter that detects unused variables by traversing the AST.

```fortran
module unused_var_linter
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront_tooling
    use fortfront_ast
    use fortfront_lexer
    implicit none
    private

    public :: check_unused_variables

    type, extends(ast_visitor_base_t) :: unused_var_visitor_t
        character(len=32), allocatable :: declared_vars(:)
        character(len=32), allocatable :: used_vars(:)
        integer :: n_declared
        integer :: n_used
    contains
        procedure :: visit_variable_decl => visit_decl
        procedure :: visit_identifier => visit_ident
    end type unused_var_visitor_t

contains

    subroutine check_unused_variables(source_file, unused_names)
        character(len=*), intent(in) :: source_file
        character(len=32), allocatable, intent(out) :: unused_names(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=:), allocatable :: error_msg
        type(unused_var_visitor_t) :: visitor
        integer :: i, j, n_unused
        logical :: is_used

        call tooling_load_ast_from_file(source_file, arena, root_index, &
                                        error_msg)

        if (len_trim(error_msg) > 0) then
            print '(a)', "Parse error: " // trim(error_msg)
            allocate (unused_names(0))
            return
        end if

        allocate (visitor%declared_vars(100))
        allocate (visitor%used_vars(100))
        visitor%n_declared = 0
        visitor%n_used = 0

        call traverse_ast(arena, root_index, visitor)

        n_unused = 0
        do i = 1, visitor%n_declared
            is_used = .false.
            do j = 1, visitor%n_used
                if (trim(visitor%declared_vars(i)) == &
                    trim(visitor%used_vars(j))) then
                    is_used = .true.
                    exit
                end if
            end do
            if (.not. is_used) n_unused = n_unused + 1
        end do

        allocate (unused_names(n_unused))
        n_unused = 0
        do i = 1, visitor%n_declared
            is_used = .false.
            do j = 1, visitor%n_used
                if (trim(visitor%declared_vars(i)) == &
                    trim(visitor%used_vars(j))) then
                    is_used = .true.
                    exit
                end if
            end do
            if (.not. is_used) then
                n_unused = n_unused + 1
                unused_names(n_unused) = visitor%declared_vars(i)
            end if
        end do
    end subroutine check_unused_variables

    subroutine visit_decl(this, node)
        class(unused_var_visitor_t), intent(inout) :: this
        type(variable_decl_node), intent(in) :: node

        if (this%n_declared < size(this%declared_vars)) then
            this%n_declared = this%n_declared + 1
            this%declared_vars(this%n_declared) = node%name
        end if
    end subroutine visit_decl

    subroutine visit_ident(this, node)
        class(unused_var_visitor_t), intent(inout) :: this
        type(identifier_node), intent(in) :: node

        if (this%n_used < size(this%used_vars)) then
            this%n_used = this%n_used + 1
            this%used_vars(this%n_used) = node%name
        end if
    end subroutine visit_ident

end module unused_var_linter

program linter_example
    use unused_var_linter
    implicit none
    character(len=32), allocatable :: unused(:)
    integer :: i

    call check_unused_variables("input.f90", unused)

    if (size(unused) > 0) then
        print '(a)', "Unused variables:"
        do i = 1, size(unused)
            print '(2x,a)', trim(unused(i))
        end do
    else
        print '(a)', "No unused variables found."
    end if
end program linter_example
```

## Example 2: Code Formatter

This example implements a simple code formatter that standardizes indentation.

```fortran
program code_formatter
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront_tooling
    use fortfront_codegen
    use fortfront_ast
    implicit none

    character(len=256) :: input_file, output_file
    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg, formatted_code
    type(tooling_parse_options_t) :: options

    if (command_argument_count() /= 2) then
        print '(a)', "Usage: code_formatter <input.f90> <output.f90>"
        stop 1
    end if

    call get_command_argument(1, input_file)
    call get_command_argument(2, output_file)

    call set_indent_config(4)
    call set_line_length_config(88)

    options%run_semantics = .false.
    options%reuse_arena = .false.

    call tooling_load_ast_from_file(trim(input_file), arena, root_index, &
                                    error_msg, options)

    if (len_trim(error_msg) > 0) then
        print '(a)', "Error parsing input: " // trim(error_msg)
        stop 1
    end if

    formatted_code = generate_code_from_arena(arena)

    call write_output_file(trim(output_file), formatted_code)

    print '(a)', "Formatted code written to " // trim(output_file)

contains

    subroutine write_output_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: unit, stat

        open (newunit=unit, file=path, status='replace', action='write', &
              iostat=stat)
        if (stat /= 0) then
            print '(a)', "Error opening output file"
            stop 1
        end if

        write (unit, '(a)') content
        close (unit)
    end subroutine write_output_file

end program code_formatter
```

## Example 3: Custom Compiler Backend

This example shows how to use FortFront as the frontend for a custom compiler that emits intermediate representation.

```fortran
module custom_ir_emitter
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront_ast
    implicit none
    private

    public :: emit_ir

    type, extends(ast_visitor_base_t) :: ir_emitter_t
        character(len=:), allocatable :: ir_code
        integer :: temp_counter
        integer :: label_counter
    contains
        procedure :: visit_assignment => emit_assignment
        procedure :: visit_binary_op => emit_binary_op
        procedure :: visit_if => emit_if
    end type ir_emitter_t

contains

    function emit_ir(arena, root_index) result(ir_code)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable :: ir_code
        type(ir_emitter_t) :: emitter

        emitter%ir_code = ""
        emitter%temp_counter = 0
        emitter%label_counter = 0

        call traverse_ast(arena, root_index, emitter)

        ir_code = emitter%ir_code
    end function emit_ir

    subroutine emit_assignment(this, node)
        class(ir_emitter_t), intent(inout) :: this
        type(assignment_node), intent(in) :: node

        this%ir_code = this%ir_code // "STORE " // trim(node%lhs_name) // &
                       " = " // trim(node%rhs_expr) // char(10)
    end subroutine emit_assignment

    subroutine emit_binary_op(this, node)
        class(ir_emitter_t), intent(inout) :: this
        type(binary_op_node), intent(in) :: node
        character(len=16) :: temp_name

        this%temp_counter = this%temp_counter + 1
        write (temp_name, '(a,i0)') "t", this%temp_counter

        this%ir_code = this%ir_code // trim(temp_name) // " = " // &
                       trim(node%op) // " " // trim(node%lhs) // " " // &
                       trim(node%rhs) // char(10)
    end subroutine emit_binary_op

    subroutine emit_if(this, node)
        class(ir_emitter_t), intent(inout) :: this
        type(if_node), intent(in) :: node
        character(len=16) :: label_then, label_end

        this%label_counter = this%label_counter + 1
        write (label_then, '(a,i0)') "L", this%label_counter
        this%label_counter = this%label_counter + 1
        write (label_end, '(a,i0)') "L", this%label_counter

        this%ir_code = this%ir_code // "BRANCH_IF " // trim(node%condition) &
                       // " " // trim(label_then) // char(10)
        this%ir_code = this%ir_code // "JUMP " // trim(label_end) // char(10)
        this%ir_code = this%ir_code // trim(label_then) // ":" // char(10)
        this%ir_code = this%ir_code // trim(label_end) // ":" // char(10)
    end subroutine emit_if

end module custom_ir_emitter

program compiler_example
    use fortfront_tooling
    use custom_ir_emitter
    implicit none

    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg, ir_code

    call tooling_load_ast_from_string("x = a + b", arena, root_index, &
                                      error_msg)

    if (len_trim(error_msg) > 0) then
        print '(a)', "Error: " // trim(error_msg)
        stop 1
    end if

    ir_code = emit_ir(arena, root_index)
    print '(a)', "Generated IR:"
    print '(a)', ir_code
end program compiler_example
```

## Example 4: AST Analysis Tool

This example counts different node types in an AST.

```fortran
program ast_statistics
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront_tooling
    use fortfront_ast
    implicit none

    character(len=256) :: input_file
    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg
    integer :: total_nodes, assignment_count, loop_count, if_count

    if (command_argument_count() /= 1) then
        print '(a)', "Usage: ast_statistics <input.f90>"
        stop 1
    end if

    call get_command_argument(1, input_file)

    call tooling_load_ast_from_file(trim(input_file), arena, root_index, &
                                    error_msg)

    if (len_trim(error_msg) > 0) then
        print '(a)', "Error: " // trim(error_msg)
        stop 1
    end if

    total_nodes = count_nodes(arena, root_index)
    assignment_count = count_node_type(arena, root_index, "assignment")
    loop_count = count_node_type(arena, root_index, "do_loop")
    if_count = count_node_type(arena, root_index, "if")

    print '(a)', "AST Statistics:"
    print '(a,i0)', "  Total nodes: ", total_nodes
    print '(a,i0)', "  Assignments: ", assignment_count
    print '(a,i0)', "  Loops: ", loop_count
    print '(a,i0)', "  If statements: ", if_count

contains

    function count_node_type(arena, root_index, node_type) result(count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=*), intent(in) :: node_type
        integer :: count
        integer, allocatable :: indices(:)

        indices = find_nodes_by_type(arena, root_index, node_type)
        count = size(indices)
    end function count_node_type

end program ast_statistics
```

## Structured Diagnostics API

FortFront provides a structured diagnostic system for consistent error reporting across all frontend phases. This mirrors the approach used in GCC's diagnostic framework.

### Basic Usage

```fortran
use frontend_diagnostics, only: make_diagnostic, format_diagnostic, &
    DIAG_BINARY_DATA, DIAGNOSTIC_ERROR
use fortfront_types, only: diagnostic_t, source_range_t

type(diagnostic_t) :: diag
character(len=:), allocatable :: formatted

diag = make_diagnostic(DIAG_BINARY_DATA, DIAGNOSTIC_ERROR, &
    "Input appears to be binary data")
formatted = format_diagnostic(diag)

print '(a)', formatted
! Output: [F002] ERROR at line 1:1: Input appears to be binary data
```

### Diagnostic with Source Location

```fortran
use frontend_diagnostics, only: make_diagnostic, format_diagnostic, &
    DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR
use fortfront_types, only: diagnostic_t, source_range_t, source_location_t

type(diagnostic_t) :: diag
type(source_range_t) :: location
character(len=:), allocatable :: formatted

location%start%line = 42
location%start%column = 15
location%end%line = 42
location%end%column = 20

diag = make_diagnostic(DIAG_SYNTAX_ERROR, DIAGNOSTIC_ERROR, &
    "Unexpected token", location)
formatted = format_diagnostic(diag)

print '(a)', formatted
! Output: [F004] ERROR at line 42:15: Unexpected token
```

### Available Diagnostic Codes

Diagnostic codes follow GCC-style conventions:

- **F001**: DIAG_EMPTY_INPUT - Empty or whitespace-only input
- **F002**: DIAG_BINARY_DATA - Input appears to be binary data
- **F003**: DIAG_LEXICAL_ERROR - Lexical analysis error
- **F004**: DIAG_SYNTAX_ERROR - Syntax error during parsing
- **F005**: DIAG_SEMANTIC_ERROR - Semantic analysis error
- **F006**: DIAG_PARSE_ERROR - Parse error
- **F007**: DIAG_NO_PROGRAM_UNIT - No valid program unit created

### Severity Levels

- **DIAGNOSTIC_ERROR**: Fatal error, transformation cannot continue
- **DIAGNOSTIC_WARNING**: Non-fatal issue, transformation can continue
- **DIAGNOSTIC_INFO**: Informational message
- **DIAGNOSTIC_HINT**: Suggestion for improvement

### Custom Diagnostics in Tools

Tools built on FortFront can define their own diagnostic codes following the same pattern:

```fortran
module my_tool_diagnostics
    use frontend_diagnostics, only: make_diagnostic, format_diagnostic, &
        DIAGNOSTIC_WARNING
    use fortfront_types, only: diagnostic_t
    implicit none
    private

    public :: DIAG_UNUSED_VAR, emit_unused_var_warning

    character(len=*), parameter :: DIAG_UNUSED_VAR = "T001"

contains

    subroutine emit_unused_var_warning(var_name, location)
        character(len=*), intent(in) :: var_name
        type(source_range_t), intent(in) :: location
        type(diagnostic_t) :: diag
        character(len=:), allocatable :: msg

        msg = "Variable '" // trim(var_name) // "' declared but never used"
        diag = make_diagnostic(DIAG_UNUSED_VAR, DIAGNOSTIC_WARNING, msg, location)
        print '(a)', format_diagnostic(diag)
    end subroutine emit_unused_var_warning

end module my_tool_diagnostics
```

## Error Handling Best Practices

### Pattern 1: Check Allocatable Error Strings

```fortran
character(len=:), allocatable :: error_msg

call some_api_function(..., error_msg)

if (allocated(error_msg)) then
    if (len_trim(error_msg) > 0) then
        print '(a)', "Error: " // trim(error_msg)
        return
    end if
end if
```

### Pattern 2: Use Result Types

```fortran
use fortfront_semantic, only: result_t

type(result_t) :: result

result = some_operation(...)

if (.not. result%success) then
    print '(a)', "Operation failed: " // trim(result%error_message)
    return
end if
```

### Pattern 3: Collect Errors for Batch Operations

```fortran
use fortfront_error, only: error_collection_t, error_record_t

type(error_collection_t) :: errors
integer :: i

call process_files(file_list, errors)

if (errors%count > 0) then
    do i = 1, errors%count
        print '(a,i0,a,i0,a,a)', &
            "Error at line ", errors%records(i)%line, &
            " column ", errors%records(i)%column, &
            ": ", trim(errors%records(i)%message)
    end do
end if
```

## Performance Optimization

### Tip 1: Reuse Arenas for Batch Processing

```fortran
type(ast_arena_t) :: arena
type(tooling_parse_options_t) :: options
integer :: root_index
character(len=:), allocatable :: error_msg
character(len=256) :: file_name
integer :: i

options%reuse_arena = .true.

do i = 1, num_files
    write (file_name, '(a,i0,a)') "input", i, ".f90"

    call tooling_load_ast_from_file(file_name, arena, root_index, &
                                    error_msg, options)

    if (len_trim(error_msg) == 0) then
        call process_ast(arena, root_index)
    end if
end do
```

### Tip 2: Use Safe Tokenization in Hot Paths

```fortran
use fortfront_lexer, only: tokenize_safe

type(token_t), allocatable :: tokens(:)

tokens = tokenize_safe(source)

if (allocated(tokens)) then
    call process_tokens(tokens)
end if
```

### Tip 3: Cache AST Traversal Results

```fortran
type :: cached_analysis_t
    integer :: total_nodes
    integer :: max_depth
    logical :: has_loops
end type cached_analysis_t

type(cached_analysis_t), allocatable :: cache(:)

if (.not. allocated(cache)) then
    allocate (cache(num_files))
    do i = 1, num_files
        cache(i) = analyze_file(files(i))
    end do
end if
```

## FAQ

### Q: How do I link FortFront as a library?

Build FortFront with fpm and add it as a dependency in your fpm.toml:

```toml
[dependencies]
fortfront = { path = "../fortfront" }
```

Alternatively, build a static library:

```sh
cd fortfront
fpm build --flag "-fPIC"
ar rcs libfortfront.a build/gfortran_*/fortfront/lib/*.o
```

Then link manually:

```sh
gfortran -o my_tool my_tool.f90 -L./fortfront -lfortfront -I./fortfront/build/gfortran_*/fortfront/include
```

### Q: Can I use FortFront from C or Python?

Yes. FortFront already ships with a production-ready ISO_C_BINDING bridge in `src/interfaces/fortfront_c_interface.f90`. That module converts C buffers into Fortran strings, calls `transform_lazy_fortran_string(input, output, error_msg)`, stores any error text via `set_last_error`, and exposes helpers such as `fortfront_parse_source_c`, `fortfront_get_last_error_c`, and `fortfront_get_version_c`. Reuse those bindings directly or adapt them for your runtime.

### Q: How do I handle large files efficiently?

For large files, consider streaming with program unit boundaries:

```fortran
use fortfront_parser, only: find_program_unit_boundary, parse_program_unit

integer :: start_idx, end_idx

start_idx = 1
do while (start_idx < size(tokens))
    end_idx = find_program_unit_boundary(tokens, start_idx)
    call parse_program_unit(tokens(start_idx:end_idx), arena, root_index)
    call process_unit(arena, root_index)
    start_idx = end_idx + 1
end do
```

### Q: Are the APIs thread-safe?

APIs are reentrant when using separate contexts. Arena allocators are NOT thread-safe. Use one arena per thread:

```fortran
!$omp parallel private(arena, root_index, error_msg)
    !$omp do
    do i = 1, num_files
        call tooling_load_ast_from_file(files(i), arena, root_index, error_msg)
        call process_ast(arena, root_index)
    end do
    !$omp end do
!$omp end parallel
```

### Q: How do I debug AST issues?

Use the to_json_interface to export AST as JSON:

```fortran
use fortfront_ast

class(ast_node), pointer :: node
character(len=:), allocatable :: json

node => arena%entries(root_index)%node
json = node%to_json()
print '(a)', json
```

### Q: Can I extend the AST with custom nodes?

Yes, extend ast_node and implement required interfaces:

```fortran
type, extends(ast_node) :: custom_node
    integer :: custom_data
contains
    procedure :: accept => custom_accept
    procedure :: to_json => custom_to_json
end type custom_node
```

Register with the arena and visitor.

### Q: How do I report bugs or request features?

File issues at: https://github.com/lazy-fortran/fortfront/issues

## See Also

- API.md - Complete API reference
- README.md - CLI usage
- examples/ - Additional code samples
