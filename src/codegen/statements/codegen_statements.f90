module codegen_statements
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_io
    use ast_nodes_misc
    use ast_nodes_procedure
    use ast_nodes_control
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_arena_interface, only: generate_code_from_arena
    use lexer_core, only: to_lower
    implicit none
    private

    public :: generate_code_assignment
    public :: generate_code_pointer_assignment
    public :: generate_code_statement_function
    public :: generate_code_subroutine_call
    public :: generate_code_print_statement
    public :: generate_code_write_statement
    public :: generate_code_read_statement
    public :: generate_code_format_statement
    public :: generate_code_termination
    public :: generate_code_return
    public :: generate_code_entry
    public :: generate_code_continue
    public :: generate_code_goto
    public :: generate_code_error_termination
    public :: generate_code_cycle
    public :: generate_code_exit
    public :: generate_code_use_statement
    public :: generate_code_intrinsic_statement
    public :: generate_code_import_statement
    public :: generate_code_include_statement
    public :: generate_code_visibility_statement
    public :: generate_code_namelist_statement
    public :: generate_code_data_statement
    public :: generate_code_implicit_statement
    public :: generate_code_comment
    public :: generate_code_directive
    public :: generate_code_blank_line
    public :: generate_code_allocate_statement
    public :: generate_code_deallocate_statement
    public :: generate_code_open_statement
    public :: generate_code_close_statement
    public :: generate_code_inquire_statement
    public :: generate_code_backspace_statement
    public :: generate_code_rewind_statement
    public :: generate_code_endfile_statement
    public :: generate_code_pause_statement
    public :: generate_code_nullify_statement

contains

    include 'codegen_statements_part1.inc'
    include 'codegen_statements_part2.inc'

end module codegen_statements
