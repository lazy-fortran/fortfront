module parser_api
    ! Public parser API for library consumers
    ! Provides parsing functionality to convert tokens into AST
    use frontend_parsing, only: &
        parse_tokens, &
        parse_tokens_safe, &
        parse_result_with_index_t, &
        find_program_unit_boundary, &
        is_function_start, &
        is_end_function, &
        parse_program_unit, &
        is_do_loop_start, &
        is_do_while_start, &
        is_select_case_start, &
        is_end_do, &
        is_end_select, &
        is_if_then_start, &
        is_end_if
    use ast_arena_modern, only: ast_arena_t
    use compiler_arena, only: compiler_arena_t, create_compiler_arena, &
        destroy_compiler_arena

    implicit none
    private

    ! Core types
    public :: parse_result_with_index_t
    public :: ast_arena_t
    public :: compiler_arena_t

    ! Main parsing functions
    public :: parse_tokens
    public :: parse_tokens_safe

    ! Arena management
    public :: create_compiler_arena
    public :: destroy_compiler_arena

    ! Parsing utilities for program structure analysis
    public :: find_program_unit_boundary
    public :: is_function_start
    public :: is_end_function
    public :: parse_program_unit

    ! Control flow detection helpers
    public :: is_do_loop_start
    public :: is_do_while_start
    public :: is_select_case_start
    public :: is_end_do
    public :: is_end_select
    public :: is_if_then_start
    public :: is_end_if

end module parser_api
