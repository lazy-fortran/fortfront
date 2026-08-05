module parser_do_constructs_module
    ! Parser module for DO constructs (do loops, do while)
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
        TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_logical_or
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_factory, only: push_do_loop, push_do_while
    use parser_if_constructs_module, only: parse_if, register_parse_do_loop
    use parser_statement_callbacks_module, only: register_fallback_do_parser, &
        register_fallback_if_parser, register_fallback_block_parser, &
        register_fallback_select_parsers
    use parser_select_constructs_module, only: parse_select_case, parse_select_type
    use parser_array_constructs_module, only: parse_where_construct, parse_associate, &
        parse_block_construct
    use parser_forall_module, only: parse_forall
    use parser_do_concurrent_locality_module, only: &
        parse_do_concurrent_locality_specs
    use parser_statement_core_module, only: parse_basic_statement_core, &
        statement_callbacks_t, &
        null_statement_callbacks, &
        find_statement_end, extend_block_statement_end
    use parser_trailing_comment_module, only: capture_trailing_comment
    implicit none
    private

    type :: do_loop_control_t
        character(len=:), allocatable :: var_name
        character(len=:), allocatable :: type_spec
        integer :: start_index = 0
        integer :: end_index = 0
        integer :: step_index = 0
    end type do_loop_control_t

    logical, save :: if_hooks_initialized = .false.

    public :: parse_do_loop, parse_do_while, parse_do_while_from_do
    public :: ensure_if_do_registration

contains

    include 'parser_do_constructs_part1.inc'
    include 'parser_do_constructs_part2.inc'

end module parser_do_constructs_module
