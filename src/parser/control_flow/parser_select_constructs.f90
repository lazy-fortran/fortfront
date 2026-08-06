module parser_select_constructs_module
    ! Parser module for SELECT CASE and SELECT TYPE constructs
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression_until
    use parser_io_statements_module, only: &
        parse_print_statement, parse_write_statement, parse_read_statement
    use parser_control_statements_module, only: &
        parse_cycle_statement, parse_exit_statement, parse_return_statement, &
        parse_stop_statement, parse_goto_statement, parse_error_stop_statement
    use parser_basic_statement_module, only: parse_statement_body
    use parser_statement_core_module, only: statement_callbacks_t, &
        null_statement_callbacks
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_select_case, push_select_case_with_default, &
        push_case_block, push_case_range, push_case_default, &
        push_select_type, push_select_type_with_default, &
        push_type_guard_block, &
        push_select_rank, push_select_rank_with_default, &
        push_rank_block, &
        push_identifier, push_literal, push_assignment, &
        push_pointer_assignment
    implicit none
    private

    public :: parse_select_case, parse_select_type, parse_select_rank

    interface
        subroutine ensure_if_do_registration_bridge()
        end subroutine ensure_if_do_registration_bridge

        recursive function parse_block_construct_bridge(parser, arena) &
                result(block_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer :: block_index
        end function parse_block_construct_bridge
    end interface

contains

    include 'parser_select_constructs_helpers.inc'
    include 'parser_select_constructs_case.inc'
    include 'parser_select_constructs_type.inc'
    include 'parser_select_constructs_rank.inc'

    integer function parse_select_case_callback(parser, arena) result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        node_index = parse_select_case(parser, arena)
    end function parse_select_case_callback

    integer function parse_select_type_callback(parser, arena) result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        node_index = parse_select_type(parser, arena)
    end function parse_select_type_callback

    integer function parse_select_rank_callback(parser, arena) result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        node_index = parse_select_rank(parser, arena)
    end function parse_select_rank_callback

end module parser_select_constructs_module
