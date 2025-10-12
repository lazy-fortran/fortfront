module parser_call_module
    ! Shared call-statement parser used across control-flow helpers
    use lexer_core, only: token_t
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_range
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_subroutine_call, push_literal
    use ast_types, only: LITERAL_STRING
    implicit none
    private

    public :: parse_call_statement

contains

    subroutine parse_call_arguments(parser, arena, arg_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: arg_index

        allocate (arg_indices(0))
        token = parser%consume()  ! consume '('

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            arg_index = parse_range(parser, arena)
            if (arg_index > 0) then
                arg_indices = [arg_indices, arg_index]
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                exit
            end if
        end do
    end subroutine parse_call_arguments

    function parse_call_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer, allocatable :: arg_indices(:)
        integer :: line, column

        stmt_index = 0

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            stmt_index = push_literal(arena, "! Error: expected subroutine name after 'call'", &
                                      LITERAL_STRING, line, column)
            return
        end if

        subroutine_name = token%text
        token = parser%consume()

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            call parse_call_arguments(parser, arena, arg_indices)
        else
            allocate (arg_indices(0))
        end if

        stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, line, column)
    end function parse_call_statement

end module parser_call_module
