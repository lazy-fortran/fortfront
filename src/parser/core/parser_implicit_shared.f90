module parser_implicit_shared_module
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, &
        TK_WHITESPACE, TK_COMMENT, TK_NEWLINE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    implicit none
    private

    public :: parse_simple_implicit_statement, parse_none_spec_list

contains

    subroutine parse_simple_implicit_statement(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: implicit_token, none_token, lparen_token
        character(len=:), allocatable :: implicit_type, none_spec

        stmt_index = 0

        implicit_token = parser%consume()

        none_token = parser%peek()
        if (none_token%kind == TK_KEYWORD .and. none_token%text == "none") then
            none_token = parser%consume()
            implicit_type = "none"

            lparen_token = parser%peek()
            if (lparen_token%kind == TK_OPERATOR .and. lparen_token%text == "(") then
                lparen_token = parser%consume()
                call parse_none_spec_list(parser, none_spec)
            end if
        else
            implicit_type = "default"
        end if

        if (implicit_type == "none") then
            if (allocated(none_spec)) then
                stmt_index = push_implicit_statement(arena, .true., &
                    line=implicit_token%line, &
                    column=implicit_token%column, &
                    none_spec=none_spec)
            else
                stmt_index = push_implicit_statement(arena, .true., &
                    line=implicit_token%line, &
                    column=implicit_token%column)
            end if
        else
            stmt_index = push_implicit_statement(arena, .false., &
                line=implicit_token%line, &
                column=implicit_token%column)
        end if
    end subroutine parse_simple_implicit_statement

    subroutine parse_none_spec_list(parser, none_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: none_spec
        type(token_t) :: token
        logical :: done

        done = .false.
        do while (.not. done)
            token = parser%peek()
            select case (token%kind)
            case (TK_OPERATOR)
                if (token%text == ")") then
                    token = parser%consume()
                    exit
                else if (token%text == ",") then
                    token = parser%consume()
                else
                    token = parser%consume()
                end if
            case (TK_KEYWORD, TK_IDENTIFIER)
                if (.not. allocated(none_spec)) then
                    none_spec = trim(token%text)
                else
                    none_spec = trim(none_spec) // ", " // trim(token%text)
                end if
                token = parser%consume()
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine parse_none_spec_list

end module parser_implicit_shared_module
