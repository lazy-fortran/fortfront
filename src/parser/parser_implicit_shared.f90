module parser_implicit_shared_module
    use lexer_core, only: token_t, TK_KEYWORD, TK_LPAREN, TK_RPAREN, TK_IDENTIFIER
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    implicit none
    private

    public :: parse_simple_implicit_statement

contains

    subroutine parse_simple_implicit_statement(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: implicit_token, none_token, lparen_token, spec_token, &
            rparen_token
        character(len=:), allocatable :: implicit_type, none_spec

        stmt_index = 0

        implicit_token = parser%consume()

        none_token = parser%peek()
        if (none_token%kind == TK_KEYWORD .and. none_token%text == "none") then
            none_token = parser%consume()
            implicit_type = "none"

            lparen_token = parser%peek()
            if (lparen_token%kind == TK_LPAREN) then
                lparen_token = parser%consume()
                spec_token = parser%peek()
                if (spec_token%kind == TK_KEYWORD .or. &
                    spec_token%kind == TK_IDENTIFIER) then
                    spec_token = parser%consume()
                    none_spec = trim(spec_token%text)
                end if
                rparen_token = parser%peek()
                if (rparen_token%kind == TK_RPAREN) then
                    rparen_token = parser%consume()
                end if
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

end module parser_implicit_shared_module
