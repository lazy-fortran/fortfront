module parser_block_data_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_block_data, push_literal
    implicit none
    private

    public :: parse_block_data

contains

    function parse_block_data(parser, arena) result(block_data_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: block_data_index
        type(token_t) :: token
        character(len=:), allocatable :: block_name, statement_text
        integer :: line, column, stmt_start_pos
        integer, allocatable :: statement_indices(:)
        integer :: stmt_index

        block_name = ""
        allocate (statement_indices(0))

        ! Skip leading trivia before the BLOCK DATA header
        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                token = parser%consume()
            case default
                exit
            end select
        end do

        if (parser%is_at_end()) then
            block_data_index = 0
            return
        end if

        token = parser%consume()
        if (.not. (token%kind == TK_KEYWORD .and. &
                   to_lower(trim(token%text)) == "block")) then
            block_data_index = 0
            return
        end if

        line = token%line
        column = token%column

        ! Skip whitespace between BLOCK and DATA
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_WHITESPACE) then
                token = parser%consume()
            else
                exit
            end if
        end do

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                if (to_lower(trim(token%text)) == "data") then
                    token = parser%consume()
                end if
            end if
        end if

        ! Consume optional whitespace/comments before the optional block name
        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                token = parser%consume()
            case default
                exit
            end select
        end do

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                block_name = token%text
                token = parser%consume()
            end if
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()

            if (token%kind == TK_KEYWORD .and. &
                to_lower(trim(token%text)) == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. to_lower(trim(parser%tokens(parser%current_token + 1)%text)) == "block") then
                        token = parser%consume()
                        token = parser%consume()
                        if (.not. parser%is_at_end()) then
                            token = parser%peek()
                            if (token%kind == TK_KEYWORD .and. &
                                to_lower(trim(token%text)) == "data") then
                                token = parser%consume()
                            end if
                        end if
                        if (.not. parser%is_at_end()) then
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER) then
                                token = parser%consume()
                            end if
                        end if
                        exit
                    end if
                end if
            end if

            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) then
                token = parser%consume()
                cycle
            end if

            stmt_start_pos = parser%current_token
            statement_text = ""
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) then
                    exit
                end if
                if (len(statement_text) > 0) statement_text = statement_text // " "
                statement_text = statement_text // token%text
                token = parser%consume()
            end do

            if (len(statement_text) > 0) then
                stmt_index = push_literal(arena, trim(statement_text), &
                                         LITERAL_STRING, &
                                         line=parser%tokens(stmt_start_pos)%line, &
                                         column=parser%tokens(stmt_start_pos)%column)
                if (stmt_index > 0) then
                    statement_indices = [statement_indices, stmt_index]
                end if
            end if
        end do

        block_data_index = push_block_data(arena, block_name, statement_indices, &
                                           line, column)
    end function parse_block_data

end module parser_block_data_module
