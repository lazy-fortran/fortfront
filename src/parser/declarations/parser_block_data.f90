module parser_block_data_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, TK_NUMBER, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_block_data, push_literal
    use parser_declarations, only: parse_declaration
    use parser_common_statement_module, only: parse_common_statement
    use parser_statement_data_module, only: parse_data_statement
    implicit none
    private

    public :: parse_block_data

contains

    function parse_block_data(parser, arena) result(block_data_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: block_data_index
        type(token_t) :: token
        type(token_t) :: next_token
        character(len=:), allocatable :: block_name
        character(len=:), allocatable :: header_label, end_label
        character(len=:), allocatable :: pending_end_label
        integer :: line, column, stmt_start_pos
        integer :: lookahead
        integer, allocatable :: statement_indices(:)
        integer :: stmt_index

        block_name = ""
        header_label = ""
        end_label = ""
        pending_end_label = ""
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

        token = parser%peek()
        if (token%kind == TK_NUMBER) then
            header_label = trim(token%text)
            token = parser%consume()
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

            ! Detect optional statement label preceding END BLOCK DATA
            if (token%kind == TK_NUMBER) then
                pending_end_label = ""
                lookahead = parser%current_token + 1
                do while (lookahead <= size(parser%tokens))
                    next_token = parser%tokens(lookahead)
                    select case (next_token%kind)
                    case (TK_WHITESPACE, TK_COMMENT)
                        lookahead = lookahead + 1
                        cycle
                    case (TK_NEWLINE)
                        pending_end_label = ""
                        exit
                    case default
                        exit
                    end select
                end do

                if (lookahead <= size(parser%tokens)) then
                    next_token = parser%tokens(lookahead)
                    if (next_token%kind == TK_KEYWORD) then
                        if (to_lower(trim(next_token%text)) == "end") then
                            pending_end_label = trim(token%text)
                            token = parser%consume()
                            do while (.not. parser%is_at_end())
                                token = parser%peek()
                                select case (token%kind)
                                case (TK_WHITESPACE, TK_COMMENT)
                                    token = parser%consume()
                                case default
                                    exit
                                end select
                            end do
                            if (parser%is_at_end()) exit
                            token = parser%peek()
                        else
                            pending_end_label = ""
                        end if
                    else
                        pending_end_label = ""
                    end if
                else
                    pending_end_label = ""
                end if
            else
                pending_end_label = ""
            end if

            if (token%kind == TK_KEYWORD .and. &
                to_lower(trim(token%text)) == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        to_lower(trim(parser%tokens(parser%current_token + &
                        1)%text)) == "block") then
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
                        if (len_trim(pending_end_label) > 0) then
                            end_label = pending_end_label
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
            stmt_index = parse_body_statement(parser, arena)
            if (stmt_index > 0) then
                statement_indices = [statement_indices, stmt_index]
            end if
            ! Guard against a parser that did not advance: drain the line.
            if (parser%current_token <= stmt_start_pos) then
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
                    token = parser%consume()
                end do
            end if
        end do

        block_data_index = push_block_data(arena, block_name, statement_indices, &
            line, column, header_label=header_label, &
            end_label=end_label)
    end function parse_block_data

    ! Parse a single BLOCK DATA body statement into a structured node, routing on
    ! the leading keyword. Type declarations, COMMON, and DATA become proper AST
    ! nodes; anything else falls back to a text literal preserving the line.
    integer function parse_body_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: keyword

        stmt_index = 0
        token = parser%peek()
        keyword = to_lower(trim(token%text))

        select case (keyword)
        case ("integer", "real", "logical", "character", "complex", &
                "double", "type", "class", "dimension")
            stmt_index = parse_declaration(parser, arena)
        case ("common")
            stmt_index = parse_common_statement(parser, arena)
        case ("data")
            stmt_index = parse_data_statement(parser, arena)
        case default
            stmt_index = parse_body_text_literal(parser, arena)
        end select
    end function parse_body_statement

    integer function parse_body_text_literal(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: statement_text
        integer :: stmt_start_pos

        stmt_index = 0
        stmt_start_pos = parser%current_token
        statement_text = ""
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
            if (len(statement_text) > 0) statement_text = statement_text // " "
            statement_text = statement_text // token%text
            token = parser%consume()
        end do

        if (len(statement_text) > 0) then
            stmt_index = push_literal(arena, trim(statement_text), &
                LITERAL_STRING, &
                line=parser%tokens(stmt_start_pos)%line, &
                column=parser%tokens(stmt_start_pos)%column)
        end if
    end function parse_body_text_literal

end module parser_block_data_module
