module parser_if_body_module
    ! Parser module for IF construct body statements
    !
    ! This module handles parsing statements within IF/THEN/ELSE/ELSEIF blocks.
    ! It detects block terminators (ELSEIF, ELSE, ENDIF, END IF) and parses
    ! individual statements within the body.
    !
    ! ISO/IEC 1539-1:2018 Section 11.1.8.1 specifies IF construct syntax.
    use lexer_core, only: token_t, TK_KEYWORD, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_statement_core_module, only: statement_callbacks_t, &
                                            find_statement_end, &
                                            extend_if_statement_end, &
                                            allocate_stmt_tokens_with_eof, &
                                            skip_whitespace_and_semicolons, &
                                            parse_basic_statement_core
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: parse_if_body
    public :: is_if_body_terminator
    public :: parse_if_body_statement

contains

    ! Parse if/elseif/else body statements
    function parse_if_body(parser, arena, parent_index, callbacks) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_count

        allocate (body_indices(0))
        stmt_count = 0

        block
            integer :: safety_counter

            safety_counter = 0
            do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                safety_counter = safety_counter + 1

                call skip_whitespace_and_semicolons(parser)
                if (parser%current_token > size(parser%tokens)) exit

                token = parser%peek()
                if (is_if_body_terminator(parser, token)) exit

                call parse_if_body_statement(parser, arena, parent_index, &
                                             body_indices, stmt_count, callbacks)
            end do
        end block

    end function parse_if_body

    logical function is_if_body_terminator(parser, token)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: keyword_text
        type(token_t) :: lookahead
        integer :: lookahead_pos

        is_if_body_terminator = .false.

        if (token%kind /= TK_KEYWORD) return

        keyword_text = to_lower(trim(token%text))
        if (keyword_text == "elseif" .or. keyword_text == "else if" .or. &
            keyword_text == "endif" .or. keyword_text == "end if") then
            is_if_body_terminator = .true.
        else if (keyword_text == "else") then
            ! Standalone else is always a terminator (else or else if)
            is_if_body_terminator = .true.
        else if (keyword_text == "end") then
            lookahead_pos = parser%current_token + 1
            do while (lookahead_pos <= size(parser%tokens))
                lookahead = parser%tokens(lookahead_pos)
                if (lookahead%kind == TK_WHITESPACE .or. &
                    lookahead%kind == TK_NEWLINE .or. &
                    lookahead%kind == TK_COMMENT) then
                    lookahead_pos = lookahead_pos + 1
                    cycle
                end if
                exit
            end do
            if (lookahead_pos <= size(parser%tokens)) then
                lookahead = parser%tokens(lookahead_pos)
                if (lookahead%kind == TK_KEYWORD .and. &
                    to_lower(trim(lookahead%text)) == "if") then
                    is_if_body_terminator = .true.
                end if
            end if
        end if
    end function is_if_body_terminator

    subroutine parse_if_body_statement(parser, arena, parent_index, body_indices, &
                                       stmt_count, callbacks)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(inout) :: stmt_count
        type(statement_callbacks_t), intent(in) :: callbacks

        type(token_t), allocatable, target :: stmt_tokens(:)
        type(token_t) :: first_stmt_token
        integer, allocatable :: stmt_indices(:)
        integer :: consumed_tokens, k
        integer :: stmt_end, last_token_index

        stmt_end = find_statement_end(parser%tokens, parser%current_token)
        first_stmt_token = parser%tokens(parser%current_token)
        if (first_stmt_token%kind == TK_KEYWORD) then
            if (first_stmt_token%text == "if") then
                stmt_end = extend_if_statement_end(parser%tokens, &
                                                   parser%current_token, &
                                                   stmt_end)
            end if
        end if
        if (stmt_end < parser%current_token) then
            stmt_end = parser%current_token
        end if

        if (stmt_end - parser%current_token + 1 <= 0) return

        call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
                                           parser%current_token, stmt_end)
        last_token_index = stmt_end
        stmt_tokens(stmt_end - parser%current_token + 2)%line = &
            parser%tokens(last_token_index)%line
        stmt_tokens(stmt_end - parser%current_token + 2)%column = &
            parser%tokens(last_token_index)%column + 1

        if (present(parent_index)) then
            stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                      parent_index=parent_index, &
                                                      callbacks=callbacks, &
                                                      consumed_count=consumed_tokens)
        else
            stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                      callbacks=callbacks, &
                                                      consumed_count=consumed_tokens)
        end if

        if (allocated(stmt_indices) .and. size(stmt_indices) > 0) then
            do k = 1, size(stmt_indices)
                if (stmt_indices(k) > 0) then
                    body_indices = [body_indices, stmt_indices(k)]
                    stmt_count = stmt_count + 1
                end if
            end do
        end if

        parser%current_token = stmt_end + 1

        block
            type(token_t), allocatable, target :: temp(:)
            call move_alloc(stmt_tokens, temp)
        end block
    end subroutine parse_if_body_statement

end module parser_if_body_module
