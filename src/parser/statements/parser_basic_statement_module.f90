module parser_basic_statement_module
    ! Parser module for basic statement parsing and utilities
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_OPERATOR, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_range
    use parser_statement_core_module, only: parse_basic_statement_core, &
        statement_callbacks_t, &
        null_statement_callbacks, &
        find_statement_end
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: parse_basic_statement_multi, parse_statement_body
    public :: parse_expression_length

contains

    ! Parse basic statement with support for multi-variable declarations
    function parse_basic_statement_multi(tokens, arena, parent_index, callbacks, &
            consumed_count, parent_parser) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, intent(out), optional :: consumed_count
        type(parser_state_t), intent(in), optional :: parent_parser
        integer, allocatable :: stmt_indices(:)
        type(statement_callbacks_t) :: local_callbacks

        if (present(callbacks)) then
            local_callbacks = callbacks
        else
            local_callbacks = null_statement_callbacks()
        end if

        if (present(parent_index)) then
            if (present(consumed_count)) then
                stmt_indices = parse_basic_statement_core( &
                    tokens, arena, parent_index=parent_index, &
                    callbacks=local_callbacks, &
                    consumed_count=consumed_count, parent_parser=parent_parser)
            else
                stmt_indices = parse_basic_statement_core( &
                    tokens, arena, parent_index=parent_index, &
                    callbacks=local_callbacks, parent_parser=parent_parser)
            end if
        else
            if (present(consumed_count)) then
                stmt_indices = parse_basic_statement_core( &
                    tokens, arena, callbacks=local_callbacks, &
                    consumed_count=consumed_count, parent_parser=parent_parser)
            else
                stmt_indices = parse_basic_statement_core(tokens, arena, &
                    callbacks=local_callbacks, parent_parser=parent_parser)
            end if
        end if
    end function parse_basic_statement_multi

    ! Unified function for parsing statement bodies (used by if blocks, &
    ! do while loops, etc.)
    function parse_statement_body(parser, arena, end_keywords, callbacks, &
            parent_index) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: end_keywords(:)
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, intent(in), optional :: parent_index
        integer, allocatable :: body_indices(:)

        type(token_t), allocatable, target :: stmt_tokens(:)
        type(token_t) :: token
        integer :: stmt_start, stmt_end
        type(statement_callbacks_t) :: local_callbacks
        logical :: has_meaningful
        integer :: nested_block_index
        integer :: stmt_count

        allocate (body_indices(0))
        stmt_count = 0

        if (present(callbacks)) then
            local_callbacks = callbacks
        else
            local_callbacks = null_statement_callbacks()
        end if

        block
            integer :: safety_counter
            safety_counter = 0
            do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                safety_counter = safety_counter + 1
                call skip_nonstatements(parser)
                if (parser%is_at_end()) exit

                token = parser%peek()
                if (check_end_keyword_match(token, parser, end_keywords)) exit

                nested_block_index = parse_nested_block(parser, arena, &
                    local_callbacks)
                if (nested_block_index > 0) then
                    body_indices = [body_indices, nested_block_index]
                    stmt_count = stmt_count + 1
                    cycle
                end if

                stmt_start = parser%current_token
                stmt_end = find_statement_end(parser%tokens, stmt_start)
                if (stmt_end < stmt_start) stmt_end = stmt_start

                has_meaningful = has_meaningful_tokens(parser%tokens, stmt_start, &
                    stmt_end)
                if (.not. has_meaningful) then
                    call advance_past_empty_statement(parser, stmt_start, stmt_end)
                    cycle
                end if

                call extract_statement_tokens(parser%tokens, stmt_start, stmt_end, &
                    stmt_tokens)
                call parse_and_add_statement(stmt_tokens, arena, parent_index, &
                    local_callbacks, body_indices, &
                    stmt_count, parser)
                call release_statement_tokens(stmt_tokens)

                parser%current_token = next_statement_start(parser%tokens, stmt_end)
            end do
        end block
    end function parse_statement_body

    integer function parse_nested_block(parser, arena, callbacks) &
            result(block_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        type(token_t) :: token

        block_index = 0
        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return
        if (token%text /= 'block') return
        if (.not. associated(callbacks%parse_block)) return
        block_index = callbacks%parse_block(parser, arena)
    end function parse_nested_block

    subroutine skip_nonstatements(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) return
        token = parser%peek()

        do while (.not. parser%is_at_end())
            select case (token%kind)
            case (TK_NEWLINE, TK_COMMENT, TK_WHITESPACE)
                token = parser%consume()
            case (TK_OPERATOR)
                if (token%text == ";") then
                    token = parser%consume()
                else
                    exit
                end if
            case default
                exit
            end select
            if (parser%is_at_end()) exit
            token = parser%peek()
        end do
    end subroutine skip_nonstatements

    logical function check_end_keyword_match(token, parser, end_keywords) &
            result(found_end)
        type(token_t), intent(in) :: token
        type(parser_state_t), intent(in) :: parser
        character(len=*), intent(in) :: end_keywords(:)

        integer :: j
        integer :: lookahead
        character(len=:), allocatable :: trimmed_keyword
        character(len=:), allocatable :: suffix_keyword

        found_end = .false.
        if (token%kind /= TK_KEYWORD) return

        do j = 1, size(end_keywords)
            trimmed_keyword = trim(end_keywords(j))
            if (len_trim(trimmed_keyword) == 0) cycle

            if (index(trimmed_keyword, "end ") == 1) then
                if (token%text == "end") then
                    suffix_keyword = &
                        adjustl(trimmed_keyword(4:len_trim(trimmed_keyword)))
                    if (len_trim(suffix_keyword) == 0) cycle

                    lookahead = parser%current_token + 1
                    do while (lookahead <= size(parser%tokens))
                        select case (parser%tokens(lookahead)%kind)
                        case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                            lookahead = lookahead + 1
                            cycle
                        end select
                        exit
                    end do

                    if (lookahead <= size(parser%tokens)) then
                        if (parser%tokens(lookahead)%kind == TK_KEYWORD .and. &
                            parser%tokens(lookahead)%text == suffix_keyword) then
                            found_end = .true.
                            return
                        end if
                    end if
                end if
            else if (token%text == trimmed_keyword) then
                found_end = .true.
                return
            end if
        end do
    end function check_end_keyword_match

    logical function has_meaningful_tokens(tokens, stmt_start, stmt_end) &
            result(has_meaningful)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        integer :: j

        has_meaningful = .false.
        if (stmt_end < stmt_start) return

        do j = stmt_start, stmt_end
            select case (tokens(j)%kind)
            case (TK_EOF, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE)
                cycle
            case default
                if (len_trim(tokens(j)%text) > 0) then
                    has_meaningful = .true.
                    return
                end if
            end select
        end do
    end function has_meaningful_tokens

    subroutine advance_past_empty_statement(parser, stmt_start, stmt_end)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(in) :: stmt_start, stmt_end

        if (stmt_end < stmt_start) then
            parser%current_token = parser%current_token + 1
        else
            parser%current_token = stmt_end + 1
        end if
    end subroutine advance_past_empty_statement

    subroutine extract_statement_tokens(tokens, stmt_start, stmt_end, stmt_tokens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(token_t), allocatable, intent(out), target :: stmt_tokens(:)
        integer :: token_count, first, last

        ! Clamp to what the token array actually holds. A statement that runs
        ! to the end of input can leave `stmt_end` past the last token, and
        ! reading `tokens(stmt_end)` then walks off the array - which is a
        ! silent corruption on some builds and a segfault on others rather
        ! than an error anywhere near the cause.
        first = max(1, min(stmt_start, size(tokens)))
        last = max(first, min(stmt_end, size(tokens)))
        token_count = last - first + 1
        allocate (stmt_tokens(token_count + 1))
        stmt_tokens(1:token_count) = tokens(first:last)
        stmt_tokens(token_count + 1)%kind = TK_EOF
        stmt_tokens(token_count + 1)%text = ""
        stmt_tokens(token_count + 1)%line = tokens(last)%line
        stmt_tokens(token_count + 1)%column = tokens(last)%column + 1
    end subroutine extract_statement_tokens

    subroutine parse_and_add_statement(stmt_tokens, arena, parent_index, &
            callbacks, body_indices, stmt_count, parent_parser)
        type(token_t), intent(in) :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(inout) :: stmt_count
        type(parser_state_t), intent(in) :: parent_parser

        integer, allocatable :: stmt_indices(:)
        integer :: k

        if (present(parent_index)) then
            stmt_indices = parse_basic_statement_multi(stmt_tokens, arena, &
                parent_index, callbacks, parent_parser=parent_parser)
        else
            stmt_indices = parse_basic_statement_multi(stmt_tokens, arena, &
                callbacks=callbacks, parent_parser=parent_parser)
        end if

        do k = 1, size(stmt_indices)
            if (stmt_indices(k) > 0) then
                body_indices = [body_indices, stmt_indices(k)]
                stmt_count = stmt_count + 1
            end if
        end do
    end subroutine parse_and_add_statement

    subroutine release_statement_tokens(stmt_tokens)
        type(token_t), allocatable, intent(inout), target :: stmt_tokens(:)
        type(token_t), allocatable, target :: temp(:)

        call move_alloc(stmt_tokens, temp)
    end subroutine release_statement_tokens

    integer function next_statement_start(tokens, stmt_end) result(next_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_end

        next_index = stmt_end + 1
        if (next_index <= size(tokens)) then
            if (tokens(next_index)%kind == TK_OPERATOR .and. &
                tokens(next_index)%text == ";") then
                next_index = next_index + 1
            end if
        end if
    end function next_statement_start

    ! Helper function to determine how many tokens an expression consumes
    function parse_expression_length(parser, arena) result(length)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: length
        integer :: start_pos, expr_index

        start_pos = parser%current_token
        expr_index = parse_range(parser, arena)
        length = parser%current_token - start_pos
        if (length == 0) length = 1 ! At least one token
    end function parse_expression_length

end module parser_basic_statement_module
