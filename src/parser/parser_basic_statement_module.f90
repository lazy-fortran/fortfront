module parser_basic_statement_module
    ! Parser module for basic statement parsing and utilities
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_OPERATOR, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, to_lower
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
                                         consumed_count) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, intent(out), optional :: consumed_count
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
                               consumed_count=consumed_count)
            else
                stmt_indices = parse_basic_statement_core( &
                               tokens, arena, parent_index=parent_index, &
                               callbacks=local_callbacks)
            end if
        else
            if (present(consumed_count)) then
                stmt_indices = parse_basic_statement_core( &
                               tokens, arena, callbacks=local_callbacks, &
                               consumed_count=consumed_count)
            else
                stmt_indices = parse_basic_statement_core(tokens, arena, &
                                                          callbacks=local_callbacks)
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

        type(token_t) :: token
        integer :: stmt_count, stmt_index
        integer :: stmt_start, stmt_end, j
        type(token_t), allocatable, target :: stmt_tokens(:)
        logical :: found_end
        type(statement_callbacks_t) :: local_callbacks
        logical :: has_meaningful
        integer :: next_index
        logical :: is_select_case
        integer :: lookahead, extended_end
        character(len=:), allocatable :: lowered_text

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
                token = parser%peek()

                ! Skip whitespace, comments, and standalone semicolons
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

                if (parser%is_at_end()) exit
                token = parser%peek()

                ! Check for end keywords
                found_end = .false.
                if (token%kind == TK_KEYWORD) then
                    do j = 1, size(end_keywords)
                        if (token%text == trim(end_keywords(j))) then
                            found_end = .true.
                            exit
                        end if
                    end do
                    if (found_end) exit
                end if

                ! Parse statement until end of line
                stmt_start = parser%current_token
                stmt_end = stmt_start
                has_meaningful = .false.
                is_select_case = .false.

                if (token%kind == TK_KEYWORD) then
                    lowered_text = to_lower(token%text)
                    if (trim(lowered_text) == "select") then
                        lookahead = stmt_start + 1
                        do while (lookahead <= size(parser%tokens))
                            select case (parser%tokens(lookahead)%kind)
                            case (TK_WHITESPACE, TK_COMMENT)
                                lookahead = lookahead + 1
                                cycle
                            end select
                            exit
                        end do
                        if (lookahead <= size(parser%tokens)) then
                            if (parser%tokens(lookahead)%kind == TK_KEYWORD) then
                                lowered_text = to_lower(parser%tokens(lookahead)%text)
                                if (trim(lowered_text) == "case") then
                                    is_select_case = .true.
                                end if
                            end if
                        end if
                    end if
                end if

                if (is_select_case) then
                    extended_end = find_statement_end(parser%tokens, stmt_start)
                    if (extended_end >= stmt_start) then
                        stmt_end = extended_end
                        has_meaningful = .true.
                    else
                        is_select_case = .false.
                    end if
                end if

                if (.not. is_select_case) then
                    do j = stmt_start, size(parser%tokens)
                        select case (parser%tokens(j)%kind)
                        case (TK_EOF)
                            stmt_end = j
                            exit
                        case (TK_NEWLINE)
                            stmt_end = j - 1
                            exit
                        case (TK_OPERATOR)
                            if (parser%tokens(j)%text == ";") then
                                stmt_end = j - 1
                                exit
                            end if
                        end select

                        stmt_end = j
                        select case (parser%tokens(j)%kind)
                        case (TK_EOF, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE)
                            cycle
                        case default
                            if (len_trim(parser%tokens(j)%text) > 0) then
                                has_meaningful = .true.
                            end if
                        end select
                    end do

                end if

                if (.not. has_meaningful) then
                    if (stmt_end < stmt_start) then
                        parser%current_token = parser%current_token + 1
                    else
                        parser%current_token = stmt_end + 1
                    end if
                    cycle
                end if

                ! Extract and parse statement tokens
                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
                    stmt_tokens(1:stmt_end - stmt_start + 1) = &
                        parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""
                    stmt_tokens(stmt_end - stmt_start + 2)%line = &
                        parser%tokens(stmt_end)%line
                    stmt_tokens(stmt_end - stmt_start + 2)%column = &
                        parser%tokens(stmt_end)%column + 1

                    ! Parse statement; multi-variable declarations may expand results
                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: k
                        if (present(parent_index)) then
                            stmt_indices = parse_basic_statement_multi( &
                                           stmt_tokens, arena, parent_index, &
                                           local_callbacks)
                        else
                            stmt_indices = parse_basic_statement_multi( &
                                           stmt_tokens, arena, callbacks=local_callbacks)
                        end if

                        ! Add all parsed statements to body
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                                stmt_count = stmt_count + 1
                            end if
                        end do
                    end block

                    deallocate (stmt_tokens)
                end if

                next_index = stmt_end + 1
                if (next_index <= size(parser%tokens)) then
                    if (parser%tokens(next_index)%kind == TK_OPERATOR .and. &
                        parser%tokens(next_index)%text == ";") then
                        next_index = next_index + 1
                    end if
                end if
                parser%current_token = next_index
            end do
        end block
    end function parse_statement_body

    ! Helper function to determine how many tokens an expression consumes
    function parse_expression_length(parser, arena) result(length)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: length
        integer :: start_pos, expr_index

        start_pos = parser%current_token
        expr_index = parse_range(parser, arena)
        length = parser%current_token - start_pos
        if (length == 0) length = 1  ! At least one token
    end function parse_expression_length

end module parser_basic_statement_module
