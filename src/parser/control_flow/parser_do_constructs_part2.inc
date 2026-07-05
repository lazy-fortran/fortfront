
    ! Helper function for parsing do while from do token
    function parse_do_while_from_do(parser, arena, line, column, loop_label) &
        result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line, column
        character(len=*), intent(in), optional :: loop_label
        integer :: loop_index

        type(token_t) :: while_token, lparen_token, rparen_token
        integer :: condition_index
        integer, allocatable :: body_indices(:)
        type(statement_callbacks_t) :: callbacks

        call initialize_if_hooks()

        ! Consume 'while'
        while_token = parser%consume()
        if (while_token%kind /= TK_KEYWORD .or. while_token%text /= "while") then
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            return
        end if

        ! Parse condition
        condition_index = parse_logical_or(parser, arena)

        ! Expect ')'
        rparen_token = parser%consume()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            return
        end if

        callbacks = build_do_body_callbacks()

        if (present(loop_label)) then
            loop_index = push_do_while(arena, condition_index, &
                                       body_indices=[integer ::], &
                                       loop_label=loop_label, line=line, column=column)
        else
            loop_index = push_do_while(arena, condition_index, &
                                       body_indices=[integer ::], &
                                       line=line, column=column)
        end if

        if (loop_index > 0) then
            call capture_trailing_comment(parser, arena, loop_index)
        end if

        ! Parse body statements until 'end' (same logic as if blocks)
        block
            integer, allocatable :: temp_body_indices(:)
            type(token_t) :: token
            integer :: stmt_start, stmt_end, j, token_count, next_index
            type(token_t), allocatable, target :: stmt_tokens(:)

            allocate (temp_body_indices(0))

            do while (.not. parser%is_at_end())
                token = parser%peek()

                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Skip whitespace/newlines/comments after "end"
                    next_index = skip_trivia_tokens(parser%tokens, &
                                                    parser%current_token + 1)
                    if (next_index <= size(parser%tokens)) then
                        if (parser%tokens(next_index)%kind == TK_KEYWORD .and. &
                            parser%tokens(next_index)%text == "do") then
                            exit
                        end if
                    end if
                end if

                stmt_start = parser%current_token
                stmt_end = stmt_start

                if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                    if (parser%tokens(stmt_start)%text == "enddo" .or. &
                        parser%tokens(stmt_start)%text == "end do") then
                        parser%current_token = stmt_start
                        exit
                    else if (parser%tokens(stmt_start)%text == "end") then
                        ! Skip whitespace/newlines/comments after "end"
                        next_index = skip_trivia_tokens(parser%tokens, stmt_start + 1)
                        if (next_index <= size(parser%tokens)) then
                            if (parser%tokens(next_index)%kind == TK_KEYWORD .and. &
                                parser%tokens(next_index)%text == "do") then
                                parser%current_token = stmt_start
                                exit
                            end if
                        else
                            parser%current_token = stmt_start
                            exit
                        end if
                    end if
                end if

                do j = stmt_start, size(parser%tokens)
                    select case (parser%tokens(j)%kind)
                    case (TK_EOF)
                        stmt_end = j
                        exit
                    case (TK_NEWLINE)
                        if (j == stmt_start) then
                            stmt_end = j
                        else
                            stmt_end = j - 1
                        end if
                        exit
                    end select
                    stmt_end = j
                end do

                if (stmt_end >= stmt_start) then
                    token_count = stmt_end - stmt_start + 1
                    allocate (stmt_tokens(token_count + 1))
                    stmt_tokens(1:token_count) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(token_count + 1)%kind = TK_EOF
                    stmt_tokens(token_count + 1)%text = ""
                    stmt_tokens(token_count + 1)%line = parser%tokens(stmt_end)%line
                    stmt_tokens(token_count + 1)%column = &
                        parser%tokens(stmt_end)%column + 1

                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: n

                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                                  loop_index, &
                                                                  callbacks)

                        do n = 1, size(stmt_indices)
                            if (stmt_indices(n) > 0) then
                                temp_body_indices = [temp_body_indices, &
                                                     stmt_indices(n)]
                            end if
                        end do
                    end block

                    block
                        type(token_t), allocatable, target :: temp(:)
                        call move_alloc(stmt_tokens, temp)
                    end block
                end if

                parser%current_token = stmt_end + 1
            end do

            body_indices = temp_body_indices
        end block

        ! Consume 'end do' tokens
        block
            type(token_t) :: token
            integer :: check_idx
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                while_token = parser%consume()  ! consume 'end'
                ! Skip whitespace/newlines/comments to find "do"
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_WHITESPACE .or. token%kind == TK_NEWLINE .or. &
                        token%kind == TK_COMMENT) then
                        while_token = parser%consume()  ! skip trivia
                    else
                        exit
                    end if
                end do
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "do") then
                    while_token = parser%consume()  ! consume 'do'
                end if
            end if
        end block

        if (loop_index > 0) then
            if (allocated(arena%entries(loop_index)%node)) then
                select type (node => arena%entries(loop_index)%node)
                type is (do_while_node)
                    if (allocated(body_indices)) then
                        node%body_indices = body_indices
                    end if
                end select
            end if
        end if

        if (allocated(body_indices)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(body_indices, temp)
            end block
        end if
    end function parse_do_while_from_do
