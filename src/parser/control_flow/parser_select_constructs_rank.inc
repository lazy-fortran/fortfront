    ! Parse a single RANK arm (RANK DEFAULT, RANK (*), or RANK (n)) after its
    ! "rank" keyword (passed as rank_block_token, already consumed) is known.
    ! Appends to rank_indices or sets default_index. ok is .false. only on a
    ! parse error the caller must propagate as select_index = 0.
    subroutine parse_rank_arm(parser, arena, rank_block_token, end_keywords, &
                              callbacks, rank_indices, default_index, ok)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: rank_block_token
        character(len=*), intent(in) :: end_keywords(:)
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(inout) :: rank_indices(:)
        integer, intent(inout) :: default_index
        logical, intent(out) :: ok

        type(token_t) :: value_token, next_token
        integer :: rank_block_index, rank_value
        integer, allocatable :: body_indices(:)
        integer :: block_line, block_column

        ok = .true.
        block_line = rank_block_token%line
        block_column = rank_block_token%column

        ! Check for default or star rank
        value_token = skip_layout(parser)

        if (value_token%kind == TK_KEYWORD .and. &
            trim(to_lower(value_token%text)) == "default") then
            ! RANK DEFAULT case
            value_token = parser%consume()  ! consume default

            ! Skip rest of current line
            call skip_rest_of_line(parser, value_token%line)

            ! Parse default rank body
            body_indices = parse_statement_body(parser, arena, &
                                                end_keywords, callbacks)

            ! Create default rank block
            default_index = push_rank_block(arena, -1, body_indices, &
                                            line=block_line, column=block_column)
        else if (value_token%kind == TK_OPERATOR .and. &
                 value_token%text == "(") then
            ! Regular RANK (n) or RANK (*)
            value_token = parser%consume()  ! consume (

            ! Get rank value
            value_token = skip_layout(parser)

            if (value_token%kind == TK_OPERATOR .and. value_token%text == "*") then
                ! RANK (*) - star rank
                value_token = parser%consume()
                rank_value = -2  ! Use -2 for star rank
            else if (value_token%kind == TK_NUMBER) then
                ! RANK (n) - specific rank
                read (value_token%text, *) rank_value
                value_token = parser%consume()
            else
                ok = .false.
                return
            end if

            ! Expect )
            next_token = skip_layout(parser)

            if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
                ok = .false.
                return
            end if
            next_token = parser%consume()  ! consume )

            ! Skip rest of current line
            call skip_rest_of_line(parser, previous_token_line(parser))

            ! Parse rank body statements
            body_indices = parse_statement_body(parser, arena, &
                                                end_keywords, callbacks)

            ! Create rank block node
            rank_block_index = push_rank_block(arena, rank_value, body_indices, &
                                               line=block_line, column=block_column)
            rank_indices = [rank_indices, rank_block_index]
        end if
    end subroutine parse_rank_arm

    recursive function parse_select_rank(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: rank_block_token
        integer :: selector_index, default_index
        integer, allocatable :: rank_indices(:)
        integer :: line, column
        character(len=:), allocatable :: rank_keyword
        character(len=20), dimension(2) :: end_keywords
        type(statement_callbacks_t) :: callbacks
        logical :: arm_ok, header_ok

        call parse_select_header(parser, arena, "rank", .false., line, column, &
                                 selector_index, header_ok)
        if (.not. header_ok) then
            select_index = 0
            return
        end if

        ! Parse rank blocks
        allocate (rank_indices(0))
        default_index = 0

        end_keywords(1) = "rank"
        ! "end select" specifically (matched as end followed by select) terminates
        ! a rank body so a trailing statement after end select is a sibling of
        ! the select node, not absorbed into the last arm. A bare end or
        ! end if/end do inside an arm is not matched (check_end_keyword_match
        ! requires the select suffix), which is why a plain "end" stays excluded.
        end_keywords(2) = "end select"

        callbacks = null_statement_callbacks()
        callbacks%parse_select_rank => parse_select_rank

        do while (parser%current_token <= size(parser%tokens))
            rank_block_token = parser%peek()

            if (rank_block_token%kind == TK_KEYWORD) then
                rank_keyword = trim(to_lower(rank_block_token%text))
                select case (rank_keyword)
                case ("rank")
                    rank_block_token = parser%consume()  ! consume rank
                    call parse_rank_arm(parser, arena, rank_block_token, &
                                        end_keywords, callbacks, rank_indices, &
                                        default_index, arm_ok)
                    if (.not. arm_ok) then
                        select_index = 0
                        return
                    end if
                case ("end")
                    if (consume_end_select(parser)) exit
                    ! If not end select, we already consumed end and
                    ! will continue to next iteration to parse what follows
                case default
                    parser%current_token = parser%current_token + 1
                end select
            else
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select rank node
        if (default_index > 0) then
            select_index = push_select_rank_with_default( &
                           arena, selector_index, rank_indices, default_index, &
                           line=line, column=column)
        else
            select_index = push_select_rank(arena, selector_index, rank_indices, &
                                            line=line, column=column)
        end if
    end function parse_select_rank
