    ! Parse a single CASE arm (default or value list) after its "case" keyword has
    ! been consumed. Appends to case_indices / sets default_index. ok is .false.
    ! only on a parse error the caller must propagate as select_index = 0.
    subroutine parse_case_arm(parser, arena, case_token, end_keywords, callbacks, &
                              case_indices, case_count, default_index, ok)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: case_token
        character(len=*), intent(in) :: end_keywords(:)
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(inout) :: case_indices(:)
        integer, intent(inout) :: case_count
        integer, intent(inout) :: default_index
        logical, intent(out) :: ok

        type(token_t) :: value_token
        integer :: case_block_index
        integer, allocatable :: value_indices(:), body_indices(:)
        logical :: values_ok

        ok = .true.

        ! Check for default case
        value_token = skip_layout(parser)

        if (value_token%kind == TK_KEYWORD .and. &
            trim(to_lower(value_token%text)) == "default") then
            value_token = parser%consume()  ! consume default

            ! Skip rest of current line
            call skip_rest_of_line(parser, value_token%line)

            ! Parse default case body using parse_statement_body
            body_indices = parse_statement_body(parser, arena, &
                                                end_keywords, callbacks)

            ! Store default case index
            default_index = push_case_default(arena, body_indices, &
                                              line=case_token%line, &
                                              column=case_token%column)
        else
            ! Regular case with values
            allocate (value_indices(0))
            if (value_token%kind == TK_OPERATOR .and. &
                value_token%text == "(") then

                value_token = parser%consume()  ! consume (
                call parse_case_value_list(parser, arena, case_token, &
                                           value_indices, values_ok)
                if (.not. values_ok) then
                    deallocate (value_indices)
                    allocate (value_indices(0))
                    ok = .false.
                    return
                end if
            else
                deallocate (value_indices)
                allocate (value_indices(0))
                ok = .false.
                return
            end if

            ! Skip rest of current line
            call skip_rest_of_line(parser, previous_token_line(parser))

            ! Parse case body statements using parse_statement_body
            body_indices = parse_statement_body(parser, arena, &
                                                end_keywords, callbacks)

            ! Create case block node and add to list
            if (size(value_indices) > 0) then
                case_block_index = push_case_block( &
                                   arena, value_indices, body_indices, &
                                   line=case_token%line, &
                                   column=case_token%column)
                case_indices = [case_indices, case_block_index]
                case_count = case_count + 1
            end if
        end if
    end subroutine parse_case_arm

    recursive integer function parse_select_case(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        type(token_t) :: case_token
        integer :: expr_index, default_index
        integer, allocatable :: case_indices(:)
        integer :: case_count, line, column
        character(len=64) :: keyword
        character(len=20), dimension(2) :: end_keywords
        type(statement_callbacks_t) :: callbacks
        logical :: arm_ok, header_ok

        call parse_select_header(parser, arena, "case", .false., line, column, &
                                 expr_index, header_ok)
        if (.not. header_ok) then
            select_index = 0
            return
        end if

        ! Parse case blocks
        allocate (case_indices(0))
        case_count = 0
        default_index = 0

        ! Define end keywords for statement parsing. "end select" terminates a
        ! case body so the last arm (which has no following "case") stops there
        ! instead of consuming statements after the construct. The "end select"
        ! form matches only end + select, not nested "end if"/"end do", which
        ! are consumed inside their own recursive statement parse.
        end_keywords(1) = "case"
        end_keywords(2) = "end select"

        callbacks = null_statement_callbacks()
        callbacks%parse_select_case => parse_select_case_callback

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                keyword = trim(to_lower(case_token%text))
                select case (keyword)
                case ("case")
                    case_token = parser%consume()  ! consume case
                    call parse_case_arm(parser, arena, case_token, end_keywords, &
                                        callbacks, case_indices, case_count, &
                                        default_index, arm_ok)
                    if (.not. arm_ok) then
                        select_index = 0
                        return
                    end if
                case ("end")
                    if (consume_end_select(parser)) exit
                case default
                    ! Other keyword, skip
                    parser%current_token = parser%current_token + 1
                end select
            else
                ! Not a keyword, skip
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select case node
        if (default_index > 0) then
            select_index = push_select_case_with_default( &
                           arena, expr_index, case_indices, default_index, &
                           line=line, column=column)
        else
            select_index = push_select_case(arena, expr_index, case_indices, &
                                            line=line, column=column)
        end if
    end function parse_select_case
