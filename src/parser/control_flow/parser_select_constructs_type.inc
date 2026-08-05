    ! Parse a single TYPE IS / CLASS IS / CLASS DEFAULT guard arm after its
    ! "type"/"class" keyword (passed as guard_token, already consumed) is known.
    ! Appends to guard_indices or sets default_index. ok is .false. only on a
    ! parse error the caller must propagate as select_index = 0.
    subroutine parse_type_guard_arm(parser, arena, guard_token, guard_keyword, &
                                    end_keywords, callbacks, guard_indices, &
                                    default_index, ok)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: guard_token
        character(len=*), intent(in) :: guard_keyword
        character(len=*), intent(in) :: end_keywords(:)
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(inout) :: guard_indices(:)
        integer, intent(inout) :: default_index
        logical, intent(out) :: ok

        type(token_t) :: is_token, next_token, type_name_token
        integer, allocatable :: body_indices(:)

        ok = .true.

        is_token = skip_layout(parser)

        if (guard_keyword == "class") then
            if ((is_token%kind == TK_KEYWORD .or. &
                 is_token%kind == TK_IDENTIFIER) .and. &
                trim(to_lower(is_token%text)) == "default") then
                is_token = parser%consume()  ! consume default

                ! Skip rest of current line
                call skip_rest_of_line(parser, is_token%line)

                ! Parse default guard body
                body_indices = parse_statement_body(parser, arena, &
                                                    end_keywords, callbacks)

                ! Create default guard
                default_index = push_type_guard_block( &
                                arena, "class_default", 0, body_indices, &
                                line=guard_token%line, column=guard_token%column)
                return
            end if
        end if

        ! Expect is
        if ((is_token%kind /= TK_KEYWORD .and. &
             is_token%kind /= TK_IDENTIFIER) .or. &
            trim(to_lower(is_token%text)) /= "is") then
            ok = .false.
            return
        end if
        is_token = parser%consume()  ! consume is

        ! Expect (
        next_token = skip_layout(parser)

        if (next_token%kind /= TK_OPERATOR .or. next_token%text /= "(") then
            ok = .false.
            return
        end if
        next_token = parser%consume()  ! consume (

        ! Parse type name
        type_name_token = skip_layout(parser)

        if (type_name_token%kind == TK_KEYWORD .and. &
            trim(to_lower(type_name_token%text)) == "default") then
            ! CLASS DEFAULT case
            type_name_token = parser%consume()  ! consume default

            ! Expect )
            next_token = skip_layout(parser)

            if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
                ok = .false.
                return
            end if
            next_token = parser%consume()  ! consume )

            ! Skip rest of current line
            call skip_rest_of_line(parser, type_name_token%line)

            ! Parse default guard body
            body_indices = parse_statement_body(parser, arena, &
                                                end_keywords, callbacks)

            ! Create default guard
            default_index = push_type_guard_block( &
                            arena, "class_default", 0, body_indices, &
                            line=guard_token%line, column=guard_token%column)
        else
            ! Regular TYPE IS or CLASS IS
            call parse_named_type_guard(parser, arena, guard_token, guard_keyword, &
                                        type_name_token, end_keywords, callbacks, &
                                        guard_indices, ok)
        end if
    end subroutine parse_type_guard_arm

    ! Parse a regular "type is (NAME)" / "class is (NAME)" guard, given the type
    ! name token already positioned (after the opening "(" was consumed). Parses
    ! an optional kind selector, the closing ")", the guard body, and appends the
    ! resulting guard block to guard_indices. ok is .false. on a parse error.
    subroutine parse_named_type_guard(parser, arena, guard_token, guard_keyword, &
                                      type_name_token, end_keywords, callbacks, &
                                      guard_indices, ok)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: guard_token
        character(len=*), intent(in) :: guard_keyword
        type(token_t), intent(inout) :: type_name_token
        character(len=*), intent(in) :: end_keywords(:)
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(inout) :: guard_indices(:)
        logical, intent(out) :: ok

        type(token_t) :: next_token
        integer :: guard_block_index, type_name_index
        integer, allocatable :: body_indices(:)
        character(len=20) :: guard_type
        integer :: name_line, name_column
        character(len=:), allocatable :: full_type_name
        character(len=:), allocatable :: kind_suffix

        ok = .true.

        ! F2018 R1155/R1156: the type-guard parentheses must hold a
        ! type-spec. An empty or otherwise nameless "( )" is a syntax error,
        ! not a construct fortfront may silently drop.
        if (type_name_token%kind /= TK_IDENTIFIER .and. &
            type_name_token%kind /= TK_KEYWORD) then
            if (guard_keyword == "type") then
                call parser%error_at_token( &
                    "Syntax error in TYPE IS specification: expected a "// &
                    "type-spec inside the parentheses", type_name_token)
            else
                call parser%error_at_token( &
                    "Syntax error in CLASS IS specification: expected a "// &
                    "type-spec inside the parentheses", type_name_token)
            end if
            ok = .false.
            return
        end if

        name_line = type_name_token%line
        name_column = type_name_token%column

        full_type_name = type_name_token%text
        type_name_token = parser%consume()

        ! Capture an optional kind selector, e.g. real(8) or real(kind=8),
        ! so real and real(8) become distinct guards instead of colliding.
        call consume_guard_kind_suffix(parser, kind_suffix)
        if (len(kind_suffix) > 0) then
            full_type_name = full_type_name // kind_suffix
        end if

        type_name_index = push_identifier(arena, full_type_name, &
                                          line=name_line, column=name_column)

        ! Expect )
        next_token = skip_layout(parser)

        if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
            ok = .false.
            return
        end if
        next_token = parser%consume()  ! consume )

        ! Skip rest of current line
        call skip_rest_of_line(parser, previous_token_line(parser))

        ! Parse guard body statements
        body_indices = parse_statement_body(parser, arena, end_keywords, callbacks)

        ! Determine guard type
        if (guard_keyword == "type") then
            guard_type = "type_is"
        else
            guard_type = "class_is"
        end if

        ! Create guard block node
        guard_block_index = push_type_guard_block( &
                            arena, guard_type, type_name_index, body_indices, &
                            line=guard_token%line, column=guard_token%column)
        guard_indices = [guard_indices, guard_block_index]
    end subroutine parse_named_type_guard

    recursive function parse_select_type(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: guard_token
        integer :: selector_index, default_index
        integer, allocatable :: guard_indices(:)
        integer :: line, column
        character(len=:), allocatable :: guard_keyword
        character(len=20), dimension(4) :: end_keywords
        type(statement_callbacks_t) :: callbacks
        logical :: arm_ok, header_ok

        call ensure_if_do_registration_bridge()

        ! Header selector may include associate_name => selector syntax
        call parse_select_header(parser, arena, "type", .true., line, column, &
                                 selector_index, header_ok)
        if (.not. header_ok) then
            select_index = 0
            return
        end if

        ! Parse type guard blocks
        allocate (guard_indices(0))
        default_index = 0

        end_keywords(1) = "type"
        end_keywords(2) = "class"
        end_keywords(3) = "contains"
        ! "end select" specifically (matched as end followed by select) terminates
        ! a guard body so a trailing statement after end select is a sibling of
        ! the select node, not absorbed into the last arm. A bare end or
        ! end if/end do inside an arm is not matched.
        end_keywords(4) = "end select"

        callbacks = null_statement_callbacks()
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_select_type => parse_select_type
        callbacks%parse_select_rank => parse_select_rank
        callbacks%parse_block => parse_block_construct_bridge

        do while (parser%current_token <= size(parser%tokens))
            guard_token = parser%peek()

            if (guard_token%kind == TK_KEYWORD) then
                guard_keyword = trim(to_lower(guard_token%text))
                select case (guard_keyword)
                case ("type", "class")
                    guard_token = parser%consume()  ! consume type or class
                    call parse_type_guard_arm(parser, arena, guard_token, &
                                              guard_keyword, end_keywords, callbacks, &
                                              guard_indices, default_index, arm_ok)
                    if (.not. arm_ok) then
                        select_index = 0
                        return
                    end if
                case ("end")
                    if (consume_end_select(parser)) exit
                    ! If not end select, we already consumed end and
                    ! will continue to next iteration to parse what follows
                case ("contains", "function", "subroutine")
                    ! Weve reached something that shouldnt be inside
                    ! select type - likely we missed an end select.
                    ! Dont consume; let the parent parser handle it.
                    exit
                case default
                    parser%current_token = parser%current_token + 1
                end select
            else
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select type node
        if (default_index > 0) then
            select_index = push_select_type_with_default( &
                           arena, selector_index, guard_indices, default_index, &
                           line=line, column=column)
        else
            select_index = push_select_type(arena, selector_index, guard_indices, &
                                            line=line, column=column)
        end if
    end function parse_select_type
