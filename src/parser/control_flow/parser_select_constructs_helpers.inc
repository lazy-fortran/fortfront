    ! Shared layout helper: skip whitespace, comments and newlines, returning the
    ! token at the new position. Mirrors the consume/peek skip loop used across the
    ! SELECT constructs, including its is_at_end early exit semantics.
    function skip_layout(parser) result(tok)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: tok

        tok = parser%peek()
        do while (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT .or. &
                  tok%kind == TK_NEWLINE)
            tok = parser%consume()
            if (parser%is_at_end()) exit
            tok = parser%peek()
        end do
    end function skip_layout

    ! Consume any remaining tokens on the given source line.
    subroutine skip_rest_of_line(parser, current_line)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(in) :: current_line

        type(token_t) :: skip_token

        do while (parser%current_token <= size(parser%tokens))
            skip_token = parser%peek()
            if (skip_token%line == current_line) then
                skip_token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_rest_of_line

    ! Line of the token just consumed, used as the basis for skip_rest_of_line.
    ! Falls back to line 1 when nothing has been consumed yet.
    function previous_token_line(parser) result(current_line)
        type(parser_state_t), intent(inout) :: parser
        integer :: current_line

        if (parser%current_token > 1) then
            current_line = parser%tokens(parser%current_token - 1)%line
        else
            current_line = 1
        end if
    end function previous_token_line

    ! Handle the "end" keyword inside a SELECT body: consume "end", skip layout,
    ! and report whether the following keyword is "select" (also consuming it).
    ! A true result means the construct terminates here.
    function consume_end_select(parser) result(is_end_select)
        type(parser_state_t), intent(inout) :: parser
        logical :: is_end_select

        type(token_t) :: end_token, next_token

        is_end_select = .false.
        end_token = parser%consume()  ! consume end

        next_token = skip_layout(parser)

        if (next_token%kind == TK_KEYWORD .and. &
            trim(to_lower(next_token%text)) == "select") then
            next_token = parser%consume()  ! consume select
            is_end_select = .true.
        end if
    end function consume_end_select

    ! Parse the shared SELECT construct header: consume "select", the construct
    ! keyword (case/type/rank), "(", the selector, then the closing ")". Sets
    ! line/column from the "select" token and selector_index from the selector.
    ! ok is .false. when any expected token is missing; the caller then returns
    ! select_index = 0, exactly as the inline preamble used to. use_type_selector
    ! selects parse_select_type_selector (associate syntax) over a plain
    ! expression.
    subroutine parse_select_header(parser, arena, keyword, use_type_selector, &
                                   line, column, selector_index, ok)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: keyword
        logical, intent(in) :: use_type_selector
        integer, intent(out) :: line, column, selector_index
        logical, intent(out) :: ok

        type(token_t) :: select_token, keyword_token, lparen_token, rparen_token

        ok = .false.
        selector_index = 0

        ! Consume select
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect the construct keyword (case/type/rank)
        keyword_token = parser%consume()
        if (keyword_token%kind /= TK_KEYWORD .or. &
            to_lower(keyword_token%text) /= keyword) return

        ! Expect (
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") return

        ! Parse selector expression
        if (use_type_selector) then
            selector_index = parse_select_type_selector(parser, arena)
        else
            selector_index = parse_expression_until(parser, arena, [")"])
        end if
        if (selector_index <= 0) then
            selector_index = 0
            return
        end if

        rparen_token = parser%peek()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") return
        rparen_token = parser%consume()

        ok = .true.
    end subroutine parse_select_header

    subroutine parse_case_value_list(parser, arena, case_token, value_indices, success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: case_token
        integer, allocatable, intent(out) :: value_indices(:)
        logical, intent(out) :: success

        type(token_t), allocatable :: tokens_buffer(:)
        type(token_t) :: token, sub_token
        type(parser_state_t) :: sub_parser
        integer :: lower_index, upper_index, range_index
        integer :: start_pos, end_pos, depth, i, buffer_len

        success = .true.
        allocate (value_indices(0))

        start_pos = parser%current_token
        depth = 1
        end_pos = start_pos - 1

        do i = start_pos, size(parser%tokens)
            token = parser%tokens(i)
            if (token%kind == TK_OPERATOR) then
                select case (token%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                    if (depth == 0) then
                        end_pos = i - 1
                        exit
                    end if
                end select
            end if
        end do

        if (end_pos < start_pos) then
            success = .false.
            if (depth == 0) parser%current_token = end_pos + 2
            return
        end if

        buffer_len = end_pos - start_pos + 2
        allocate (tokens_buffer(buffer_len))
        tokens_buffer(1:buffer_len - 1) = parser%tokens(start_pos:end_pos)
        tokens_buffer(buffer_len)%kind = TK_EOF
        tokens_buffer(buffer_len)%text = ""
        tokens_buffer(buffer_len)%line = parser%tokens(end_pos)%line
        tokens_buffer(buffer_len)%column = parser%tokens(end_pos)%column

        sub_parser = create_parser_state(tokens_buffer)
        if (associated(parser%diagnostic_sink)) then
            sub_parser%diagnostic_sink => parser%diagnostic_sink
        end if

        success = .true.

        do
            sub_token = sub_parser%peek()
            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ":") then
                ! Leading colon: one-sided range with no lower bound (:hi).
                lower_index = 0
            else
                lower_index = parse_expression_until(sub_parser, arena, &
                                                     [":", ","])
                if (lower_index <= 0) then
                    success = .false.
                    exit
                end if
                sub_token = sub_parser%peek()
            end if

            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ":") then
                sub_token = sub_parser%consume()
                upper_index = parse_expression_until(sub_parser, arena, [","])
                if (upper_index <= 0) then
                    upper_index = 0
                end if
                range_index = push_case_range( &
                              arena, lower_index, upper_index, line=case_token%line, &
                              column=case_token%column)
                value_indices = [value_indices, range_index]
                sub_token = sub_parser%peek()
            else
                value_indices = [value_indices, lower_index]
            end if

            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ",") then
                sub_token = sub_parser%consume()
                cycle
            else
                exit
            end if
        end do

        if (.not. success .or. size(value_indices) == 0) then
            value_indices = [integer ::]
            success = .false.
        end if

        parser%current_token = end_pos + 2
    end subroutine parse_case_value_list

    function parse_select_type_selector(parser, arena) result(selector_index)
        ! Parse the selector part of SELECT TYPE:
        !   SELECT TYPE (x)           - just an expression
        !   SELECT TYPE (y => x)      - associate_name => selector
        ! For the associate syntax, we create an assignment node representing y => x
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: selector_index

        type(token_t) :: token, next_token
        integer :: name_index
        integer :: lookahead_idx
        logical :: has_associate

        selector_index = 0

        ! Skip whitespace to find the first real token
        token = parser%peek()
        do while (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT .or. &
                  token%kind == TK_NEWLINE)
            token = parser%consume()
            if (parser%is_at_end()) return
            token = parser%peek()
        end do

        ! Check if this is associate syntax: identifier => selector
        has_associate = .false.
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            ! Look ahead for =>
            lookahead_idx = parser%current_token + 1
            do while (lookahead_idx <= size(parser%tokens))
                next_token = parser%tokens(lookahead_idx)
                if (next_token%kind == TK_WHITESPACE .or. &
                    next_token%kind == TK_COMMENT) then
                    lookahead_idx = lookahead_idx + 1
                    cycle
                else if (next_token%kind == TK_OPERATOR .and. &
                         next_token%text == "=>") then
                    has_associate = .true.
                    exit
                else
                    exit
                end if
            end do
        end if

        if (has_associate) then
            ! Parse: associate_name => selector
            ! First, consume the associate name
            name_index = push_identifier(arena, token%text, line=token%line, &
                                         column=token%column)
            token = parser%consume()  ! consume the identifier

            ! Skip whitespace to get to =>
            token = parser%peek()
            do while (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT)
                token = parser%consume()
                if (parser%is_at_end()) return
                token = parser%peek()
            end do

            ! Consume =>
            if (token%kind == TK_OPERATOR .and. token%text == "=>") then
                token = parser%consume()
            else
                return  ! Error: expected =>
            end if

            ! Parse the selector expression after =>
            selector_index = parse_expression_until(parser, arena, [")"])
            if (selector_index <= 0) return

            ! Create a pointer assignment node to represent y => x
            selector_index = push_pointer_assignment(arena, name_index, &
                                                     selector_index, &
                                                     line=parser%tokens(1)%line, &
                                                     column=parser%tokens(1)%column)
        else
            ! Simple selector (just an expression)
            selector_index = parse_expression_until(parser, arena, [")"])
        end if
    end function parse_select_type_selector

    ! Consume an optional kind selector following a guard type name, e.g. the
    ! "(8)" in "type is (real(8))". Returns the parenthesized text (including
    ! the parentheses) so the caller can fold it into the type name; returns an
    ! empty string when no kind selector is present. Leaves the parser position
    ! unchanged in that case.
    subroutine consume_guard_kind_suffix(parser, kind_suffix)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: kind_suffix

        type(token_t) :: tok
        integer :: depth, saved_pos

        kind_suffix = ""
        saved_pos = parser%current_token

        tok = parser%peek()
        do while (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT .or. &
                  tok%kind == TK_NEWLINE)
            tok = parser%consume()
            if (parser%is_at_end()) then
                parser%current_token = saved_pos
                return
            end if
            tok = parser%peek()
        end do

        if (tok%kind /= TK_OPERATOR .or. tok%text /= "(") then
            parser%current_token = saved_pos
            return
        end if

        depth = 0
        do while (.not. parser%is_at_end())
            tok = parser%consume()
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                ! drop layout inside the selector
            case default
                if (tok%kind == TK_OPERATOR .and. tok%text == "(") then
                    depth = depth + 1
                    kind_suffix = kind_suffix // "("
                else if (tok%kind == TK_OPERATOR .and. tok%text == ")") then
                    depth = depth - 1
                    kind_suffix = kind_suffix // ")"
                    if (depth == 0) return
                else
                    kind_suffix = kind_suffix // tok%text
                end if
            end select
        end do

        ! Unbalanced parentheses: restore and report no suffix
        parser%current_token = saved_pos
        kind_suffix = ""
    end subroutine consume_guard_kind_suffix
