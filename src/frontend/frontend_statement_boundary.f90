module frontend_statement_boundary
    ! Statement boundary detection and control-flow awareness
    ! Handles finding statement boundaries and inline construct detection

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_NEWLINE, &
                          TK_OPERATOR, TK_WHITESPACE, TK_COMMENT, &
                          TK_IDENTIFIER, to_lower

    implicit none
    private

    ! Public statement boundary interface
    public :: find_statement_boundary

contains

    pure integer function next_significant_token_index(tokens, start_index) &
        result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index

        idx = start_index
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case default
                return
            end select
        end do
        idx = 0
    end function next_significant_token_index

    pure logical function inline_where_parenthetical(tokens, start_index) &
        result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer :: idx
        integer :: depth

        is_inline = .false.
        depth = 1
        idx = start_index
        do while (idx <= size(tokens) .and. depth > 0)
            if (tokens(idx)%kind == TK_OPERATOR) then
                select case (tokens(idx)%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                end select
            else if (tokens(idx)%kind == TK_EOF) then
                return
            end if
            idx = idx + 1
        end do
        if (depth > 0) return

        idx = next_significant_token_index(tokens, idx)
        if (idx == 0) return
        select case (tokens(idx)%kind)
        case (TK_NEWLINE, TK_EOF)
            return
        case default
            is_inline = .true.
        end select
    end function inline_where_parenthetical

    pure logical function inline_where_colon_variant(tokens, start_index) &
        result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer :: idx
        integer :: colon_index

        is_inline = .false.
        colon_index = 0
        idx = start_index
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_NEWLINE, TK_EOF)
                return
            case (TK_OPERATOR)
                if (tokens(idx)%text == ":") then
                    colon_index = idx + 1
                    exit
                end if
            end select
            idx = idx + 1
        end do

        if (colon_index == 0) return

        idx = next_significant_token_index(tokens, colon_index)
        if (idx == 0) return
        select case (tokens(idx)%kind)
        case (TK_NEWLINE, TK_EOF)
            return
        case default
            is_inline = .true.
        end select
    end function inline_where_colon_variant

    pure logical function is_inline_where_statement(tokens, where_index) &
        result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: where_index
        integer :: first_token

        is_inline = .false.
        if (where_index < 1 .or. where_index > size(tokens)) return

        first_token = next_significant_token_index(tokens, where_index + 1)
        if (first_token == 0) return

        if (tokens(first_token)%kind == TK_OPERATOR) then
            if (tokens(first_token)%text == "(") then
                is_inline = inline_where_parenthetical(tokens, first_token + 1)
                return
            end if
        end if

        is_inline = inline_where_colon_variant(tokens, first_token)
    end function is_inline_where_statement

    pure logical function is_inline_forall_statement(tokens, forall_index) &
        result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: forall_index
        integer :: idx, depth

        is_inline = .false.
        if (forall_index < 1 .or. forall_index > size(tokens)) return

        idx = next_significant_token_index(tokens, forall_index + 1)
        if (idx == 0) return

        if (tokens(idx)%kind /= TK_OPERATOR) return
        if (tokens(idx)%text /= "(") return

        depth = 1
        idx = idx + 1
        do while (idx <= size(tokens) .and. depth > 0)
            if (tokens(idx)%kind == TK_OPERATOR) then
                select case (tokens(idx)%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                end select
            else if (tokens(idx)%kind == TK_EOF) then
                return
            end if
            idx = idx + 1
        end do

        if (depth > 0) return

        idx = next_significant_token_index(tokens, idx)
        if (idx == 0) return
        select case (tokens(idx)%kind)
        case (TK_NEWLINE, TK_EOF)
            return
        case default
            is_inline = .true.
        end select
    end function is_inline_forall_statement

    pure integer function find_statement_start(tokens, start_pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos

        idx = start_pos
        do while (idx <= size(tokens))
            if (tokens(idx)%kind == TK_NEWLINE) then
                idx = idx + 1
                cycle
            end if

            if (tokens(idx)%kind == TK_OPERATOR) then
                if (tokens(idx)%text == ";") then
                    idx = idx + 1
                    cycle
                end if
            end if

            exit
        end do
    end function find_statement_start

    pure subroutine detect_multiline_construct(tokens, stmt_start, &
                                               is_multiline, nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start
        logical, intent(out) :: is_multiline
        integer, intent(out) :: nesting_level
        integer :: i, max_idx

        is_multiline = .false.
        nesting_level = 0
        if (stmt_start < 1 .or. stmt_start > size(tokens)) return
        if (tokens(stmt_start)%kind /= TK_KEYWORD) return

        select case (tokens(stmt_start)%text)
        case ("if")
            max_idx = min(stmt_start + 20, size(tokens))
            do i = stmt_start + 1, max_idx
                if (tokens(i)%kind == TK_KEYWORD) then
                    if (tokens(i)%text == "then") then
                        is_multiline = .true.
                        nesting_level = 1
                        return
                    end if
                else if (tokens(i)%kind == TK_NEWLINE) then
                    return
                end if
            end do
        case ("do")
            is_multiline = .true.
            nesting_level = 1
        case ("select")
            if (begins_select_construct(tokens, stmt_start)) then
                is_multiline = .true.
                nesting_level = 1
            end if
        case ("where")
            if (.not. is_inline_where_statement(tokens, stmt_start)) then
                max_idx = min(stmt_start + 20, size(tokens))
                do i = stmt_start + 1, max_idx
                    if (tokens(i)%kind == TK_NEWLINE) then
                        is_multiline = .true.
                        nesting_level = 1
                        return
                    else if (tokens(i)%kind == TK_KEYWORD) then
                        if (tokens(i)%text == "end" .or. &
                            tokens(i)%text == "elsewhere") then
                            is_multiline = .true.
                            nesting_level = 1
                            return
                        end if
                    end if
                end do
            end if
        case ("submodule")
            is_multiline = .true.
            nesting_level = 1
        case ("type")
            ! Distinguish derived type definitions from derived type declarations.
            ! Declarations use type(name) :: var and should not consume until end type.
            max_idx = min(stmt_start + 20, size(tokens))
            do i = stmt_start + 1, max_idx
                select case (tokens(i)%kind)
                case (TK_WHITESPACE, TK_COMMENT)
                    cycle
                case (TK_NEWLINE)
                    exit
                case (TK_OPERATOR)
                    if (tokens(i)%text == "(") return
                    exit
                case (TK_KEYWORD)
                    if (tokens(i)%text == "is") return
                    exit
                case default
                    exit
                end select
            end do
            is_multiline = .true.
            nesting_level = 1
        case ("forall")
            if (.not. is_inline_forall_statement(tokens, stmt_start)) then
                is_multiline = .true.
                nesting_level = 1
            end if
        end select
    end subroutine detect_multiline_construct

    pure subroutine locate_multiline_end(tokens, stmt_start, stmt_end, &
                                         initial_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(out) :: stmt_end
        integer, intent(in) :: initial_level
        integer :: i, nesting_level
        logical :: found_end

        nesting_level = initial_level
        stmt_end = stmt_start
        do i = stmt_start, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                stmt_end = i - 1
                exit
            end if

            if (tokens(i)%kind == TK_KEYWORD) then
                call update_multiline_keyword_state(tokens, stmt_start, i, &
                                                    nesting_level, stmt_end, &
                                                    found_end)
                if (found_end) exit
            end if

            stmt_end = i
        end do
    end subroutine locate_multiline_end

    pure subroutine update_multiline_keyword_state(tokens, stmt_start, idx, &
                                                   nesting_level, stmt_end, &
                                                   found_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, idx
        integer, intent(inout) :: nesting_level
        integer, intent(inout) :: stmt_end
        logical, intent(out) :: found_end

        found_end = .false.
        select case (tokens(idx)%text)
        case ("if")
            call maybe_increment_if_nesting(tokens, stmt_start, idx, &
                                            nesting_level)
        case ("do")
            if (idx > stmt_start) nesting_level = nesting_level + 1
        case ("select")
            if (idx > stmt_start) then
                if (begins_select_construct(tokens, idx)) then
                    nesting_level = nesting_level + 1
                end if
            end if
        case ("submodule")
            if (tokens(stmt_start)%text == "submodule" .and. idx > stmt_start) &
                then
                nesting_level = nesting_level + 1
            end if
        case ("type")
            if (tokens(stmt_start)%text == "type" .and. idx > stmt_start) then
                nesting_level = nesting_level + 1
            end if
        case ("forall")
            if (idx > stmt_start) then
                if (.not. is_inline_forall_statement(tokens, idx)) then
                    nesting_level = nesting_level + 1
                end if
            end if
        case ("endif")
            call try_close_construct(tokens, stmt_start, "if", idx, &
                                     nesting_level, stmt_end, found_end, &
                                     .false.)
        case ("end")
            call handle_end_keyword(tokens, stmt_start, idx, nesting_level, &
                                    stmt_end, found_end)
        case ("enddo")
            call try_close_construct(tokens, stmt_start, "do", idx, &
                                     nesting_level, stmt_end, found_end, &
                                     .false.)
        case ("endsubmodule")
            call try_close_construct(tokens, stmt_start, "submodule", idx, &
                                     nesting_level, stmt_end, found_end, &
                                     .true.)
        case ("endforall")
            call try_close_construct(tokens, stmt_start, "forall", idx, &
                                     nesting_level, stmt_end, found_end, &
                                     .false.)
        end select
    end subroutine update_multiline_keyword_state

    pure subroutine maybe_increment_if_nesting(tokens, stmt_start, idx, &
                                               nesting_level)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, idx
        integer, intent(inout) :: nesting_level
        integer :: j, limit, prev_kw
        logical :: is_elseif

        if (idx <= stmt_start) return

        is_elseif = .false.
        prev_kw = idx - 1
        do while (prev_kw >= stmt_start)
            if (tokens(prev_kw)%kind == TK_KEYWORD) then
                if (tokens(prev_kw)%text == "else") then
                    is_elseif = .true.
                end if
                exit
            else
                select case (tokens(prev_kw)%kind)
                case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                    prev_kw = prev_kw - 1
                case default
                    exit
                end select
            end if
        end do

        if (is_elseif) return

        limit = min(idx + 20, size(tokens))
        do j = idx + 1, limit
            if (tokens(j)%kind == TK_KEYWORD) then
                if (tokens(j)%text == "then") then
                    nesting_level = nesting_level + 1
                    return
                end if
            else if (tokens(j)%kind == TK_NEWLINE) then
                return
            end if
        end do
    end subroutine maybe_increment_if_nesting

    pure subroutine handle_end_keyword(tokens, stmt_start, idx, nesting_level, &
                                       stmt_end, found_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, idx
        integer, intent(inout) :: nesting_level
        integer, intent(inout) :: stmt_end
        logical, intent(out) :: found_end

        found_end = .false.
        if (idx + 1 <= size(tokens)) then
            if (tokens(idx + 1)%kind == TK_KEYWORD) then
                if (tokens(idx + 1)%text == "if") then
                    call try_close_construct(tokens, stmt_start, "if", idx + 1, &
                                             nesting_level, stmt_end, found_end, &
                                             .false.)
                    if (found_end) return
                end if
            end if
        end if

        call handle_two_word_end_constructs(tokens, stmt_start, idx, &
                                            nesting_level, stmt_end, found_end)
    end subroutine handle_end_keyword

    pure subroutine handle_two_word_end_constructs(tokens, stmt_start, idx, &
                                                   nesting_level, stmt_end, &
                                                   found_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, idx
        integer, intent(inout) :: nesting_level
        integer, intent(inout) :: stmt_end
        logical, intent(out) :: found_end
        integer :: kw_pos

        found_end = .false.

        kw_pos = idx + 1
        do while (kw_pos <= size(tokens) .and. &
                  (tokens(kw_pos)%kind == TK_WHITESPACE .or. &
                   tokens(kw_pos)%kind == TK_COMMENT))
            kw_pos = kw_pos + 1
        end do
        if (kw_pos <= size(tokens)) then
            if (tokens(kw_pos)%kind == TK_KEYWORD) then
                select case (tokens(kw_pos)%text)
                case ("do")
                    call try_close_construct(tokens, stmt_start, "do", kw_pos, &
                                             nesting_level, stmt_end, &
                                             found_end, .false.)
                    if (found_end) return
                case ("forall")
                    call try_close_construct(tokens, stmt_start, "forall", kw_pos, &
                                             nesting_level, stmt_end, &
                                             found_end, .false.)
                    if (found_end) return
                end select
            end if
        end if

        if (idx + 1 <= size(tokens)) then
            if (tokens(idx + 1)%kind == TK_KEYWORD) then
                select case (tokens(idx + 1)%text)
                case ("do")
                    call try_close_construct(tokens, stmt_start, "do", idx + 1, &
                                             nesting_level, stmt_end, &
                                             found_end, .false.)
                case ("select")
                    call try_close_construct(tokens, stmt_start, "select", &
                                             idx + 1, nesting_level, stmt_end, &
                                             found_end, .false.)
                case ("where")
                    call try_close_construct(tokens, stmt_start, "where", &
                                             idx + 1, nesting_level, stmt_end, &
                                             found_end, .false.)
                case ("submodule")
                    call try_close_construct(tokens, stmt_start, "submodule", &
                                             idx + 1, nesting_level, stmt_end, &
                                             found_end, .true.)
                case ("type")
                    call try_close_construct(tokens, stmt_start, "type", &
                                             idx + 1, nesting_level, stmt_end, &
                                             found_end, .true.)
                end select
                if (found_end) return
            end if
        end if
    end subroutine handle_two_word_end_constructs

    pure subroutine try_close_construct(tokens, stmt_start, construct_name, &
                                        closing_idx, nesting_level, stmt_end, &
                                        found_end, include_identifier)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, closing_idx
        integer, intent(inout) :: nesting_level
        integer, intent(inout) :: stmt_end
        character(len=*), intent(in) :: construct_name
        logical, intent(out) :: found_end
        logical, intent(in) :: include_identifier

        found_end = .false.

        ! Always decrement nesting for closing constructs
        nesting_level = nesting_level - 1

        ! Only mark as found if this matches our starting construct AND nesting is 0
        if (tokens(stmt_start)%text /= construct_name) return
        if (nesting_level /= 0) return

        stmt_end = closing_idx
        if (include_identifier) call extend_with_identifier(tokens, stmt_end)
        found_end = .true.
    end subroutine try_close_construct

    pure subroutine extend_with_identifier(tokens, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: stmt_end

        if (stmt_end + 1 <= size(tokens)) then
            if (tokens(stmt_end + 1)%kind == TK_IDENTIFIER) then
                stmt_end = stmt_end + 1
            end if
        end if
    end subroutine extend_with_identifier

    pure subroutine locate_single_line_end(tokens, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(out) :: stmt_end
        integer :: i, bracket_depth, paren_depth

        bracket_depth = 0
        paren_depth = 0
        stmt_end = stmt_start
        do i = stmt_start, size(tokens)
            select case (tokens(i)%kind)
            case (TK_EOF)
                stmt_end = i - 1
                exit
            case (TK_OPERATOR)
                select case (tokens(i)%text)
                case ("[")
                    bracket_depth = bracket_depth + 1
                case ("]")
                    bracket_depth = bracket_depth - 1
                case ("(")
                    paren_depth = paren_depth + 1
                case (")")
                    paren_depth = paren_depth - 1
                case (";")
                    if (bracket_depth == 0 .and. paren_depth == 0) then
                        stmt_end = i - 1
                        exit
                    end if
                end select
            case (TK_NEWLINE)
                if (bracket_depth == 0 .and. paren_depth == 0) then
                    stmt_end = i - 1
                    exit
                end if
            case (TK_COMMENT)
                cycle
            case default
                stmt_end = i
            end select
        end do
    end subroutine locate_single_line_end

    ! Find statement boundary (control-flow aware)
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        integer :: nesting_level
        logical :: is_multiline

        stmt_start = find_statement_start(tokens, start_pos)
        if (stmt_start > size(tokens)) then
            stmt_end = size(tokens)
            return
        end if

        call detect_multiline_construct(tokens, stmt_start, is_multiline, &
                                        nesting_level)
        if (is_multiline) then
            call locate_multiline_end(tokens, stmt_start, stmt_end, &
                                      nesting_level)
        else
            call locate_single_line_end(tokens, stmt_start, stmt_end)
        end if

        if (stmt_end > size(tokens)) stmt_end = size(tokens)
        if (stmt_end < stmt_start) stmt_end = stmt_start
    end subroutine find_statement_boundary

    pure logical function begins_select_construct(tokens, select_index) &
        result(is_select_construct)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: select_index
        integer :: idx, max_idx
        character(len=:), allocatable :: lowered

        is_select_construct = .false.
        if (select_index < 1 .or. select_index > size(tokens)) return

        idx = select_index + 1
        max_idx = min(size(tokens), select_index + 100)
        do while (idx <= max_idx)
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                idx = idx + 1
            case (TK_OPERATOR)
                if (tokens(idx)%text == "&") then
                    idx = idx + 1
                else
                    return
                end if
            case (TK_KEYWORD)
                lowered = to_lower(tokens(idx)%text)
                if (lowered == "case" .or. lowered == "type" .or. &
                    lowered == "rank") then
                    is_select_construct = .true.
                end if
                return
            case default
                return
            end select
        end do
    end function begins_select_construct

end module frontend_statement_boundary
