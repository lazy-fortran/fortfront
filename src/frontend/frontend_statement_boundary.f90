module frontend_statement_boundary
    ! Statement boundary detection and control-flow awareness
    ! Handles finding statement boundaries and inline construct detection

    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_NEWLINE, &
                          TK_OPERATOR, TK_WHITESPACE, TK_COMMENT

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

        if (tokens(first_token)%kind == TK_OPERATOR .and. &
            tokens(first_token)%text == "(") then
            is_inline = inline_where_parenthetical(tokens, first_token + 1)
        else
            is_inline = inline_where_colon_variant(tokens, first_token)
        end if
    end function is_inline_where_statement

    ! Find statement boundary (control-flow aware)
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end

        integer :: i, nesting_level
        logical :: is_multiline_construct

        stmt_start = start_pos
        stmt_end = start_pos
        is_multiline_construct = .false.
        nesting_level = 0

        ! Skip leading newlines and semicolons (semicolons act as statement separators)
        stmt_start = start_pos
        do while (stmt_start <= size(tokens) .and. &
                  (tokens(stmt_start)%kind == TK_NEWLINE .or. &
                   (tokens(stmt_start)%kind == TK_OPERATOR .and. &
                    tokens(stmt_start)%text == ";")))
            stmt_start = stmt_start + 1
        end do

        if (stmt_start > size(tokens)) then
            stmt_end = size(tokens)
            return
        end if

        ! Check if this starts a multi-line control flow construct
        if (tokens(stmt_start)%kind == TK_KEYWORD) then
            select case (tokens(stmt_start)%text)
            case ("if")
                ! Check if it's if/then (multi-line) by looking ahead
                do i = stmt_start + 1, min(stmt_start + 20, size(tokens))
                    if (tokens(i)%kind == TK_KEYWORD .and. tokens(i)%text == &
                        "then") then
                        is_multiline_construct = .true.
                        nesting_level = 1
                        exit
                    else if (tokens(i)%kind == TK_NEWLINE) then
                        exit  ! Single-line if
                    end if
                end do
            case ("do")
                is_multiline_construct = .true.
                nesting_level = 1
            case ("select")
                is_multiline_construct = .true.
                nesting_level = 1
            case ("where")
                if (.not. is_inline_where_statement(tokens, stmt_start)) then
                    do i = stmt_start + 1, min(stmt_start + 20, size(tokens))
                        if (tokens(i)%kind == TK_NEWLINE) then
                            ! Multi-line where construct without inline body
                            is_multiline_construct = .true.
                            nesting_level = 1
                            exit
                        else if (tokens(i)%kind == TK_KEYWORD .and. &
                                 (tokens(i)%text == "end" .or. &
                                  tokens(i)%text == "elsewhere")) then
                            is_multiline_construct = .true.
                            nesting_level = 1
                            exit
                        end if
                    end do
                end if
            end select
        end if

        if (is_multiline_construct) then
            ! Find the matching end construct
            stmt_end = stmt_start
            do i = stmt_start, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    stmt_end = i - 1
                    exit
                end if

                if (tokens(i)%kind == TK_KEYWORD) then
                    select case (tokens(i)%text)
                        ! Handle nested constructs
                    case ("if")
                        if (i > stmt_start) then
                            ! Check if it's if/then (nested)
                            block
                                integer :: j
                                do j = i + 1, min(i + 20, size(tokens))
                                    if (tokens(j)%kind == TK_KEYWORD .and. &
                                        tokens(j)%text == &
                                        "then") then
                                        nesting_level = nesting_level + 1
                                        exit
                                    else if (tokens(j)%kind == TK_NEWLINE) then
                                        exit
                                    end if
                                end do
                            end block
                        end if
                    case ("do")
                        if (i > stmt_start) then
                            nesting_level = nesting_level + 1
                        end if
                    case ("select")
                        if (i > stmt_start) then
                            nesting_level = nesting_level + 1
                        end if

                        ! Handle end constructs
                    case ("endif", "end")
                        if (tokens(stmt_start)%text == "if") then
                            if (tokens(i)%text == "endif") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i
                                    exit
                                end if
                            else if (tokens(i)%text == "end") then
                                if (i + 1 <= size(tokens)) then
                                    if (tokens(i + 1)%kind == TK_KEYWORD .and. &
                                        tokens(i + 1)%text == "if") then
                                        nesting_level = nesting_level - 1
                                        if (nesting_level == 0) then
                                            stmt_end = i + 1
                                            exit
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    case ("enddo")
                        if (tokens(stmt_start)%text == "do") then
                            nesting_level = nesting_level - 1
                            if (nesting_level == 0) then
                                stmt_end = i
                                exit
                            end if
                        end if
                    end select

                    ! Check for two-word end constructs
                    if (tokens(i)%text == "end") then
                        if (i + 1 <= size(tokens) .and. tokens(i + 1)%kind == &
                            TK_KEYWORD) then
                            if (tokens(i + 1)%text == "do" .and. &
                                tokens(stmt_start)%text &
                                == "do") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            else if (tokens(i + 1)%text == "select" .and. &
                                     tokens(stmt_start)%text == &
                                     "select") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            else if (tokens(i + 1)%text == "where" .and. &
                                     tokens(stmt_start)%text == &
                                     "where") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    stmt_end = i + 1
                                    exit
                                end if
                            end if
                        end if
                    end if
                end if

                stmt_end = i
            end do
        else
            ! Single-line statement - find end at newline or semicolon
            do i = stmt_start, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    stmt_end = i - 1
                    exit
                else if (tokens(i)%kind == TK_NEWLINE .or. &
                         (tokens(i)%kind == TK_OPERATOR .and. tokens(i)%text &
                          == ";")) then
                    stmt_end = i - 1
                    exit
                else if (tokens(i)%kind /= TK_COMMENT) then
                    stmt_end = i
                end if
            end do
        end if

        ! Ensure we don't go beyond bounds
        if (stmt_end > size(tokens)) stmt_end = size(tokens)
        if (stmt_end < stmt_start) stmt_end = stmt_start
    end subroutine find_statement_boundary

end module frontend_statement_boundary
