module parser_block_statement_utils_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_OPERATOR, TK_KEYWORD, TK_IDENTIFIER, &
        TK_NEWLINE, TK_WHITESPACE, TK_COMMENT
    implicit none
    private

    public :: is_if_statement_start
    public :: is_block_construct_keyword
    public :: block_construct_start
    public :: locate_block_statement_end
    public :: locate_single_line_end

contains

    ! True when the token opens a multi-line construct whose token span has to
    ! be located by matching its terminator rather than by end of line.
    logical function is_block_construct_keyword(tok) result(is_construct)
        type(token_t), intent(in) :: tok
        character(len=:), allocatable :: token_lower

        is_construct = tok%kind == TK_KEYWORD
        if (.not. is_construct) return
        token_lower = trim(to_lower(tok%text))
        select case (token_lower)
        case ("if", "do", "select", "associate", "block")
            is_construct = .true.
        case default
            is_construct = .false.
        end select
    end function is_block_construct_keyword

    ! Retained name for the historical caller: a statement starts a block
    ! construct when its first token is one of the construct keywords.
    logical function is_if_statement_start(first_token) result(is_if_start)
        type(token_t), intent(in) :: first_token

        is_if_start = is_block_construct_keyword(first_token)
    end function is_if_statement_start

    ! Index of the token that opens the construct of the statement beginning at
    ! stmt_start, skipping a leading construct name ("check: if (...) then").
    ! Returns 0 when the statement does not open a block construct.
    integer function block_construct_start(all_tokens, stmt_start) result(kw_pos)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        integer :: colon_pos, candidate

        kw_pos = 0
        if (stmt_start < 1 .or. stmt_start > size(all_tokens)) return

        if (is_block_construct_keyword(all_tokens(stmt_start))) then
            ! Unless it is being assigned to. `block = registry%blocks(i)`
            ! names a variable, and treating it as a construct sends the
            ! parser looking for an `end block` that is not there. A construct
            ! keyword never has an assignment operator next.
            candidate = next_significant(all_tokens, stmt_start + 1)
            if (candidate <= size(all_tokens)) then
                if (all_tokens(candidate)%kind == TK_OPERATOR) then
                    if (trim(all_tokens(candidate)%text) == "=") return
                end if
            end if
            kw_pos = stmt_start
            return
        end if

        if (all_tokens(stmt_start)%kind /= TK_IDENTIFIER) return
        colon_pos = next_significant(all_tokens, stmt_start + 1)
        if (colon_pos > size(all_tokens)) return
        if (all_tokens(colon_pos)%kind /= TK_OPERATOR) return
        if (trim(all_tokens(colon_pos)%text) /= ":") return
        candidate = next_significant(all_tokens, colon_pos + 1)
        if (candidate > size(all_tokens)) return
        if (.not. is_block_construct_keyword(all_tokens(candidate))) return
        kw_pos = candidate
    end function block_construct_start

    integer function next_significant(all_tokens, start_pos) result(pos)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: start_pos

        pos = start_pos
        do while (pos <= size(all_tokens))
            select case (all_tokens(pos)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                pos = pos + 1
            case default
                return
            end select
        end do
    end function next_significant

    ! Index of the last token of the construct opened at stmt_start.
    !
    ! One scan serves every construct: the terminator of the construct named by
    ! stmt_type closes a nesting level, any other occurrence of the construct
    ! keyword opens one. Keyword text is compared case-insensitively, and the
    ! second token of a two-word terminator ("end do") is skipped so that it
    ! cannot be miscounted as a fresh opening (issue #2972).
    integer function locate_block_statement_end(all_tokens, stmt_start, &
            stmt_type, unaccounted) result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        character(len=*), intent(in) :: stmt_type
        ! Token-accounting invariant. .true. when the scan consumed every
        ! remaining token of the unit without matching the terminator, so the
        ! span it returns covers source that does not belong to the construct.
        !
        ! This is a check on the SCANNER, not on the source: the pre-parse
        ! construct-terminator validator has already established that the
        ! source closes every construct it opens. So if this fires, the
        ! bookkeeping here is wrong, and the caller is about to absorb
        ! unrelated statements into this construct and drop them from the AST.
        ! That is the mechanism of #2928, the bare-END procedure span, and
        ! #2966/#2967/#2972/#2974/#2977 (issue #2983).
        logical, intent(out), optional :: unaccounted

        integer :: pos
        integer :: depth
        integer :: closer_end
        character(len=:), allocatable :: construct
        character(len=:), allocatable :: token_lower

        construct = trim(to_lower(stmt_type))
        stmt_end = stmt_start
        if (present(unaccounted)) unaccounted = .false.

        if (construct == "if") then
            if (is_single_line_if_statement(all_tokens, stmt_start)) then
                stmt_end = locate_single_line_end(all_tokens, stmt_start, &
                    all_tokens(stmt_start)%line)
                return
            end if
        end if

        depth = 1
        pos = stmt_start + 1

        do while (pos <= size(all_tokens))
            if (all_tokens(pos)%kind == TK_KEYWORD) then
                token_lower = trim(to_lower(all_tokens(pos)%text))
                closer_end = construct_terminator_end(all_tokens, pos, construct)
                if (closer_end > 0) then
                    depth = depth - 1
                    stmt_end = closer_end
                    if (depth <= 0) return
                    pos = closer_end + 1
                    cycle
                else if (token_lower == construct) then
                    if (construct == "if" .and. &
                        is_single_line_if_statement(all_tokens, pos)) then
                        stmt_end = locate_single_line_end(all_tokens, pos, &
                            all_tokens(pos)%line)
                        pos = stmt_end + 1
                        cycle
                    end if
                    if (.not. opens_nested_construct(all_tokens, pos, construct)) &
                        then
                        stmt_end = pos
                        pos = pos + 1
                        cycle
                    end if
                    depth = depth + 1
                end if
            end if
            stmt_end = pos
            pos = pos + 1
        end do

        ! Ran out of tokens with the construct still open.
        if (present(unaccounted)) unaccounted = .true.
    end function locate_block_statement_end

    ! Index of the last token of the terminator of `construct` starting at pos,
    ! or 0 when the token at pos does not start one. Handles both the one-word
    ! ("enddo") and two-word ("end do") spellings, case-insensitively.
    integer function construct_terminator_end(all_tokens, pos, construct) &
            result(closer_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        character(len=*), intent(in) :: construct
        character(len=:), allocatable :: token_lower

        closer_end = 0
        token_lower = trim(to_lower(all_tokens(pos)%text))

        if (token_lower == "end"//construct) then
            closer_end = pos
            return
        end if

        if (token_lower /= "end") return
        if (pos + 1 > size(all_tokens)) return
        if (all_tokens(pos + 1)%kind /= TK_KEYWORD) return
        if (trim(to_lower(all_tokens(pos + 1)%text)) /= construct) return
        closer_end = pos + 1
    end function construct_terminator_end

    ! An occurrence of the construct keyword that does not actually open a
    ! nested construct: "else if" continues the current IF construct.
    logical function opens_nested_construct(all_tokens, pos, construct) &
            result(opens)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        character(len=*), intent(in) :: construct

        opens = .true.
        if (construct /= "if") return
        if (pos <= 1) return
        if (all_tokens(pos - 1)%kind /= TK_KEYWORD) return
        if (trim(to_lower(all_tokens(pos - 1)%text)) == "else") opens = .false.
    end function opens_nested_construct

    logical function is_single_line_if_statement(all_tokens, stmt_start) &
            result(is_single_line)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start

        integer :: pos
        integer :: paren_depth
        logical :: pending_continuation
        character(len=:), allocatable :: token_text

        is_single_line = .true.
        paren_depth = 0
        pending_continuation = .false.

        do pos = stmt_start + 1, size(all_tokens)
            select case (all_tokens(pos)%kind)
            case (TK_OPERATOR)
                token_text = all_tokens(pos)%text
                if (token_text == "(") then
                    paren_depth = paren_depth + 1
                else if (token_text == ")") then
                    paren_depth = max(0, paren_depth - 1)
                else if (paren_depth == 0) then
                    if (token_text == "&") then
                        pending_continuation = .true.
                    else
                        pending_continuation = .false.
                    end if
                end if
            case (TK_KEYWORD)
                if (paren_depth == 0) then
                    if (trim(to_lower(all_tokens(pos)%text)) == "then") then
                        is_single_line = .false.
                        return
                    end if
                end if
            case (TK_NEWLINE, TK_COMMENT)
                if (paren_depth == 0 .and. .not. pending_continuation) then
                    return
                end if
            case default
            end select
        end do
    end function is_single_line_if_statement

    integer function locate_single_line_end(all_tokens, stmt_start, stmt_line) &
            result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(in) :: stmt_line

        integer :: pos

        stmt_end = stmt_start
        do pos = stmt_start, size(all_tokens)
            if (all_tokens(pos)%line /= stmt_line) then
                stmt_end = pos - 1
                return
            end if
        end do
        stmt_end = size(all_tokens)
    end function locate_single_line_end

end module parser_block_statement_utils_module
