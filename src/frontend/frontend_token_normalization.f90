module frontend_token_normalization
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_COMMENT, &
        TK_WHITESPACE, TK_NEWLINE, TK_OPERATOR, TK_EOF, &
        TK_NUMBER, to_lower
    use frontend_program_unit_scanner, only: find_next_nontrivial_index, &
        token_precedes_identifier, &
        token_requires_identifier_after, &
        token_follows_identifier_context, &
        keyword_can_be_identifier, &
        token_is_block_keyword

    implicit none
    private

    public :: normalize_keyword_identifiers

contains

    subroutine normalize_keyword_identifiers(tokens)
        type(token_t), intent(inout) :: tokens(:)
        integer :: i
        type(token_t) :: prev_token
        type(token_t) :: next_token
        character(len=:), allocatable :: lowered

        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD) cycle

            lowered = to_lower(trim(tokens(i)%text))
            if (lowered == "end") then
                prev_token = find_previous_nontrivial_token(tokens, i)
                if (.not. token_precedes_identifier(prev_token)) cycle

                next_token = find_next_nontrivial_token(tokens, i)
                if (token_is_block_keyword(next_token)) cycle

                tokens(i)%kind = TK_IDENTIFIER
                cycle
            end if

            if (.not. keyword_can_be_identifier(lowered)) cycle
            if (lowered == "goto") then
                if (is_computed_goto_context(tokens, i)) cycle
            end if

            prev_token = find_previous_nontrivial_token(tokens, i)
            next_token = find_next_nontrivial_token(tokens, i)

            if (token_precedes_identifier(prev_token) .or. &
                token_requires_identifier_after(prev_token) .or. &
                token_follows_identifier_context(next_token)) then
                tokens(i)%kind = TK_IDENTIFIER
            end if
        end do
    end subroutine normalize_keyword_identifiers

    function find_previous_nontrivial_token(tokens, pos) result(prev_token)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        type(token_t) :: prev_token
        integer :: i

        prev_token%kind = TK_EOF
        prev_token%text = ""

        if (pos <= 1) return

        do i = pos - 1, 1, -1
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                prev_token = tokens(i)
                return
            end select
        end do
    end function find_previous_nontrivial_token

    function find_next_nontrivial_token(tokens, pos) result(next_token)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        type(token_t) :: next_token
        integer :: i

        next_token%kind = TK_EOF
        next_token%text = ""

        if (pos >= size(tokens)) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                next_token = tokens(i)
                return
            end select
        end do
    end function find_next_nontrivial_token

    logical function is_computed_goto_context(tokens, pos) result(is_computed)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: next_idx

        is_computed = .false.
        next_idx = find_next_nontrivial_index(tokens, pos)
        if (next_idx <= 0) return
        if (tokens(next_idx)%kind /= TK_OPERATOR) return
        if (trim(tokens(next_idx)%text) /= "(") return

        next_idx = find_next_nontrivial_index(tokens, next_idx)
        if (next_idx <= 0) return
        if (tokens(next_idx)%kind == TK_NUMBER) is_computed = .true.
    end function is_computed_goto_context

end module frontend_token_normalization
