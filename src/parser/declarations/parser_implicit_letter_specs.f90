module parser_implicit_letter_specs_module
    ! Validation of the letter-spec-list that terminates every implicit-spec.
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, TK_EOF
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: validate_implicit_letter_specs

contains

    ! F2018 R863/R865: every implicit-spec ends in a parenthesised
    ! letter-spec-list, and a letter-spec is a single letter or a range of two
    ! single letters. Only the group that closes an implicit-spec is checked, so
    ! a preceding kind or derived-type selector such as real(kind=8) or class(t)
    ! is left alone.
    subroutine validate_implicit_letter_specs(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        integer :: pos, depth, group_start, last_index

        last_index = statement_end_index(parser)
        pos = parser%current_token
        depth = 0
        group_start = 0

        do while (pos <= last_index)
            token = parser%get_token_at_index(pos)
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                if (depth == 0) group_start = pos
                depth = depth + 1
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                depth = depth - 1
                if (depth == 0 .and. group_start > 0) then
                    if (closes_implicit_spec(parser, pos, last_index)) then
                        if (.not. is_letter_spec_list(parser, group_start + 1, &
                            pos - 1)) then
                            call parser%error_at_token( &
                                "Syntax error in IMPLICIT statement: the "// &
                                "letter-spec list must hold single letters or "// &
                                "letter ranges", &
                                parser%get_token_at_index(group_start))
                            return
                        end if
                    end if
                    group_start = 0
                end if
            end if
            pos = pos + 1
        end do
    end subroutine validate_implicit_letter_specs

    ! Index of the last token belonging to the current statement.
    integer function statement_end_index(parser) result(last_index)
        type(parser_state_t), intent(in) :: parser
        type(token_t) :: token
        integer :: pos

        last_index = parser%current_token - 1
        pos = parser%current_token
        do while (pos <= parser%get_token_count())
            token = parser%get_token_at_index(pos)
            if (token%kind == TK_NEWLINE .or. token%kind == TK_COMMENT .or. &
                token%kind == TK_EOF) exit
            last_index = pos
            pos = pos + 1
        end do
    end function statement_end_index

    ! A parenthesised group closes an implicit-spec when the statement ends
    ! right after it or the next token is the comma before the next spec.
    logical function closes_implicit_spec(parser, close_pos, last_index) &
            result(closes)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: close_pos
        integer, intent(in) :: last_index
        type(token_t) :: token
        integer :: pos

        closes = .false.
        pos = skip_blanks(parser, close_pos + 1, last_index)
        if (pos > last_index) then
            closes = .true.
            return
        end if

        token = parser%get_token_at_index(pos)
        if (token%kind /= TK_OPERATOR) return
        closes = (token%text == ",")
    end function closes_implicit_spec

    logical function is_letter_spec_list(parser, first, last) result(valid)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: first
        integer, intent(in) :: last
        type(token_t) :: token
        integer :: pos

        valid = .false.
        if (last < first) return

        pos = skip_blanks(parser, first, last)
        do
            if (pos > last) return
            if (.not. is_single_letter(parser%get_token_at_index(pos))) return
            pos = skip_blanks(parser, pos + 1, last)
            if (pos > last) exit

            token = parser%get_token_at_index(pos)
            if (token%kind == TK_OPERATOR .and. token%text == "-") then
                pos = skip_blanks(parser, pos + 1, last)
                if (pos > last) return
                if (.not. is_single_letter(parser%get_token_at_index(pos))) return
                pos = skip_blanks(parser, pos + 1, last)
                if (pos > last) exit
                token = parser%get_token_at_index(pos)
            end if

            if (token%kind /= TK_OPERATOR) return
            if (token%text /= ",") return
            pos = skip_blanks(parser, pos + 1, last)
        end do

        valid = .true.
    end function is_letter_spec_list

    integer function skip_blanks(parser, first, last) result(pos)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: first
        integer, intent(in) :: last
        type(token_t) :: token

        pos = first
        do while (pos <= last)
            token = parser%get_token_at_index(pos)
            if (token%kind /= TK_WHITESPACE) exit
            pos = pos + 1
        end do
    end function skip_blanks

    logical function is_single_letter(token) result(is_letter)
        type(token_t), intent(in) :: token
        character(len=1) :: c

        is_letter = .false.
        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return
        if (.not. allocated(token%text)) return
        if (len(token%text) /= 1) return
        c = token%text(1:1)
        is_letter = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z")
    end function is_single_letter

end module parser_implicit_letter_specs_module
