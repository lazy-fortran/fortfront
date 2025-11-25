module parser_expression_tokens_module
    use lexer_core, only: token_t, TK_OPERATOR, to_lower
    use parser_state_module, only: parser_state_t
    use parser_token_views_module, only: token_view_t, view_peek_token, &
                                         view_lookahead_token
    implicit none
    private

    public :: token_matches
    public :: token_is_boolean_literal
    public :: is_prefix_operator_token
    public :: is_immediate_prefix_token
    public :: is_not_operator_token
    public :: token_is_terminator
    public :: is_legacy_array_literal_start

contains

    logical function token_matches(token, text)
        type(token_t), intent(in) :: token
        character(len=*), intent(in) :: text
        token_matches = trim(token%text) == trim(text)
    end function token_matches

    logical function token_is_boolean_literal(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        lowered = to_lower(token%text)
        token_is_boolean_literal = (lowered == ".true." .or. lowered == &
                                    ".false." .or. &
                                    lowered == "true" .or. lowered == "false")
    end function token_is_boolean_literal

    logical function is_prefix_operator_token(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_OPERATOR) then
            is_prefix_operator_token = .false.
            return
        end if

        lowered = to_lower(token%text)
        is_prefix_operator_token = (lowered == "+" .or. lowered == "-" .or. lowered &
                                    == ".not.")
    end function is_prefix_operator_token

    ! High-precedence prefix operators that bind immediately to their operand
    ! (unary + and -). These should NOT include .not. which has lower precedence.
    logical function is_immediate_prefix_token(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_OPERATOR) then
            is_immediate_prefix_token = .false.
            return
        end if

        lowered = to_lower(token%text)
        is_immediate_prefix_token = (lowered == "+" .or. lowered == "-")
    end function is_immediate_prefix_token

    ! Check if token is .not. operator
    ! ISO/IEC 1539-1:2018 Table 10.1: .not. has lower precedence than comparison
    logical function is_not_operator_token(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_OPERATOR) then
            is_not_operator_token = .false.
            return
        end if

        lowered = to_lower(token%text)
        is_not_operator_token = (lowered == ".not.")
    end function is_not_operator_token

    logical function token_is_terminator(token, terminators)
        type(token_t), intent(in) :: token
        character(len=*), intent(in), optional :: terminators(:)
        integer :: idx

        if (.not. present(terminators)) then
            token_is_terminator = .false.
            return
        end if

        token_is_terminator = .false.
        do idx = 1, size(terminators)
            if (trim(token%text) == trim(terminators(idx))) then
                token_is_terminator = .true.
                return
            end if
        end do
    end function token_is_terminator

    logical function is_legacy_array_literal_start(parser, view)
        type(parser_state_t), intent(in) :: parser
        type(token_view_t), intent(in) :: view
        type(token_t) :: current
        type(token_t) :: next_token

        is_legacy_array_literal_start = .false.
        current = view_peek_token(view, parser)
        if (current%kind /= TK_OPERATOR) return
        if (trim(current%text) /= "(") return

        next_token = view_lookahead_token(view, parser, 1)
        if (next_token%kind == TK_OPERATOR .and. trim(next_token%text) == "/") then
            is_legacy_array_literal_start = .true.
        end if
    end function is_legacy_array_literal_start

end module parser_expression_tokens_module
