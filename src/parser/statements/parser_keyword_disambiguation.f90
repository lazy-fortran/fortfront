module parser_keyword_disambiguation_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_COMMENT, TK_WHITESPACE, &
                          TK_NEWLINE, TK_EOF, to_lower
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: keyword_should_parse_as_identifier
    public :: looks_like_format_statement

contains

    logical function keyword_should_parse_as_identifier(first_token, parser) &
        result(as_identifier)
        type(token_t), intent(in) :: first_token
        type(parser_state_t), intent(in) :: parser
        character(len=:), allocatable :: lowered

        as_identifier = .false.
        if (.not. allocated(first_token%text)) return

        lowered = to_lower(trim(first_token%text))
        select case (lowered)
        case ("format")
            as_identifier = .not. looks_like_format_statement(parser)
        end select
    end function keyword_should_parse_as_identifier

    logical function looks_like_format_statement(parser) result(is_format)
        type(parser_state_t), intent(in) :: parser
        integer :: idx, depth, token_count
        type(token_t) :: tok
        logical :: continuation_allowed
        logical :: trailing_continuation

        is_format = .false.
        if (.not. associated(parser%tokens)) return

        token_count = size(parser%tokens)
        if (token_count == 0) return

        idx = parser%current_token + 1
        continuation_allowed = .false.

        ! Look for the required opening parenthesis after FORMAT
        do while (idx <= token_count)
            tok = parser%tokens(idx)
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NEWLINE)
                if (.not. continuation_allowed) return
                continuation_allowed = .false.
                idx = idx + 1
                cycle
            case (TK_OPERATOR)
                select case (tok%text)
                case ("&")
                    continuation_allowed = .true.
                    idx = idx + 1
                    cycle
                case ("(")
                    exit
                case default
                    return
                end select
            case default
                return
            end select
        end do

        if (idx > token_count) return

        depth = 1
        idx = idx + 1
        continuation_allowed = .false.

        ! Walk the parenthesized format specifier
        do while (idx <= token_count .and. depth > 0)
            tok = parser%tokens(idx)
            select case (tok%kind)
            case (TK_OPERATOR)
                select case (tok%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                case ("&")
                    continuation_allowed = .true.
                end select
            case (TK_NEWLINE)
                if (.not. continuation_allowed) return
                continuation_allowed = .false.
            end select
            idx = idx + 1
        end do

        if (depth /= 0) return

        trailing_continuation = .false.
        do while (idx <= token_count)
            tok = parser%tokens(idx)
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NEWLINE)
                if (trailing_continuation) then
                    trailing_continuation = .false.
                    idx = idx + 1
                    cycle
                end if
                is_format = .true.
                return
            case (TK_EOF)
                is_format = .true.
                return
            case (TK_OPERATOR)
                select case (tok%text)
                case (";")
                    is_format = .true.
                    return
                case ("&")
                    trailing_continuation = .true.
                    idx = idx + 1
                    cycle
                case default
                    return
                end select
            case default
                return
            end select
        end do

        is_format = .true.
    end function looks_like_format_statement

end module parser_keyword_disambiguation_module
