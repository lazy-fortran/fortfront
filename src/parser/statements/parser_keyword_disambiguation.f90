module parser_keyword_disambiguation_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_COMMENT, TK_WHITESPACE, &
                          TK_NEWLINE, TK_EOF, TK_KEYWORD, TK_IDENTIFIER, &
                          TK_NUMBER, TK_STRING, to_lower
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: keyword_should_parse_as_identifier
    public :: looks_like_format_statement
    public :: looks_like_implicit_statement

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
        case ("implicit")
            as_identifier = .not. looks_like_implicit_statement(parser)
        case ("if")
            as_identifier = should_parse_if_as_identifier(parser)
        case default
            if (keyword_supports_assignment_disambiguation(lowered)) then
                as_identifier = statement_contains_assignment(parser)
            end if
        end select
    end function keyword_should_parse_as_identifier

    logical function keyword_supports_assignment_disambiguation(keyword) &
        result(is_supported)
        character(len=*), intent(in) :: keyword

        select case (keyword)
        case ("call", "stop", "cycle", "exit", "return", &
              "continue", "goto", "go", "entry", "select", &
              "contains", "else", "dimension", "common", &
              "program", "module", "if")
            is_supported = .true.
        case default
            is_supported = .false.
        end select
    end function keyword_supports_assignment_disambiguation

    ! Decide if leading IF token is an identifier rather than a control keyword.
    ! Returns true only when no opening parenthesis appears before assignment syntax.
    logical function should_parse_if_as_identifier(parser) result(as_identifier)
        type(parser_state_t), intent(in) :: parser
        integer :: idx, token_count, depth
        type(token_t) :: tok
        logical :: continuation, expect_component

        as_identifier = .false.
        if (.not. associated(parser%tokens)) return

        token_count = size(parser%tokens)
        if (token_count < parser%current_token) return

        idx = parser%current_token + 1
        continuation = .false.
        depth = 0
        expect_component = .false.

        do while (idx <= token_count)
            tok = parser%tokens(idx)
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NEWLINE)
                if (.not. continuation) exit
                continuation = .false.
                idx = idx + 1
                cycle
            case (TK_OPERATOR)
                select case (tok%text)
                case ("&")
                    continuation = .true.
                case ("(")
                    if (expect_component) return
                    depth = depth + 1
                case (")")
                    if (depth == 0) return
                    depth = depth - 1
                case ("=", "=>")
                    if (depth == 0 .and. .not. expect_component) then
                        as_identifier = .true.
                        return
                    else
                        return
                    end if
                case ("%")
                    if (depth == 0 .and. .not. expect_component) then
                        expect_component = .true.
                    else
                        return
                    end if
                case (":", ",")
                    if (depth == 0) return
                case (";")
                    exit
                case default
                    if (depth == 0) return
                end select
            case (TK_IDENTIFIER)
                if (expect_component) then
                    expect_component = .false.
                else if (depth == 0) then
                    return
                end if
            case (TK_NUMBER, TK_STRING)
                if (depth == 0) return
            case (TK_KEYWORD)
                if (depth == 0) return
            case (TK_EOF)
                exit
            end select
            idx = idx + 1
        end do

        as_identifier = .false.
    end function should_parse_if_as_identifier

    logical function statement_contains_assignment(parser) result(has_assignment)
        type(parser_state_t), intent(in) :: parser
        integer :: idx, depth, token_count
        type(token_t) :: tok
        logical :: continuation

        has_assignment = .false.
        if (.not. associated(parser%tokens)) return

        token_count = size(parser%tokens)
        if (token_count < parser%current_token) return

        idx = parser%current_token + 1
        depth = 0
        continuation = .false.

        do while (idx <= token_count)
            tok = parser%tokens(idx)
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NEWLINE)
                if (.not. continuation) return
                continuation = .false.
                idx = idx + 1
                cycle
            case (TK_OPERATOR)
                select case (tok%text)
                case ("&")
                    continuation = .true.
                    idx = idx + 1
                    cycle
                case ("(")
                    depth = depth + 1
                case (")")
                    if (depth > 0) depth = depth - 1
                case ("=", "=>")
                    if (depth == 0) then
                        has_assignment = .true.
                        return
                    end if
                case (";")
                    return
                end select
            case (TK_EOF)
                return
            case default
                continuation = .false.
            end select
            idx = idx + 1
        end do
    end function statement_contains_assignment

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

    logical function looks_like_implicit_statement(parser) result(is_implicit)
        type(parser_state_t), intent(in) :: parser
        integer :: idx, token_count
        type(token_t) :: tok
        logical :: continuation_allowed
        character(len=:), allocatable :: lowered

        is_implicit = .false.
        if (.not. associated(parser%tokens)) return

        token_count = size(parser%tokens)
        if (token_count == 0) return

        idx = parser%current_token + 1
        continuation_allowed = .false.

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
                case default
                    return
                end select
            case default
                exit
            end select
        end do

        if (idx > token_count) return

        tok = parser%tokens(idx)
        if (tok%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(tok%text))
        select case (lowered)
        case ("none", "integer", "real", "logical", "character", "complex", &
              "double", "type", "class", "procedure")
            is_implicit = .true.
        case default
            is_implicit = .false.
        end select
    end function looks_like_implicit_statement

end module parser_keyword_disambiguation_module
