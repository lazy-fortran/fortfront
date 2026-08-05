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
                if (assignment_operator_immediately_follows(parser)) then
                    as_identifier = .true.
                else
                    as_identifier = statement_contains_assignment(parser)
                end if
            end if
        end select
    end function keyword_should_parse_as_identifier

    logical function keyword_supports_assignment_disambiguation(keyword) &
            result(is_supported)
        character(len=*), intent(in) :: keyword

        ! `parameter = x` and `rank = n` assign to variables of those names.
        ! The statement forms are `parameter (n = 5)` and `rank (n)`, which
        ! have a parenthesis where these have an assignment operator, so the
        ! two cannot be confused.
        !
        ! `block = x` likewise assigns to a variable; the construct form is a
        ! bare `block` with nothing after it. Mistaking one for the other
        ! sends the parser looking for an `end block` that is not there.
        select case (keyword)
        case ("call", "stop", "cycle", "exit", "return", "error", &
                "continue", "goto", "go", "entry", "select", &
                "contains", "else", "dimension", "common", &
                "program", "module", "submodule", "if", "data", &
                "read", "write", "print", "open", "close", &
                "inquire", "backspace", "rewind", "endfile", "format", &
                "parameter", "rank", "block", "operator")
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
                case (",")
                    ! An assignment's `=` always comes before any top-level
                    ! comma: `x = [1, 2]` has it first. An `=` after one is a
                    ! specifier keyword, as in `stop 1, quiet=.true.`, and
                    ! reading that as an assignment turned the statement into
                    ! an assignment to a variable named `stop`.
                    if (depth == 0) return
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

    logical function assignment_operator_immediately_follows(parser) &
            result(has_assignment)
        type(parser_state_t), intent(in) :: parser
        integer :: idx, token_count
        type(token_t) :: tok

        has_assignment = .false.
        if (.not. associated(parser%tokens)) return

        token_count = size(parser%tokens)
        if (token_count < parser%current_token) return

        idx = parser%current_token + 1
        do while (idx <= token_count)
            tok = parser%tokens(idx)
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NEWLINE)
                return
            case default
                if (tok%kind == TK_OPERATOR) then
                    if (trim(tok%text) == "=" .or. trim(tok%text) == "=>") then
                        has_assignment = .true.
                    end if
                end if
                return
            end select
        end do
    end function assignment_operator_immediately_follows

    logical function looks_like_format_statement(parser) result(is_format)
        type(parser_state_t), intent(in) :: parser
        integer :: idx, depth, token_count

        is_format = .false.
        if (.not. associated(parser%tokens)) return

        token_count = size(parser%tokens)
        if (token_count == 0) return

        idx = parser%current_token + 1
        if (.not. find_format_opening_paren(parser, idx, token_count)) return

        depth = 1
        idx = idx + 1
        if (.not. walk_format_specifier(parser, idx, depth, token_count)) return

        is_format = check_format_statement_end(parser, idx, token_count)
    end function looks_like_format_statement

    logical function find_format_opening_paren(parser, idx, token_count) &
            result(found)
        type(parser_state_t), intent(in) :: parser
        integer, intent(inout) :: idx
        integer, intent(in) :: token_count
        type(token_t) :: tok
        logical :: continuation_allowed

        found = .false.
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
                case ("(")
                    found = .true.
                    return
                case default
                    return
                end select
            case default
                return
            end select
        end do
    end function find_format_opening_paren

    logical function walk_format_specifier(parser, idx, depth, token_count) &
            result(valid)
        type(parser_state_t), intent(in) :: parser
        integer, intent(inout) :: idx
        integer, intent(inout) :: depth
        integer, intent(in) :: token_count
        type(token_t) :: tok
        logical :: continuation_allowed

        valid = .false.
        continuation_allowed = .false.

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

        valid = (depth == 0)
    end function walk_format_specifier

    logical function check_format_statement_end(parser, idx, token_count) &
            result(is_valid)
        type(parser_state_t), intent(in) :: parser
        integer, intent(inout) :: idx
        integer, intent(in) :: token_count
        type(token_t) :: tok
        logical :: trailing_continuation

        is_valid = .false.
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
                is_valid = .true.
                return
            case (TK_EOF)
                is_valid = .true.
                return
            case (TK_OPERATOR)
                select case (tok%text)
                case (";")
                    is_valid = .true.
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

        is_valid = .true.
    end function check_format_statement_end

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
        if (tok%kind /= TK_KEYWORD .and. tok%kind /= TK_IDENTIFIER) return

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
