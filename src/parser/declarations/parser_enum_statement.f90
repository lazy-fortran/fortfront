module parser_enum_statement_module
    ! Parse F2003 ENUM constructs into a structured enum_node:
    !   enum, bind(c)
    !       enumerator :: red, green = 5, blue
    !   end enum
    ! Enumerator values follow F2003 4.6: an explicit value sets the counter;
    ! an implicit value is the previous value plus one, starting at zero.
    !
    ! The body of an enum-def (F2003 R460) admits nothing but
    ! enumerator-def-stmt between ENUM and END ENUM, an initialized enumerator
    ! requires the "::" separator, and an enumerator value must be an integer
    ! within the kind of the enumeration. Violations are recorded on the node
    ! and reported by semantic_enum_validation; the parser keeps parsing so a
    ! single malformed enumerator does not swallow the rest of the unit.
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, TK_OPERATOR, TK_NUMBER, TK_STRING, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_factory, only: push_enum
    use semantic_constant_values, only: integer_literal_fits_default_kind
    implicit none
    private

    public :: parse_enum_construct

    character(len=*), parameter :: MSG_UNEXPECTED_STATEMENT = &
        "Unexpected statement in ENUM definition: only ENUMERATOR statements "// &
        "may appear between ENUM and END ENUM"
    character(len=*), parameter :: MSG_NESTED_ENUM = &
        "Unexpected ENUM statement: an ENUM definition may not be nested "// &
        "inside another ENUM definition"
    character(len=*), parameter :: MSG_MISSING_COLONS = &
        "Syntax error in ENUMERATOR definition: '::' is required before an "// &
        "initialized enumerator"
    character(len=*), parameter :: MSG_NON_INTEGER_VALUE = &
        "ENUMERATOR must be initialized with an integer expression"
    character(len=*), parameter :: MSG_VALUE_TOO_BIG = &
        "ENUMERATOR value is too big for its kind"

    ! Constraint violations collected while parsing one ENUM body.
    type :: enum_violations_t
        type(string_t), allocatable :: messages(:)
        integer, allocatable :: lines(:)
        integer, allocatable :: columns(:)
    end type enum_violations_t

contains

    integer function parse_enum_construct(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        type(string_t), allocatable :: names(:)
        integer, allocatable :: values(:)
        type(enum_violations_t) :: violations
        logical :: is_bind_c
        integer :: line, column, next_value

        allocate (names(0))
        allocate (values(0))
        call init_violations(violations)
        is_bind_c = .false.
        next_value = 0

        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume() ! consume "enum"

        is_bind_c = consume_enum_header(parser)

        do while (.not. parser%is_at_end())
            call skip_trivia_and_newlines(parser)
            if (parser%is_at_end()) exit
            token = parser%peek()
            if (is_end_enum(parser)) then
                call consume_end_enum(parser)
                exit
            end if
            if (is_word(token, "enumerator")) then
                call parse_enumerator_line(parser, names, values, next_value, &
                    violations)
            else if (is_word(token, "enum")) then
                call add_violation(violations, MSG_NESTED_ENUM, token)
                call skip_nested_enum(parser)
            else
                call add_violation(violations, MSG_UNEXPECTED_STATEMENT, token)
                call skip_rest_of_line(parser)
            end if
        end do

        stmt_index = push_enum(arena, names, values, is_bind_c, &
            line=line, column=column, &
            violation_messages=violations%messages, &
            violation_lines=violations%lines, &
            violation_columns=violations%columns)
    end function parse_enum_construct

    subroutine init_violations(violations)
        type(enum_violations_t), intent(out) :: violations

        allocate (violations%messages(0))
        allocate (violations%lines(0))
        allocate (violations%columns(0))
    end subroutine init_violations

    subroutine add_violation(violations, message, token)
        type(enum_violations_t), intent(inout) :: violations
        character(len=*), intent(in) :: message
        type(token_t), intent(in) :: token

        violations%messages = [violations%messages, string_t(message)]
        violations%lines = [violations%lines, token%line]
        violations%columns = [violations%columns, token%column]
    end subroutine add_violation

    logical function is_word(token, word) result(matches)
        type(token_t), intent(in) :: token
        character(len=*), intent(in) :: word

        matches = .false.
        if (token%kind /= TK_KEYWORD .and. token%kind /= TK_IDENTIFIER) return
        matches = to_lower(trim(token%text)) == word
    end function is_word

    logical function consume_enum_header(parser) result(is_bind_c)
        ! Consume the rest of the "enum[, bind(c)]" header line.
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        is_bind_c = .false.
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                if (to_lower(trim(token%text)) == "bind") is_bind_c = .true.
            end if
            token = parser%consume()
        end do
    end function consume_enum_header

    subroutine skip_rest_of_line(parser)
        ! Discard the remainder of a statement that does not belong in an ENUM
        ! body, so the ENUMERATOR statements that follow it still parse.
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            token = parser%consume()
            if (token%kind == TK_NEWLINE) exit
        end do
    end subroutine skip_rest_of_line

    subroutine skip_nested_enum(parser)
        ! Discard a nested enum-def, including its END ENUM, so that the inner
        ! END ENUM does not terminate the enclosing definition.
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        call skip_rest_of_line(parser)
        do while (.not. parser%is_at_end())
            call skip_trivia_and_newlines(parser)
            if (parser%is_at_end()) exit
            if (is_end_enum(parser)) then
                call consume_end_enum(parser)
                exit
            end if
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            call skip_rest_of_line(parser)
        end do
    end subroutine skip_nested_enum

    subroutine parse_enumerator_line(parser, names, values, next_value, violations)
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(inout) :: names(:)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(inout) :: next_value
        type(enum_violations_t), intent(inout) :: violations
        type(token_t) :: token
        integer :: this_value
        logical :: has_colons

        token = parser%consume() ! consume "enumerator"
        has_colons = consume_separator_tokens(parser)

        do while (.not. parser%is_at_end())
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
            if (token%kind == TK_IDENTIFIER) then
                names = [names, string_t(trim(token%text))]
                token = parser%consume()
                this_value = next_value
                call skip_inline_trivia(parser)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. trim(token%text) == "=") then
                    if (.not. has_colons) then
                        call add_violation(violations, MSG_MISSING_COLONS, token)
                    end if
                    token = parser%consume()
                    this_value = parse_int_value(parser, violations)
                end if
                values = [values, this_value]
                next_value = this_value + 1
            else
                token = parser%consume()
            end if
        end do
    end subroutine parse_enumerator_line

    integer function parse_int_value(parser, violations) result(value)
        type(parser_state_t), intent(inout) :: parser
        type(enum_violations_t), intent(inout) :: violations
        type(token_t) :: token
        integer :: sign_factor, magnitude
        logical :: fits, is_integer

        value = 0
        sign_factor = 1
        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. trim(token%text) == "-") then
            sign_factor = -1
            token = parser%consume()
            call skip_inline_trivia(parser)
            token = parser%peek()
        else if (token%kind == TK_OPERATOR .and. trim(token%text) == "+") then
            token = parser%consume()
            call skip_inline_trivia(parser)
            token = parser%peek()
        end if
        if (token%kind == TK_STRING) then
            call add_violation(violations, MSG_NON_INTEGER_VALUE, token)
            token = parser%consume()
            return
        end if
        if (token%kind /= TK_NUMBER) return
        if (is_real_literal_text(trim(token%text))) then
            call add_violation(violations, MSG_NON_INTEGER_VALUE, token)
            token = parser%consume()
            return
        end if
        fits = integer_literal_fits_default_kind(trim(token%text), magnitude, &
            is_integer_literal=is_integer)
        if (is_integer .and. .not. fits) then
            call add_violation(violations, MSG_VALUE_TOO_BIG, token)
        end if
        if (fits) value = sign_factor * magnitude
        token = parser%consume()
    end function parse_int_value

    logical function is_real_literal_text(text) result(is_real)
        ! A numeric literal is real when it carries a decimal point or a real
        ! exponent letter. Kind-suffixed integers such as 42_8 are not real.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered
        integer :: underscore_pos

        lowered = to_lower(text)
        underscore_pos = index(lowered, '_')
        if (underscore_pos > 0) lowered = lowered(1:underscore_pos - 1)
        is_real = index(lowered, '.') > 0
        if (.not. is_real) is_real = index(lowered, 'e') > 0
        if (.not. is_real) is_real = index(lowered, 'd') > 0
    end function is_real_literal_text

    logical function is_end_enum(parser) result(at_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        at_end = .false.
        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return
        lowered = to_lower(trim(token%text))
        if (lowered == "endenum") then
            at_end = .true.
        else if (lowered == "end") then
            at_end = .true.
        end if
    end function is_end_enum

    subroutine consume_end_enum(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        token = parser%consume() ! "end" or "endenum"
        lowered = to_lower(trim(token%text))
        if (lowered == "end") then
            call skip_inline_trivia(parser)
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    if (to_lower(trim(token%text)) == "enum") token = parser%consume()
                end if
            end if
        end if
    end subroutine consume_end_enum

    logical function consume_separator_tokens(parser) result(has_colons)
        ! Skip whitespace and an optional "::" before the enumerator name list.
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        has_colons = .false.
        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. trim(token%text) == "::") then
            token = parser%consume()
            has_colons = .true.
        end if
    end function consume_separator_tokens

    subroutine skip_inline_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_inline_trivia

    subroutine skip_trivia_and_newlines(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_trivia_and_newlines

end module parser_enum_statement_module
