module parser_do_concurrent_locality_module
    ! Parses and validates the locality-spec-list of a DO CONCURRENT statement
    ! (Fortran 2023 R1130/R1131) and enforces the locality constraints that are
    ! decidable from the statement text alone:
    !   C1129  DEFAULT (NONE) shall not appear more than once.
    !   C1130  A variable-name shall not appear in more than one locality-spec.
    ! The reduce-operation is restricted to the operators and intrinsic function
    ! names listed in R1131, and REDUCE requires a ":" between the operation and
    ! its variable-name-list.
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, &
        TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    implicit none
    private

    integer, parameter :: MAX_LOCALITY_NAME = 63

    public :: parse_do_concurrent_locality_specs

contains

    ! Consume every locality-spec that follows the concurrent-header. Returns
    ! .false. after recording a parser diagnostic when the list is malformed or
    ! violates a locality constraint.
    logical function parse_do_concurrent_locality_specs(parser) result(valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=MAX_LOCALITY_NAME), allocatable :: seen(:)
        type(token_t) :: tok
        character(len=:), allocatable :: spec
        integer :: default_none_count

        valid = .true.
        default_none_count = 0
        allocate (seen(0))

        do
            call skip_blanks(parser)
            tok = parser%peek()
            if (ends_locality_list(tok)) return

            ! Only "name (" can start a locality-spec. Anything else is left
            ! alone so that token streams which omit the statement-ending
            ! newline still fall through to the loop body unchanged.
            if (.not. starts_locality_spec(parser)) return

            spec = to_lower(trim(tok%text))
            select case (spec)
            case ("local", "local_init", "shared")
                tok = parser%consume()
                if (.not. parse_name_list_spec(parser, spec, seen)) then
                    valid = .false.
                    return
                end if
            case ("default")
                tok = parser%consume()
                if (.not. parse_default_spec(parser, default_none_count)) then
                    valid = .false.
                    return
                end if
            case ("reduce")
                tok = parser%consume()
                if (.not. parse_reduce_spec(parser, seen)) then
                    valid = .false.
                    return
                end if
            case default
                call parser%error_at_token("Syntax error in DO CONCURRENT "// &
                    "statement: unknown locality specifier '"// &
                    trim(tok%text)//"'", tok, &
                    suggestion="use LOCAL, LOCAL_INIT, SHARED, REDUCE or "// &
                    "DEFAULT (NONE)")
                valid = .false.
                return
            end select
        end do
    end function parse_do_concurrent_locality_specs

    ! LOCAL / LOCAL_INIT / SHARED ( variable-name-list )
    logical function parse_name_list_spec(parser, spec, seen) result(valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: spec
        character(len=MAX_LOCALITY_NAME), allocatable, intent(inout) :: seen(:)

        valid = expect_open_paren(parser, spec)
        if (.not. valid) return
        valid = parse_variable_name_list(parser, spec, seen)
        if (.not. valid) return
        valid = expect_close_paren(parser, spec)
    end function parse_name_list_spec

    ! DEFAULT ( NONE ), at most once per statement (C1129).
    logical function parse_default_spec(parser, default_none_count) result(valid)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(inout) :: default_none_count
        type(token_t) :: tok

        valid = expect_open_paren(parser, "default")
        if (.not. valid) return

        call skip_blanks(parser)
        tok = parser%peek()
        if (tok%kind /= TK_IDENTIFIER .and. tok%kind /= TK_KEYWORD) then
            call report_expected(parser, tok, "NONE", "DEFAULT")
            valid = .false.
            return
        end if
        if (to_lower(trim(tok%text)) /= "none") then
            call report_expected(parser, tok, "NONE", "DEFAULT")
            valid = .false.
            return
        end if
        tok = parser%consume()

        valid = expect_close_paren(parser, "default")
        if (.not. valid) return

        default_none_count = default_none_count + 1
        if (default_none_count > 1) then
            call parser%error_at_token("DEFAULT (NONE) specified more than "// &
                "once in DO CONCURRENT", tok, &
                suggestion="give DEFAULT (NONE) exactly once")
            valid = .false.
        end if
    end function parse_default_spec

    ! REDUCE ( reduce-operation : variable-name-list )
    logical function parse_reduce_spec(parser, seen) result(valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=MAX_LOCALITY_NAME), allocatable, intent(inout) :: seen(:)
        type(token_t) :: tok

        valid = expect_open_paren(parser, "reduce")
        if (.not. valid) return

        call skip_blanks(parser)
        tok = parser%peek()
        if (.not. is_reduce_operation(tok)) then
            call parser%error_at_token("Expected reduction operator or "// &
                "function name in DO CONCURRENT REDUCE specifier", tok, &
                suggestion="use +, *, .and., .or., .eqv., .neqv., max, min, "// &
                "iand, ior or ieor")
            valid = .false.
            return
        end if
        tok = parser%consume()

        call skip_blanks(parser)
        tok = parser%peek()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= ":") then
            call report_expected(parser, tok, "':'", "REDUCE")
            valid = .false.
            return
        end if
        tok = parser%consume()

        valid = parse_variable_name_list(parser, "reduce", seen)
        if (.not. valid) return
        valid = expect_close_paren(parser, "reduce")
    end function parse_reduce_spec

    ! variable-name-list with the C1130 single-appearance check.
    logical function parse_variable_name_list(parser, spec, seen) result(valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: spec
        character(len=MAX_LOCALITY_NAME), allocatable, intent(inout) :: seen(:)
        type(token_t) :: tok
        character(len=:), allocatable :: name

        valid = .true.
        do
            call skip_blanks(parser)
            tok = parser%peek()
            if (tok%kind /= TK_IDENTIFIER .and. tok%kind /= TK_KEYWORD) then
                call report_expected(parser, tok, "a variable name", spec)
                valid = .false.
                return
            end if
            name = to_lower(trim(tok%text))
            if (name_already_seen(name, seen)) then
                call parser%error_at_token("Variable '"//trim(tok%text)// &
                    "' has already been specified in a locality-spec of this "// &
                    "DO CONCURRENT statement", tok, &
                    suggestion="name each variable in at most one locality-spec")
                valid = .false.
                return
            end if
            call remember_name(name, seen)
            tok = parser%consume()

            call skip_blanks(parser)
            tok = parser%peek()
            if (tok%kind == TK_OPERATOR .and. tok%text == ",") then
                tok = parser%consume()
                cycle
            end if
            return
        end do
    end function parse_variable_name_list

    logical function expect_open_paren(parser, spec) result(valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: spec
        type(token_t) :: tok

        call skip_blanks(parser)
        tok = parser%peek()
        valid = tok%kind == TK_OPERATOR
        if (valid) valid = tok%text == "("
        if (.not. valid) then
            call report_expected(parser, tok, "'('", spec)
            return
        end if
        tok = parser%consume()
    end function expect_open_paren

    logical function expect_close_paren(parser, spec) result(valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: spec
        type(token_t) :: tok

        call skip_blanks(parser)
        tok = parser%peek()
        valid = tok%kind == TK_OPERATOR
        if (valid) valid = tok%text == ")"
        if (.not. valid) then
            call report_expected(parser, tok, "')'", spec)
            return
        end if
        tok = parser%consume()
    end function expect_close_paren

    subroutine report_expected(parser, tok, expected, spec)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: tok
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: spec

        call parser%error_at_token("Expected "//expected//" in DO CONCURRENT "// &
            trim(spec)//" locality specifier", tok)
    end subroutine report_expected

    ! R1131 reduce-operation: an intrinsic operator or intrinsic function name.
    logical function is_reduce_operation(tok) result(is_operation)
        type(token_t), intent(in) :: tok
        character(len=:), allocatable :: text

        is_operation = .false.
        if (tok%kind /= TK_OPERATOR .and. tok%kind /= TK_IDENTIFIER .and. &
            tok%kind /= TK_KEYWORD) return

        text = to_lower(trim(tok%text))
        select case (text)
        case ("+", "*", ".and.", ".or.", ".eqv.", ".neqv.", &
                "max", "min", "iand", "ior", "ieor")
            is_operation = .true.
        end select
    end function is_reduce_operation

    ! A locality-spec is always "name ( ... )". Peeking one token past the name
    ! keeps the check off ordinary loop-body statements.
    logical function starts_locality_spec(parser) result(is_spec)
        type(parser_state_t), intent(in) :: parser
        type(token_t) :: name_tok
        type(token_t) :: next_tok
        integer :: idx

        is_spec = .false.
        name_tok = parser%peek()
        if (name_tok%kind /= TK_IDENTIFIER .and. name_tok%kind /= TK_KEYWORD) return

        idx = parser%current_token + 1
        do
            next_tok = parser%get_token_at_index(idx)
            if (next_tok%kind /= TK_WHITESPACE) exit
            idx = idx + 1
        end do

        if (next_tok%kind /= TK_OPERATOR) return
        is_spec = next_tok%text == "("
    end function starts_locality_spec

    logical function ends_locality_list(tok) result(is_end)
        type(token_t), intent(in) :: tok

        is_end = .false.
        select case (tok%kind)
        case (TK_NEWLINE, TK_EOF, TK_COMMENT)
            is_end = .true.
            return
        end select
        if (tok%kind /= TK_OPERATOR) return
        is_end = tok%text == ";"
    end function ends_locality_list

    subroutine skip_blanks(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: tok

        do
            tok = parser%peek()
            if (tok%kind /= TK_WHITESPACE) return
            tok = parser%consume()
        end do
    end subroutine skip_blanks

    logical function name_already_seen(name, seen) result(is_present)
        character(len=*), intent(in) :: name
        character(len=MAX_LOCALITY_NAME), intent(in) :: seen(:)
        integer :: i

        is_present = .false.
        do i = 1, size(seen)
            if (trim(seen(i)) == name) then
                is_present = .true.
                return
            end if
        end do
    end function name_already_seen

    subroutine remember_name(name, seen)
        character(len=*), intent(in) :: name
        character(len=MAX_LOCALITY_NAME), allocatable, intent(inout) :: seen(:)
        character(len=MAX_LOCALITY_NAME), allocatable :: grown(:)
        integer :: n

        n = size(seen)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = seen
        grown(n + 1) = name
        call move_alloc(grown, seen)
    end subroutine remember_name

end module parser_do_concurrent_locality_module
