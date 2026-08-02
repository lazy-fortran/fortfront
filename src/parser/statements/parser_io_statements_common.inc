    ! Parse format specifier (common logic for write/read)
    subroutine parse_format_specifier(parser, format_spec, is_control_specifier)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: format_spec
        procedure(io_control_specifier_predicate), optional :: &
            is_control_specifier
        type(token_t) :: token
        logical :: is_control_token

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            format_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_STRING) then
            format_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            is_control_token = .false.
            if (present(is_control_specifier)) then
                is_control_token = is_control_specifier(token)
            end if
            if (.not. is_control_token) then
                format_spec = token%text
                token = parser%consume()
            else
                format_spec = ""
            end if
        else if (token%kind == TK_NUMBER) then
            format_spec = token%text
            token = parser%consume()
        else
            format_spec = ""
        end if
    end subroutine parse_format_specifier

    ! Parse a positional format that may be a character expression.
    ! When the format is a single literal/identifier/label, returns it as
    ! format_spec text. When it continues with a binary operator (an
    ! expression such as "(" // fmt // ")"), parses the whole format as an
    ! expression and returns its arena index in format_expr_index.
    subroutine parse_positional_format(parser, arena, format_spec, &
                                       format_expr_index, is_control_specifier)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out) :: format_spec
        integer, intent(out) :: format_expr_index
        procedure(io_control_specifier_predicate), optional :: &
            is_control_specifier
        type(token_t) :: token
        integer :: saved_pos

        format_expr_index = 0
        saved_pos = parser%current_token

        call parse_format_specifier(parser, format_spec, is_control_specifier)
        if (allocated(format_spec)) then
            if (len(format_spec) == 0) deallocate (format_spec)
        end if

        if (.not. allocated(format_spec)) return

        ! If the simple format spec is followed by a binary operator, the
        ! format is actually an expression; reparse it as a whole.
        token = parser%peek()
        if (token%kind == TK_OPERATOR) then
            select case (token%text)
            case ("//", "+", "-", "*", "/")
                deallocate (format_spec)
                parser%current_token = saved_pos
                format_expr_index = parse_comparison(parser, arena)
                if (format_expr_index <= 0) then
                    parser%current_token = saved_pos
                    call parse_format_specifier(parser, format_spec, &
                                                is_control_specifier)
                    if (allocated(format_spec)) then
                        if (len(format_spec) == 0) deallocate (format_spec)
                    end if
                end if
            end select
        end if
    end subroutine parse_positional_format

    subroutine skip_io_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_io_trivia

    logical function parse_keyworded_format_clause(parser, format_spec) &
        result(found)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: format_spec

        type(parser_state_t) :: checkpoint
        type(token_t) :: token
        integer :: clause_line

        found = .false.
        clause_line = 0

        checkpoint = parser
        call skip_io_trivia(checkpoint)

        token = checkpoint%peek()
        if (.not. is_format_specifier_keyword(token)) return

        clause_line = token%line
        token = checkpoint%consume()
        call skip_io_trivia(checkpoint)

        token = checkpoint%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "=") return
        token = checkpoint%consume()

        call skip_io_trivia(checkpoint)
        call parse_format_specifier(checkpoint, format_spec)
        if (.not. allocated(format_spec)) return
        if (len(format_spec) == 0) then
            write (error_unit, *) &
                "Error: Expected format specifier after 'fmt=' at line ", &
                clause_line
            deallocate (format_spec)
            return
        end if

        parser%current_token = checkpoint%current_token
        found = .true.
    end function parse_keyworded_format_clause

    ! Parse unit specifier (common logic for write/read)
    function parse_unit_specifier(parser) result(unit_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable :: unit_spec
        type(token_t) :: token
        type(parser_state_t) :: checkpoint

        ! Accept optional keyword form: unit=<value>
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            if (to_lower(trim(token%text)) == "unit") then
                checkpoint = parser
                token = checkpoint%consume()  ! tentatively consume "unit"
                token = checkpoint%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = checkpoint%consume()  ! consume "="
                    parser = checkpoint
                end if
            end if
        end if

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            unit_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            unit_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            unit_spec = token%text
            token = parser%consume()
        else
            unit_spec = ""
        end if
    end function parse_unit_specifier

    ! Collect trailing specification for backspace/rewind/endfile statements
    function collect_position_spec(parser) result(spec_text)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable :: spec_text
        type(token_t) :: token
        character :: last_char
        logical :: needs_space

        spec_text = ""

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) exit
            if (token%kind == TK_COMMENT) exit
            if (token%kind == TK_WHITESPACE) then
                token = parser%consume()
                cycle
            end if
            if (token%kind == TK_OPERATOR) then
                if (token%text == ";") exit
            end if

            needs_space = len(spec_text) > 0
            if (needs_space) then
                last_char = spec_text(len(spec_text):len(spec_text))
                if (last_char == ' ') needs_space = .false.
                ! Never add space before string literals
                if (token%kind == TK_STRING) needs_space = .false.
                select case (token%text)
                case (")", ",")
                    needs_space = .false.
                case ("=")
                    if (last_char /= '(') needs_space = .true.
                case ("(")
                    if (last_char /= '(' .and. last_char /= ' ') needs_space = .true.
                    if (last_char == '(') needs_space = .false.
                case default
                    if (last_char == '(') needs_space = .false.
                    ! Do not add space after = to prevent breaking string literals
                    if (last_char == '=') needs_space = .false.
                end select
            end if

            if (needs_space) spec_text = spec_text // " "

            token = parser%consume()
            spec_text = spec_text // token%text
            if (token%kind == TK_OPERATOR) then
                if (token%text == ",") spec_text = spec_text // " "
            end if
        end do

        spec_text = trim(spec_text)
    end function collect_position_spec

    subroutine prepare_position_specifier_parser(parser, specifier_parser)
        type(parser_state_t), intent(in) :: parser
        type(parser_state_t), intent(out) :: specifier_parser
        type(token_t) :: token

        specifier_parser = parser
        token = specifier_parser%peek()
        if (token%kind == TK_OPERATOR) then
            if (token%text == '(') token = specifier_parser%consume()
        end if
    end subroutine prepare_position_specifier_parser

    ! Parse argument list (common logic for print/write/read)
    subroutine parse_argument_list(parser, arena, arg_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: current_arg_index

        allocate (arg_indices(0))

        token = parser%peek()
        if (parser%is_at_end() .or. token%kind == TK_NEWLINE .or. &
            token%kind == TK_COMMENT) return

        ! Parse first argument
        current_arg_index = parse_io_implied_do(parser, arena)
        if (current_arg_index == 0) current_arg_index = &
            parse_range(parser, arena)
        if (current_arg_index > 0) then
            arg_indices = [current_arg_index]

            ! Parse additional arguments separated by commas
            do
                token = parser%peek()
                if (token%kind == TK_NEWLINE .or. token%kind == TK_COMMENT) exit
                if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit

                token = parser%consume()  ! consume comma
                current_arg_index = parse_io_implied_do(parser, arena)
                if (current_arg_index == 0) current_arg_index = &
                    parse_range(parser, arena)
                if (current_arg_index > 0) then
                    arg_indices = [arg_indices, current_arg_index]
                else
                    exit
                end if
            end do
        end if
    end subroutine parse_argument_list

    ! Parse implied-do expression specific to I/O lists: &
    ! (expr, var = start, end [, step])
    function parse_io_implied_do(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        type(token_t) :: token
        integer :: saved_pos
        integer :: value_expr_index
        integer :: start_index, end_index, step_index
        character(len=:), allocatable :: var_name
        integer :: line, column

        expr_index = 0
        saved_pos = parser%current_token

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") return
        line = token%line
        column = token%column
        token = parser%consume()  ! consume (

        ! A legacy array constructor starts with `(/`, not with an I/O
        ! implied-do expression.  Leave it for the expression parser; without
        ! this guard the slash is misread as division with a missing left node.
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "/") then
            expr_index = fail_and_restore()
            return
        end if

        value_expr_index = parse_comparison(parser, arena)
        if (value_expr_index <= 0) then
            expr_index = fail_and_restore()
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ",") then
            expr_index = fail_and_restore()
            return
        end if
        token = parser%consume()

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            expr_index = fail_and_restore()
            return
        end if
        var_name = token%text
        token = parser%consume()

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "=") then
            expr_index = fail_and_restore()
            return
        end if
        token = parser%consume()

        ! Past "( expr , name =" the item can only be an io-implied-do
        ! (F2023 R1224), so the rest of the iterator and its closing right
        ! parenthesis are required rather than speculative.
        start_index = parse_comparison(parser, arena)
        if (start_index <= 0) then
            call parser%error("Syntax error in iterator: expected an initial "// &
                "bound expression")
            expr_index = fail_and_restore()
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ",") then
            call parser%error_at_token("Syntax error in iterator: expected "// &
                "',' after the initial bound", token)
            expr_index = fail_and_restore()
            return
        end if
        token = parser%consume()

        end_index = parse_comparison(parser, arena)
        if (end_index <= 0) then
            call parser%error("Syntax error in iterator: expected a terminal "// &
                "bound expression")
            expr_index = fail_and_restore()
            return
        end if

        step_index = 0
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
            step_index = parse_comparison(parser, arena)
            if (step_index <= 0) then
                call parser%error("Syntax error in iterator: expected a "// &
                    "stride expression")
                expr_index = fail_and_restore()
                return
            end if
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
            call parser%error_at_token("Expected a right parenthesis closing "// &
                "the implied-DO in the I/O list", token, &
                suggestion="close the implied-DO with ')'")
            expr_index = fail_and_restore()
            return
        end if
        token = parser%consume()

        if (step_index > 0) then
            expr_index = push_io_implied_do(arena, value_expr_index, var_name, &
                                            start_expr_index=start_index, &
                                            end_expr_index=end_index, &
                                            step_expr_index=step_index, line=line, &
                                            column=column)
        else
            expr_index = push_io_implied_do(arena, value_expr_index, var_name, &
                                            start_expr_index=start_index, &
                                            end_expr_index=end_index, line=line, &
                                            column=column)
        end if
        if (allocated(var_name)) then
            block
                character(len=:), allocatable :: temp
                call move_alloc(var_name, temp)
            end block
        end if
        return

    contains
        integer function fail_and_restore()
            if (allocated(var_name)) then
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(var_name, temp)
                end block
            end if
            parser%current_token = saved_pos
            fail_and_restore = 0
        end function fail_and_restore
    end function parse_io_implied_do
