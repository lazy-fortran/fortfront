    function parse_print_statement(parser, arena) result(print_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: print_index

        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        character(len=:), allocatable :: format_spec

        ! Consume print keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Parse format spec (*, format string, or format variable)
        call parse_format_specifier(parser, format_spec)
        if (len(format_spec) == 0) format_spec = "*"  ! Default

        ! Skip comma after format spec if present
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
        end if

        ! Parse all print arguments
        call parse_argument_list(parser, arena, arg_indices)

        ! Create print statement node with parsed arguments
        print_index = push_print_statement(arena, format_spec, arg_indices, &
                                           line, column)
    end function parse_print_statement

    function parse_write_statement(parser, arena) result(write_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: write_index

        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        character(len=:), allocatable :: unit_spec, format_spec, namelist_group
        character(len=:), allocatable :: io_control_list
        logical :: has_keyworded_format
        integer :: format_expr_index

        format_expr_index = 0

        ! Check if were at write keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "write") then
            ! Consume write keyword
            token = parser%consume()
        else
            write_index = 0
            return
        end if
        line = token%line
        column = token%column

        ! Expect opening parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            write (error_unit, *) "Error: Expected '(' after 'write' at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Parse unit specifier
        unit_spec = parse_unit_specifier(parser)
        if (len(unit_spec) == 0) then
            write (error_unit, *) &
                "Error: Expected unit specifier in write statement at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Check for format specifier or namelist (optional)
        has_keyworded_format = .false.
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()  ! consume comma

            ! Check if next token is nml keyword
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. token%text == "nml") then
                token = parser%consume()  ! consume nml

                ! Expect = after nml
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume =

                    ! Parse namelist group name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        namelist_group = token%text
                        token = parser%consume()
                    else
                        write (error_unit, *) &
                            "Error: Expected namelist group name after 'nml='", &
                            " at line ", token%line
                    end if
                end if
            else
                has_keyworded_format = parse_keyworded_format_clause(parser, &
                                                                     format_spec)
                if (.not. has_keyworded_format) then
                    ! Parse positional format specifier (literal or expression)
                    call parse_positional_format(parser, arena, format_spec, &
                                                 format_expr_index, &
                                                 is_control_specifier= &
                                                 is_write_io_control_specifier)
                end if
            end if
        end if

        ! Collect any additional I/O control specifiers (iostat, iomsg, etc.)
        io_control_list = collect_io_control_specifiers(parser, &
                                                        is_write_io_control_specifier)

        ! Expect closing parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
            write (error_unit, *) &
                "Error: Expected ')' after write unit and format at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Parse all write arguments
        call parse_argument_list(parser, arena, arg_indices)

        ! Create write statement node with parsed arguments
        if (allocated(namelist_group)) then
            write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                               namelist_group=namelist_group, &
                                               io_control_list=io_control_list, &
                                               line=line, column=column)
        else if (format_expr_index > 0) then
            write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                               format_expr_index=format_expr_index, &
                                               io_control_list=io_control_list, &
                                               line=line, column=column)
        else if (allocated(format_spec)) then
            write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                               format_spec=format_spec, &
                                               io_control_list=io_control_list, &
                                               line=line, column=column)
        else
            write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                               io_control_list=io_control_list, &
                                               line=line, column=column)
        end if
    end function parse_write_statement

    function parse_read_statement(parser, arena) result(read_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: read_index

        type(token_t) :: token
        integer, allocatable :: var_indices(:)
        integer :: line, column
        character(len=:), allocatable :: unit_spec, format_spec, namelist_group
        character(len=:), allocatable :: io_control_list
        type(parser_state_t) :: checkpoint
        integer :: format_expr_index

        format_expr_index = 0

        ! Consume read keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Check for opening parenthesis (determines read format)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            ! Format: read (unit, format) variables
            token = parser%consume()  ! consume (

            ! Parse unit specifier
            unit_spec = parse_unit_specifier(parser)
            if (len(unit_spec) == 0) then
                write (error_unit, *) &
                    "Error: Expected unit specifier in read statement at line ", &
                    token%line
                read_index = 0
                return
            end if

            ! Check for format specifier or namelist (optional)
            format_spec = ""
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume comma

                ! Check if next token is nml keyword for namelist
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER .and. token%text == "nml") then
                    token = parser%consume()  ! consume nml

                    ! Expect = after nml
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        token = parser%consume()  ! consume =

                        ! Parse namelist group name
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER) then
                            namelist_group = token%text
                            token = parser%consume()
                        else
                            write (error_unit, *) &
                                "Error: Expected namelist group name after 'nml=' &
                                &at line ", token%line
                        end if
                    end if
                else if (is_format_specifier_keyword(token)) then
                    ! Check if next token is a keyword parameter (e.g., fmt=)
                    ! Only consume keyword if followed by =
                    ! Save position to check for = before committing
                    checkpoint = parser
                    token = checkpoint%consume()  ! tentatively consume keyword

                    ! Check if = follows
                    token = checkpoint%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        ! Its a keyworded format (fmt= or format=)
                        parser = checkpoint  ! commit keyword consumption
                        token = parser%consume()  ! consume =
                    end if
                    ! Otherwise, treat as positional format variable
                end if

                if (.not. allocated(namelist_group)) then
                    call parse_positional_format(parser, arena, format_spec, &
                                                 format_expr_index, &
                                                 is_control_specifier= &
                                                 is_read_io_control_specifier)
                end if
            end if

            ! Collect any additional I/O control specifiers (iostat, iomsg, etc.)
            io_control_list = collect_io_control_specifiers( &
                              parser, is_read_io_control_specifier)

            ! Expect closing parenthesis
            token = parser%consume()
            if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
                write (error_unit, *) &
                    "Error: Expected ')' after read unit and format at line ", &
                    token%line
                read_index = 0
                return
            end if
        else
            ! Format: read format, variables (list-directed without unit)
            unit_spec = "*"  ! Default to standard input

            ! Parse format specifier
            call parse_format_specifier(parser, format_spec)
            if (len(format_spec) == 0) format_spec = "*"  ! Default

            ! Skip comma after format spec if present
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            end if
        end if

        ! Parse all read variables
        call parse_argument_list(parser, arena, var_indices)

        ! Create read statement node with parsed variables
        if (allocated(namelist_group)) then
            read_index = push_read_statement(arena, unit_spec, var_indices, &
                                             format_spec=format_spec, &
                                             namelist_group=namelist_group, &
                                             io_control_list=io_control_list, &
                                             line=line, column=column)
        else if (format_expr_index > 0) then
            read_index = push_read_statement(arena, unit_spec, var_indices, &
                                             format_expr_index=format_expr_index, &
                                             io_control_list=io_control_list, &
                                             line=line, column=column)
        else if (allocated(format_spec)) then
            read_index = push_read_statement(arena, unit_spec, var_indices, &
                                             format_spec=format_spec, &
                                             io_control_list=io_control_list, &
                                             line=line, column=column)
        else
            read_index = push_read_statement(arena, unit_spec, var_indices, &
                                             io_control_list=io_control_list, &
                                             line=line, column=column)
        end if
    end function parse_read_statement

    pure logical function is_format_specifier_keyword(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
            is_format_specifier_keyword = .false.
            return
        end if

        if (.not. allocated(token%text)) then
            is_format_specifier_keyword = .false.
            return
        end if

        lowered = to_lower(token%text)
        lowered = trim(lowered)
        is_format_specifier_keyword = &
            (lowered == "fmt" .or. lowered == "format")
    end function is_format_specifier_keyword

    pure logical function is_write_io_control_specifier(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
            is_write_io_control_specifier = .false.
            return
        end if

        if (.not. allocated(token%text)) then
            is_write_io_control_specifier = .false.
            return
        end if

        lowered = to_lower(token%text)
        lowered = trim(lowered)
        is_write_io_control_specifier = &
            (lowered == "iostat" .or. lowered == "iomsg" .or. &
             lowered == "err" .or. lowered == "advance" .or. &
             lowered == "asynchronous" .or. lowered == "decimal" .or. &
             lowered == "delim" .or. lowered == "id" .or. &
             lowered == "pos" .or. lowered == "rec" .or. &
             lowered == "round" .or. lowered == "sign")
    end function is_write_io_control_specifier

    pure logical function is_read_io_control_specifier(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
            is_read_io_control_specifier = .false.
            return
        end if

        if (.not. allocated(token%text)) then
            is_read_io_control_specifier = .false.
            return
        end if

        lowered = to_lower(token%text)
        lowered = trim(lowered)
        is_read_io_control_specifier = &
            (lowered == "iostat" .or. lowered == "iomsg" .or. &
             lowered == "err" .or. lowered == "end" .or. &
             lowered == "eor" .or. lowered == "size" .or. &
             lowered == "advance" .or. lowered == "asynchronous" .or. &
             lowered == "blank" .or. lowered == "decimal" .or. &
             lowered == "delim" .or. lowered == "id" .or. &
             lowered == "pad" .or. lowered == "pos" .or. &
             lowered == "rec" .or. lowered == "round" .or. &
             lowered == "sign")
    end function is_read_io_control_specifier

    function collect_io_control_specifiers(parser, is_control_specifier) &
        result(spec_text)
        type(parser_state_t), intent(inout) :: parser
        procedure(io_control_specifier_predicate) :: is_control_specifier
        character(len=:), allocatable :: spec_text
        type(token_t) :: token
        integer :: paren_depth
        logical :: found_specifier

        spec_text = ""

        do while (.not. parser%is_at_end())
            call skip_io_trivia(parser)
            token = parser%peek()

            if (token%kind == TK_OPERATOR .and. token%text == ")") exit

            found_specifier = .false.

            if (is_control_specifier(token)) then
                found_specifier = .true.
            else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                if (len(spec_text) > 0) spec_text = spec_text // ", "
                token = parser%consume()
                call skip_io_trivia(parser)
                token = parser%peek()
                if (is_control_specifier(token)) then
                    found_specifier = .true.
                else
                    exit
                end if
            else
                exit
            end if

            if (found_specifier) then
                spec_text = spec_text // token%text
                token = parser%consume()
                call skip_io_trivia(parser)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    spec_text = spec_text // "="
                    token = parser%consume()
                    call skip_io_trivia(parser)

                    paren_depth = 0
                    do while (.not. parser%is_at_end())
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            paren_depth = paren_depth + 1
                            spec_text = spec_text // token%text
                            token = parser%consume()
                        else if (token%kind == TK_OPERATOR .and. &
                                 token%text == ")") then
                            if (paren_depth == 0) exit
                            paren_depth = paren_depth - 1
                            spec_text = spec_text // token%text
                            token = parser%consume()
                        else if (token%kind == TK_OPERATOR .and. &
                                 token%text == ",") then
                            if (paren_depth == 0) exit
                            spec_text = spec_text // token%text
                            token = parser%consume()
                        else if (token%kind == TK_NEWLINE) then
                            exit
                        else
                            spec_text = spec_text // token%text
                            token = parser%consume()
                        end if
                    end do
                else
                    exit
                end if
            end if
        end do
    end function collect_io_control_specifiers

    function parse_format_statement(parser, arena) result(format_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: format_index

        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: format_spec
        integer :: paren_depth

        token = parser%peek()
        line = token%line
        column = token%column

        if (token%kind /= TK_KEYWORD .or. token%text /= "format") then
            format_index = 0
            return
        end if

        token = parser%consume()

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            write (error_unit, *) &
                "Error: Expected '(' after 'format' at line ", token%line
            format_index = 0
            return
        end if

        format_spec = "("
        token = parser%consume()
        paren_depth = 1

        do while (.not. parser%is_at_end() .and. paren_depth > 0)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                paren_depth = paren_depth + 1
                format_spec = format_spec // "("
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                paren_depth = paren_depth - 1
                format_spec = format_spec // ")"
                token = parser%consume()
                if (paren_depth == 0) exit
            else if (token%kind == TK_NEWLINE) then
                exit
            else if (token%kind == TK_COMMENT) then
                format_spec = format_spec // trim(token%text)
                call update_format_paren_depth_from_text(token%text, &
                    paren_depth)
                token = parser%consume()
                if (paren_depth == 0) exit
            else if (token%kind == TK_STRING .and. &
                     is_hollerith_token(token%text)) then
                format_spec = format_spec // hollerith_to_char_literal(token%text)
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == "!") then
                token = parser%consume()
            else
                format_spec = format_spec // trim(token%text)
                token = parser%consume()
            end if
        end do

        format_index = push_format_statement(arena, format_spec, line, column)
    end function parse_format_statement

    subroutine update_format_paren_depth_from_text(text, paren_depth)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: paren_depth
        integer :: i

        do i = 1, len_trim(text)
            select case (text(i:i))
            case ("(")
                paren_depth = paren_depth + 1
            case (")")
                paren_depth = paren_depth - 1
            end select
        end do
    end subroutine update_format_paren_depth_from_text

    logical function is_hollerith_token(text) result(is_hollerith)
        character(len=*), intent(in) :: text
        integer :: i

        is_hollerith = .false.
        if (len_trim(text) < 2) return

        i = 1
        do while (i <= len(text))
            select case (text(i:i))
            case ("0":"9")
                i = i + 1
            case ("H", "h")
                is_hollerith = (i > 1)
                return
            case default
                return
            end select
        end do
    end function is_hollerith_token

    function hollerith_to_char_literal(text) result(literal)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: literal
        character(len=:), allocatable :: digits, payload
        integer :: h_pos, count, stat, last_pos

        h_pos = index(to_lower(text), "h")
        if (h_pos <= 1) then
            literal = trim(text)
            return
        end if

        digits = text(:h_pos - 1)
        read (digits, *, iostat=stat) count
        if (stat /= 0 .or. count < 0) then
            literal = trim(text)
            return
        end if

        last_pos = min(len(text), h_pos + count)
        if (last_pos < h_pos + 1) then
            payload = ""
        else
            payload = text(h_pos + 1:last_pos)
        end if
        literal = quote_fortran_char_literal(payload)
    end function hollerith_to_char_literal

    function quote_fortran_char_literal(text) result(literal)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: literal
        integer :: i

        literal = "'"
        do i = 1, len(text)
            if (text(i:i) == "'") then
                literal = literal // "''"
            else
                literal = literal // text(i:i)
            end if
        end do
        literal = literal // "'"
    end function quote_fortran_char_literal

    function parse_open_statement(parser, arena) result(open_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: open_index
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: spec_text
        character(len=:), allocatable :: lowered

        token = parser%peek()
        lowered = trim(to_lower(token%text))
        if (token%kind /= TK_KEYWORD .or. lowered /= "open") then
            open_index = 0
            return
        end if
        line = token%line
        column = token%column
        token = parser%consume()

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            open_index = 0
            return
        end if
        token = parser%consume()

        spec_text = ""
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                if (len(spec_text) > 0) spec_text = spec_text // ", "
                token = parser%consume()
            else if (token%kind == TK_NEWLINE) then
                exit
            else
                ! Only add space if not immediately after = AND if token is not &
                ! a string literal
                if (len(spec_text) > 0 .and. token%text /= "=" .and. token%kind /= &
                    TK_STRING) then
                    if (spec_text(len(spec_text):len(spec_text)) /= '=') then
                        spec_text = spec_text // " "
                    end if
                end if
                spec_text = spec_text // token%text
                token = parser%consume()
            end if
        end do

        open_index = push_open_statement(arena, spec_text, line, column)
    end function parse_open_statement

    function parse_close_statement(parser, arena) result(close_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: close_index
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: spec_text
        character(len=:), allocatable :: lowered

        token = parser%peek()
        lowered = trim(to_lower(token%text))
        if (token%kind /= TK_KEYWORD .or. lowered /= "close") then
            close_index = 0
            return
        end if
        line = token%line
        column = token%column
        token = parser%consume()

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            close_index = 0
            return
        end if
        token = parser%consume()

        spec_text = ""
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            ! Only add space if not immediately after = AND if token is not &
            ! a string literal
            if (len(spec_text) > 0 .and. token%text /= "," .and. token%kind /= &
                TK_STRING) then
                if (spec_text(len(spec_text):len(spec_text)) /= '=') then
                    spec_text = spec_text // " "
                end if
            end if
            spec_text = spec_text // token%text
            token = parser%consume()
        end do

        close_index = push_close_statement(arena, spec_text, line, column)
    end function parse_close_statement

    function parse_inquire_statement(parser, arena) result(inquire_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: inquire_index
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: spec_text
        character(len=:), allocatable :: lowered

        token = parser%peek()
        lowered = trim(to_lower(token%text))
        if (token%kind /= TK_KEYWORD .or. lowered /= "inquire") then
            inquire_index = 0
            return
        end if
        line = token%line
        column = token%column
        token = parser%consume()

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            inquire_index = 0
            return
        end if
        token = parser%consume()

        spec_text = ""
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            ! Only add space if not immediately after = AND if token is not &
            ! a string literal
            if (len(spec_text) > 0 .and. token%text /= "," .and. token%kind /= &
                TK_STRING) then
                if (spec_text(len(spec_text):len(spec_text)) /= '=') then
                    spec_text = spec_text // " "
                end if
            end if
            spec_text = spec_text // token%text
            token = parser%consume()
        end do

        inquire_index = push_inquire_statement(arena, spec_text, line, column)
    end function parse_inquire_statement

    function parse_backspace_statement(parser, arena) result(backspace_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: backspace_index
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: unit_spec
        character(len=:), allocatable :: lowered

        token = parser%peek()
        lowered = trim(to_lower(token%text))
        if (token%kind /= TK_KEYWORD .or. lowered /= "backspace") then
            backspace_index = 0
            return
        end if
        line = token%line
        column = token%column
        token = parser%consume()

        unit_spec = collect_position_spec(parser)
        unit_spec = trim(unit_spec)
        if (len(unit_spec) == 0) then
            backspace_index = 0
            return
        end if
        backspace_index = push_backspace_statement(arena, unit_spec, line, column)
    end function parse_backspace_statement

    function parse_rewind_statement(parser, arena) result(rewind_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: rewind_index
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: unit_spec
        character(len=:), allocatable :: lowered

        token = parser%peek()
        lowered = trim(to_lower(token%text))
        if (token%kind /= TK_KEYWORD .or. lowered /= "rewind") then
            rewind_index = 0
            return
        end if
        line = token%line
        column = token%column
        token = parser%consume()

        unit_spec = collect_position_spec(parser)
        unit_spec = trim(unit_spec)
        if (len(unit_spec) == 0) then
            rewind_index = 0
            return
        end if
        rewind_index = push_rewind_statement(arena, unit_spec, line, column)
    end function parse_rewind_statement

    function parse_endfile_statement(parser, arena) result(endfile_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: endfile_index
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: unit_spec
        character(len=:), allocatable :: lowered

        token = parser%peek()
        lowered = trim(to_lower(token%text))
        if (token%kind /= TK_KEYWORD .or. lowered /= "endfile") then
            endfile_index = 0
            return
        end if
        line = token%line
        column = token%column
        token = parser%consume()

        unit_spec = collect_position_spec(parser)
        unit_spec = trim(unit_spec)
        if (len(unit_spec) == 0) then
            endfile_index = 0
            return
        end if
        endfile_index = push_endfile_statement(arena, unit_spec, line, column)
    end function parse_endfile_statement
