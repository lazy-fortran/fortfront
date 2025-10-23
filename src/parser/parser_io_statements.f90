module parser_io_statements_module
    ! Parser module for I/O statement types (print, write, read)
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE, to_lower
    use parser_state_module
    use parser_expressions_module, only: parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_io, only: open_statement_node, close_statement_node, &
                            inquire_statement_node, backspace_statement_node, &
                            rewind_statement_node, endfile_statement_node
    use ast_factory, only: push_print_statement, push_write_statement, &
                           push_read_statement, push_format_statement, &
                           push_open_statement, push_close_statement, &
                           push_inquire_statement, push_backspace_statement, &
                           push_rewind_statement, push_endfile_statement
    use ast_factory
    implicit none
    private

    public :: parse_print_statement, parse_write_statement, parse_read_statement
    public :: parse_format_statement
    public :: parse_open_statement, parse_close_statement
    public :: parse_inquire_statement
    public :: parse_backspace_statement, parse_rewind_statement, parse_endfile_statement

contains

    ! Parse format specifier (common logic for write/read)
    subroutine parse_format_specifier(parser, format_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: format_spec
        type(token_t) :: token

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            format_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_STRING) then
            format_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            format_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            format_spec = token%text
            token = parser%consume()
        else
            format_spec = ""
        end if
    end subroutine parse_format_specifier

    ! Parse unit specifier (common logic for write/read)
    function parse_unit_specifier(parser) result(unit_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable :: unit_spec
        type(token_t) :: token

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
                    if (last_char == '=') needs_space = .true.
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

    ! Parse argument list (common logic for print/write/read)
    subroutine parse_argument_list(parser, arena, arg_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: current_arg_index

        allocate (arg_indices(0))

        if (.not. parser%is_at_end()) then
            ! Parse first argument
            current_arg_index = parse_io_implied_do(parser, arena)
            if (current_arg_index == 0) current_arg_index = &
                parse_comparison(parser, arena)
            if (current_arg_index > 0) then
                arg_indices = [current_arg_index]

                ! Parse additional arguments separated by commas
                do
                    token = parser%peek()
                    if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit

                    token = parser%consume()  ! consume comma
                    current_arg_index = parse_io_implied_do(parser, arena)
                    if (current_arg_index == 0) current_arg_index = &
                        parse_comparison(parser, arena)
                    if (current_arg_index > 0) then
                        arg_indices = [arg_indices, current_arg_index]
                    else
                        exit
                    end if
                end do
            end if
        end if
    end subroutine parse_argument_list

    ! Parse implied-do expression specific to I/O lists: (expr, var = start, end [, step])
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
        token = parser%consume()  ! consume '('

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

        start_index = parse_comparison(parser, arena)
        if (start_index <= 0) then
            expr_index = fail_and_restore()
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ",") then
            expr_index = fail_and_restore()
            return
        end if
        token = parser%consume()

        end_index = parse_comparison(parser, arena)
        if (end_index <= 0) then
            expr_index = fail_and_restore()
            return
        end if

        step_index = 0
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
            step_index = parse_comparison(parser, arena)
            if (step_index <= 0) then
                expr_index = fail_and_restore()
                return
            end if
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
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

    function parse_print_statement(parser, arena) result(print_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: print_index

        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        character(len=:), allocatable :: format_spec

        ! Consume 'print' keyword
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

        ! Check if we're at write keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "write") then
            ! Consume 'write' keyword
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
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()  ! consume comma

            ! Check if next token is 'nml' keyword
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. token%text == "nml") then
                token = parser%consume()  ! consume 'nml'

                ! Expect '=' after 'nml'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume '='

                    ! Parse namelist group name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        namelist_group = token%text
                        token = parser%consume()
                    else
                        write (error_unit, *) &
                            "Error: Expected namelist group name after 'nml=' at line ", &
                            token%line
                    end if
                end if
            else
                ! Parse format specifier
                call parse_format_specifier(parser, format_spec)
                if (allocated(format_spec)) then
                    if (len(format_spec) == 0) deallocate (format_spec)
                end if
            end if
        end if

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
                                               line=line, column=column)
        else if (allocated(format_spec)) then
            write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                               format_spec=format_spec, &
                                               line=line, column=column)
        else
            write_index = push_write_statement(arena, unit_spec, arg_indices, &
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
        character(len=:), allocatable :: unit_spec, format_spec

        ! Consume 'read' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Check for opening parenthesis (determines read format)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            ! Format: read (unit, format) variables
            token = parser%consume()  ! consume '('

            ! Parse unit specifier
            unit_spec = parse_unit_specifier(parser)
            if (len(unit_spec) == 0) then
                write (error_unit, *) &
                    "Error: Expected unit specifier in read statement at line ", &
                    token%line
                read_index = 0
                return
            end if

            ! Check for format specifier (optional)
            format_spec = ""
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume comma
                call parse_format_specifier(parser, format_spec)
            end if

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
        read_index = push_read_statement(arena, unit_spec, var_indices, &
                                         format_spec, line, column)
    end function parse_read_statement

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
            else
                format_spec = format_spec // trim(token%text)
                token = parser%consume()
            end if
        end do

        format_index = push_format_statement(arena, format_spec, line, column)
    end function parse_format_statement

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
                if (len(spec_text) > 0 .and. token%text /= "=") spec_text = spec_text // " "
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

            if (len(spec_text) > 0 .and. token%text /= ",") spec_text = spec_text // " "
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

            if (len(spec_text) > 0 .and. token%text /= ",") then
                spec_text = spec_text // " "
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

end module parser_io_statements_module
