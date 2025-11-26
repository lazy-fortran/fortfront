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

    abstract interface
        pure logical function io_control_specifier_predicate(token)
            import :: token_t
            type(token_t), intent(in) :: token
        end function io_control_specifier_predicate
    end interface

contains

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
                    ! Do not add space after '=' to prevent breaking string literals
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
            parse_comparison(parser, arena)
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
                    parse_comparison(parser, arena)
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
        character(len=:), allocatable :: io_control_list
        logical :: has_keyworded_format

        ! Check if we're at write keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "write") then
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
        has_keyworded_format = .false.
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
                has_keyworded_format = parse_keyworded_format_clause(parser, &
                                                                     format_spec)
                if (.not. has_keyworded_format) then
                    ! Parse positional format specifier
                    call parse_format_specifier(parser, format_spec, &
                                                is_control_specifier= &
                                                is_write_io_control_specifier)
                    if (allocated(format_spec)) then
                        if (len(format_spec) == 0) deallocate (format_spec)
                    end if
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

            ! Check for format specifier or namelist (optional)
            format_spec = ""
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume comma

                ! Check if next token is 'nml' keyword for namelist
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
                                "Error: Expected namelist group name after 'nml=' &
                                &at line ", token%line
                        end if
                    end if
                else if (is_format_specifier_keyword(token)) then
                    ! Check if next token is a keyword parameter (e.g., 'fmt=')
                    ! Only consume keyword if followed by '='
                    ! Save position to check for '=' before committing
                    checkpoint = parser
                    token = checkpoint%consume()  ! tentatively consume keyword

                    ! Check if '=' follows
                    token = checkpoint%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        ! It's a keyworded format (fmt= or format=)
                        parser = checkpoint  ! commit keyword consumption
                        token = parser%consume()  ! consume '='
                    end if
                    ! Otherwise, treat as positional format variable
                end if

                if (.not. allocated(namelist_group)) then
                    call parse_format_specifier(parser, format_spec, &
                                                is_control_specifier= &
                                                is_read_io_control_specifier)
                end if
            end if

        ! Collect any additional I/O control specifiers (iostat, iomsg, etc.)
            io_control_list = collect_io_control_specifiers(parser, &
                                                             is_read_io_control_specifier)

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

    function collect_io_control_specifiers(parser, is_control_specifier) result(spec_text)
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
                ! Only add space if not immediately after '=' AND if token is not &
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

            ! Only add space if not immediately after '=' AND if token is not &
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

            ! Only add space if not immediately after '=' AND if token is not &
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

end module parser_io_statements_module
