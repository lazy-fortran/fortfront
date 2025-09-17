module parser_io_statements_module
    ! Parser module for I/O statement types (print, write, read)
    use iso_fortran_env, only: error_unit
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_comparison
    use ast_core
    use ast_factory
    implicit none
    private

    public :: parse_print_statement, parse_write_statement, parse_read_statement

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

    ! Parse argument list (common logic for print/write/read)
    subroutine parse_argument_list(parser, arena, arg_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: current_arg_index

        allocate(arg_indices(0))

        if (.not. parser%is_at_end()) then
            ! Parse first argument
            current_arg_index = parse_comparison(parser, arena)
            if (current_arg_index > 0) then
                arg_indices = [current_arg_index]

                ! Parse additional arguments separated by commas
                do
                    token = parser%peek()
                    if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit

                    token = parser%consume() ! consume comma
                    current_arg_index = parse_comparison(parser, arena)
                    if (current_arg_index > 0) then
                        arg_indices = [arg_indices, current_arg_index]
                    else
                        exit
                    end if
                end do
            end if
        end if
    end subroutine parse_argument_list

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
        if (len(format_spec) == 0) format_spec = "*" ! Default

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
        character(len=:), allocatable :: unit_spec, format_spec

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
            write(error_unit, *) "Error: Expected '(' after 'write' at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Parse unit specifier
        unit_spec = parse_unit_specifier(parser)
        if (len(unit_spec) == 0) then
            write(error_unit, *) &
                "Error: Expected unit specifier in write statement at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Check for format specifier (optional)
        format_spec = ""
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume() ! consume comma
            call parse_format_specifier(parser, format_spec)
        end if

        ! Expect closing parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
            write(error_unit, *) &
                "Error: Expected ')' after write unit and format at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Parse all write arguments
        call parse_argument_list(parser, arena, arg_indices)

        ! Create write statement node with parsed arguments        
        write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                           format_spec, line, column)
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

        ! Expect opening parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            write(error_unit, *) "Error: Expected '(' after 'read' at line ", &
                token%line
            read_index = 0
            return
        end if

        ! Parse unit specifier
        unit_spec = parse_unit_specifier(parser)
        if (len(unit_spec) == 0) then
            write(error_unit, *) &
                "Error: Expected unit specifier in read statement at line ", &
                token%line
            read_index = 0
            return
        end if

        ! Check for format specifier (optional)
        format_spec = ""
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume() ! consume comma
            call parse_format_specifier(parser, format_spec)
        end if

        ! Expect closing parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
            write(error_unit, *) &
                "Error: Expected ')' after read unit and format at line ", &
                token%line
            read_index = 0
            return
        end if

        ! Parse all read variables
        call parse_argument_list(parser, arena, var_indices)

        ! Create read statement node with parsed variables
        read_index = push_read_statement(arena, unit_spec, var_indices, &
                                         format_spec, line, column)
    end function parse_read_statement

end module parser_io_statements_module