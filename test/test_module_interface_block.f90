program test_module_interface_block
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_procedure_definitions_module, only: parse_interface_block
    implicit none

    call test_module_with_interface_block()
    call test_interface_block_reports_unexpected_token()
    print *, ""
    print *, "Interface block parser tests completed."

contains

    subroutine test_module_with_interface_block()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = "module math_interface" // nl // &
                     "    implicit none" // nl // &
                     "" // nl // &
                     "    interface add" // nl // &
                     "        module procedure add_int, add_real" // nl // &
                     "    end interface add" // nl // &
                     "" // nl // &
                     "contains" // nl // &
                     "" // nl // &
                     "    function add_int(a, b) result(c)" // nl // &
                     "        integer, intent(in) :: a, b" // nl // &
                     "        integer :: c" // nl // &
                     "        c = a + b" // nl // &
                     "    end function add_int" // nl // &
                     "" // nl // &
                     "    function add_real(a, b) result(c)" // nl // &
                     "        real, intent(in) :: a, b" // nl // &
                     "        real :: c" // nl // &
                     "        c = a + b" // nl // &
                     "    end function add_real" // nl // &
                     "" // nl // &
                     "end module math_interface"

        print *, ""
        print *, "Test: Module with interface block"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, trim(output_code)

        if (index(output_code, "interface add") == 0) then
            print *, "FAIL: Interface header missing from output"
            error stop 1
        end if

        if (index(output_code, "module procedure add_int, add_real") == 0) then
            print *, "FAIL: Module procedure list missing from interface body"
            error stop 1
        end if

        if (index(output_code, "end interface add") == 0) then
            print *, "FAIL: Interface end statement missing"
            error stop 1
        end if

        if (index(output_code, "function add_int") == 0) then
            print *, "FAIL: add_int function missing from output"
            error stop 1
        end if

        if (index(output_code, "function add_real") == 0) then
            print *, "FAIL: add_real function missing from output"
            error stop 1
        end if

        print *, "[PASS] Module with interface block"
    end subroutine test_module_with_interface_block

    subroutine test_interface_block_reports_unexpected_token()
        character(:), allocatable :: input_code
        character(:), allocatable :: lex_error
        character(:), allocatable :: parser_error
        character(:), allocatable :: lower_error
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(parser_prefix_buffer_t) :: prefix_buffer
        integer :: interface_index
        character(len=1), parameter :: nl = new_line('A')
        character(len=*), parameter :: expected_fragment = &
            "unexpected token 'unexpected_token' in interface block."

        input_code = "interface add" // nl // &
                     "    unexpected_token" // nl // &
                     "end interface add"

        print *, ""
        print *, "Test: Interface block reports unexpected token"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, lex_error)

        if (allocated(lex_error) .and. len_trim(lex_error) > 0) then
            print *, "Lexing error: ", trim(lex_error)
            error stop 1
        end if

        parser = create_parser_state(tokens)
        call prefix_buffer%clear()
        interface_index = parse_interface_block(parser, arena, prefix_buffer)
        if (interface_index <= 0) then
            print *, "FAIL: Interface block parsing returned invalid index"
            error stop 1
        end if

        if (.not. parser%has_errors()) then
            print *, "FAIL: Parser did not report unexpected token inside interface block"
            error stop 1
        end if

        parser_error = parser%get_error_messages()
        lower_error = to_lower_string(parser_error)
        if (index(lower_error, expected_fragment) == 0) then
            print *, "FAIL: Parser error message missing expected fragment"
            print *, "Actual:"
            print *, trim(parser_error)
            error stop 1
        end if

        print *, "[PASS] Interface block unexpected token error surfaced"
    end subroutine test_interface_block_reports_unexpected_token

    pure function to_lower_string(value) result(lower_value)
        character(len=*), intent(in) :: value
        character(len=len(value)) :: lower_value
        integer :: i, code

        lower_value = value
        do i = 1, len(lower_value)
            code = iachar(lower_value(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lower_value(i:i) = achar(code + 32)
            end if
        end do
    end function to_lower_string

end program test_module_interface_block
