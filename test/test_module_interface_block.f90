program test_module_interface_block
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_module_with_interface_block()
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

end program test_module_interface_block
