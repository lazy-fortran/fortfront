program test_issue_1737_generic_interface
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_generic_interface_in_program()
    print *, ""
    print *, "Issue 1737: Generic interface tests completed."

contains

    subroutine test_generic_interface_in_program()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = "program test_interface_generic" // nl // &
                     "    implicit none" // nl // &
                     "" // nl // &
                     "    interface swap" // nl // &
                     "        module procedure swap_int, swap_real" // nl // &
                     "    end interface swap" // nl // &
                     "" // nl // &
                     "    integer :: a, b" // nl // &
                     "    real :: x, y" // nl // &
                     "" // nl // &
                     "    a = 5" // nl // &
                     "    b = 10" // nl // &
                     "    call swap(a, b)" // nl // &
                     "" // nl // &
                     "    x = 1.5" // nl // &
                     "    y = 2.5" // nl // &
                     "    call swap(x, y)" // nl // &
                     "" // nl // &
                     "contains" // nl // &
                     "" // nl // &
                     "    subroutine swap_int(p, q)" // nl // &
                     "        integer, intent(inout) :: p, q" // nl // &
                     "        integer :: temp" // nl // &
                     "        temp = p" // nl // &
                     "        p = q" // nl // &
                     "        q = temp" // nl // &
                     "    end subroutine swap_int" // nl // &
                     "" // nl // &
                     "    subroutine swap_real(p, q)" // nl // &
                     "        real, intent(inout) :: p, q" // nl // &
                     "        real :: temp" // nl // &
                     "        temp = p" // nl // &
                     "        p = q" // nl // &
                     "        q = temp" // nl // &
                     "    end subroutine swap_real" // nl // &
                     "" // nl // &
                     "end program test_interface_generic"

        print *, ""
        print *, "Test: Generic interface in program with internal procedures"
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

        if (index(output_code, "module procedure") /= 0) then
            print *, "FAIL: module procedure should not appear in program output"
            error stop 1
        end if

        if (index(output_code, "procedure :: swap_int") == 0) then
            print *, "FAIL: swap_int should be declared with procedure in interface"
            error stop 1
        end if

        if (index(output_code, "procedure :: swap_real") == 0 .and. &
            index(output_code, "procedure :: swap_int, swap_real") == 0) then
            print *, "FAIL: swap_real should be declared with procedure in interface"
            error stop 1
        end if

        if (index(output_code, "subroutine swap_int") == 0) then
            print *, "FAIL: swap_int subroutine missing from output"
            error stop 1
        end if

        if (index(output_code, "subroutine swap_real") == 0) then
            print *, "FAIL: swap_real subroutine missing from output"
            error stop 1
        end if

        print *, "[PASS] Generic interface with internal procedures"
    end subroutine test_generic_interface_in_program

end program test_issue_1737_generic_interface
