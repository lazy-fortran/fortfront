program test_issue_1744_operator_interface
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_operator_interface()
    call test_assignment_interface()
    print *, ""
    print *, "Issue 1744 operator interface tests completed."

contains

    subroutine test_operator_interface()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = "module vector_ops" // nl // &
                     "    implicit none" // nl // &
                     "" // nl // &
                     "    type :: vec3" // nl // &
                     "        real :: x, y, z" // nl // &
                     "    end type vec3" // nl // &
                     "" // nl // &
                     "    interface operator(+)" // nl // &
                     "        module procedure vec3_add" // nl // &
                     "    end interface" // nl // &
                     "" // nl // &
                     "end module vector_ops"

        print *, ""
        print *, "Test: Interface operator(+) preserved"
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

        if (index(output_code, "interface operator(+)") == 0) then
            print *, "FAIL: interface operator(+) missing from output"
            error stop 1
        end if

        if (index(output_code, "end interface operator(+)") == 0) then
            print *, "FAIL: end interface operator(+) missing from output"
            error stop 1
        end if

        if (index(output_code, "module procedure vec3_add") == 0) then
            print *, "FAIL: module procedure statement missing"
            error stop 1
        end if

        print *, "[PASS] Interface operator(+) preserved correctly"
    end subroutine test_operator_interface

    subroutine test_assignment_interface()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = "module string_ops" // nl // &
                     "    implicit none" // nl // &
                     "" // nl // &
                     "    interface assignment(=)" // nl // &
                     "        module procedure assign_string" // nl // &
                     "    end interface" // nl // &
                     "" // nl // &
                     "end module string_ops"

        print *, ""
        print *, "Test: Interface assignment(=) preserved"
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

        if (index(output_code, "interface assignment(=)") == 0) then
            print *, "FAIL: interface assignment(=) missing from output"
            error stop 1
        end if

        if (index(output_code, "end interface assignment(=)") == 0) then
            print *, "FAIL: end interface assignment(=) missing from output"
            error stop 1
        end if

        if (index(output_code, "module procedure assign_string") == 0) then
            print *, "FAIL: module procedure statement missing"
            error stop 1
        end if

        print *, "[PASS] Interface assignment(=) preserved correctly"
    end subroutine test_assignment_interface

end program test_issue_1744_operator_interface
