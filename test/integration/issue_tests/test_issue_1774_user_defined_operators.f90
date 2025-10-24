program test_issue_1774_user_defined_operators
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_user_defined_operator_preserved()
    print *, ""
    print *, "Issue 1774 user-defined operator test completed."

contains

    subroutine test_user_defined_operator_preserved()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = "module operator_mod" // nl // &
                     "    use, intrinsic :: iso_fortran_env," // &
                     " only: dp => real64" // nl // &
                     "    implicit none" // nl // &
                     "    type :: vector" // nl // &
                     "        real(dp) :: x, y" // nl // &
                     "    end type vector" // nl // &
                     "    " // nl // &
                     "    interface operator(.dot.)" // nl // &
                     "        module procedure dot_product_vec" // nl // &
                     "    end interface" // nl // &
                     "    " // nl // &
                     "contains" // nl // &
                     "    real(dp) function dot_product_vec(a, b)" // nl // &
                     "        type(vector), intent(in) :: a, b" // nl // &
                     "        dot_product_vec = a%x * b%x + a%y * b%y" // nl // &
                     "    end function dot_product_vec" // nl // &
                     "end module operator_mod" // nl // &
                     "" // nl // &
                     "program test_operator_defined" // nl // &
                     "    use operator_mod" // nl // &
                     "    implicit none" // nl // &
                     "    type(vector) :: v1, v2" // nl // &
                     "    real(dp) :: result" // nl // &
                     "    " // nl // &
                     "    v1 = vector(1.0_dp, 2.0_dp)" // nl // &
                     "    v2 = vector(3.0_dp, 4.0_dp)" // nl // &
                     "    result = v1 .dot. v2" // nl // &
                     "    print *, 'Dot product:', result" // nl // &
                     "end program test_operator_defined"

        print *, ""
        print *, "Test: User-defined operator .dot. preserved in expression"
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

        if (index(output_code, ".dot.") == 0) then
            print *, "FAIL: user-defined operator .dot. was removed"
            print *, "Output:"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, "result = v1 .dot. v2") == 0) then
            print *, "FAIL: full operator expression not preserved"
            print *, "Output:"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "[PASS] User-defined operator .dot. preserved correctly"
    end subroutine test_user_defined_operator_preserved

end program test_issue_1774_user_defined_operators
