program test_issue_1889_one_line_if
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_single_line_if_preserved()
    print *, ""
    print *, "Issue 1889 single-line IF tests completed."

contains

    subroutine verify_single_line_if_preserved()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=1), parameter :: nl = new_line('A')

        input_code = "program single_line_if" // nl // &
                     "    implicit none" // nl // &
                     "    integer :: i, j" // nl // &
                     "" // nl // &
                     "    do i = 1, 5" // nl // &
                     "        if (j == 3) cycle" // nl // &
                     "        if (i == 4) exit" // nl // &
                     "    end do" // nl // &
                     "end program single_line_if"

        print *, ""
        print *, "Test: single-line IF statements stay compact"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error:", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, trim(output_code)

        if (index(output_code, "if (j == 3) cycle") == 0) then
            print *, "FAIL: cycle statement not preserved as single-line IF"
            error stop 1
        end if

        if (index(output_code, "if (j == 3) then") /= 0) then
            print *, "FAIL: cycle statement converted to block IF"
            error stop 1
        end if

        if (index(output_code, "if (i == 4) exit") == 0) then
            print *, "FAIL: exit statement not preserved as single-line IF"
            error stop 1
        end if

        if (index(output_code, "if (i == 4) then") /= 0) then
            print *, "FAIL: exit statement converted to block IF"
            error stop 1
        end if

        print *, "[PASS] Single-line IF statements remain compact"
    end subroutine verify_single_line_if_preserved

end program test_issue_1889_one_line_if
