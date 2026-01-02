program test_issue_1889_one_line_if
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_single_line_if_preserved()
    print *, ""
    print *, "Issue 1889 single-line IF tests completed."

contains

    include 'common/read_example.inc'

    subroutine verify_single_line_if_preserved()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example('examples/f90/issue_1889_single_line_if.f90', &
                          input_code)

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

        if (index(output_code, "if (i == 2) j = j + i") == 0) then
            print *, "FAIL: assignment single-line IF not preserved"
            error stop 1
        end if

        if (index(output_code, "if (i == 2) then") /= 0) then
            print *, "FAIL: assignment IF converted to block form"
            error stop 1
        end if

        if (index(output_code, "if (j > 0) j = j - 1") == 0) then
            print *, "FAIL: trailing single-line IF assignment missing"
            error stop 1
        end if

        if (index(output_code, "if (j > 0) then") /= 0) then
            print *, "FAIL: trailing assignment IF converted to block form"
            error stop 1
        end if

        print *, "[PASS] Single-line IF statements remain compact"
    end subroutine verify_single_line_if_preserved


end program test_issue_1889_one_line_if
