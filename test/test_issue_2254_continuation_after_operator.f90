program test_issue_2254_continuation_after_operator
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_continuation_after_operator()
    print *, ""
    print *, "Issue 2254 free-form continuation tests completed."

contains

    subroutine verify_continuation_after_operator()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example('examples/f90/issue_2254_continuation_after_operator.f90', &
                          input_code)

        print *, ""
        print *, "Test: free-form continuation after operator"
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

        ! Verify the output contains the expected assignment
        ! The continuation should be normalized so x = 1 + 2 (or similar)
        if (index(output_code, "x") == 0) then
            print *, "FAIL: Variable 'x' missing from output"
            error stop 1
        end if

        ! Verify the expression is present (either inline or split)
        if (index(output_code, "1") == 0 .or. index(output_code, "2") == 0) then
            print *, "FAIL: Expression components missing from output"
            error stop 1
        end if

        ! Verify print statement is present
        if (index(output_code, "print") == 0) then
            print *, "FAIL: Print statement missing from output"
            error stop 1
        end if

        print *, "[PASS] Free-form continuation after operator accepted and round-tripped"
    end subroutine verify_continuation_after_operator

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2254_continuation_after_operator
