program test_issue_1861_nested_do_print
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_print_preserved()
    print *, ""
    print *, "Issue 1861 nested DO print tests completed."

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine verify_print_preserved()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example('examples/f90/issue_1861_nested_do_print.f90', &
                          input_code)

        print *, ""
        print *, "Test: print survives nested DO loops with whitespace"
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

        if (index(output_code, "print *, matrix") == 0) then
            print *, "FAIL: print statement missing from emitted code"
            error stop 1
        end if

        print *, "[PASS] Print statement preserved after nested DO loops"
    end subroutine verify_print_preserved


end program test_issue_1861_nested_do_print
