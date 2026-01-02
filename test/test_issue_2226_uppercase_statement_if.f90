program test_issue_2226_uppercase_statement_if
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_uppercase_statement_if()
    print *, ""
    print *, "Issue 2226 uppercase statement IF tests completed."

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine verify_uppercase_statement_if()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example('examples/f90/issue_2226_uppercase_statement_if.f90', &
                          input_code)

        print *, ""
        print *, "Test: uppercase keywords in statement IF are accepted"
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

        ! Verify the uppercase statement IF constructs are present
        if (index(output_code, "stop 99") == 0 .and. index(output_code, "STOP 99") == 0) then
            print *, "FAIL: STOP statement missing from output"
            error stop 1
        end if

        if (index(output_code, "call emit") == 0 .and. index(output_code, "CALL emit") == 0) then
            print *, "FAIL: CALL statement missing from output"
            error stop 1
        end if

        if (index(output_code, "write") == 0 .and. index(output_code, "WRITE") == 0) then
            print *, "FAIL: WRITE statement missing from output"
            error stop 1
        end if

        print *, "[PASS] Uppercase keywords in statement IF accepted"
    end subroutine verify_uppercase_statement_if


end program test_issue_2226_uppercase_statement_if
