program test_issue_2254_free_form_continuation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call exercise_free_form_continuation()
    print *, 'PASS: Issue 2254 free-form continuation preserved'

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'


    subroutine exercise_free_form_continuation()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example('examples/f90/issue_2254_free_form_continuation.f90', &
                          source)

        arena = create_ast_arena()

        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'Lex failure: ' // trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'Parse failure: ' // trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output)
        if (index(output, 'x = 3') == 0 .and. index(output, 'x = 1 + 2') == 0) then
            write (error_unit, '(A)') 'Round-trip lost arithmetic expression'
            error stop 1
        end if

        if (index(output, 'print *, x') == 0) then
            write (error_unit, '(A)') 'Output missing print statement'
            error stop 1
        end if
    end subroutine exercise_free_form_continuation

end program test_issue_2254_free_form_continuation
