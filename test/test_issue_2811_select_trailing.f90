program test_issue_2811_select_trailing
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_trailing_is_sibling( &
        'examples/f90/issue_2811_select_type_trailing.f90', 'SELECT TYPE')
    call verify_trailing_is_sibling( &
        'examples/f90/issue_2811_select_rank_trailing.f90', 'SELECT RANK')

    print *, ""
    print *, "Issue 2811 select-trailing tests completed."

contains

    include 'common/read_example.inc'

    subroutine verify_trailing_is_sibling(example_path, label)
        character(len=*), intent(in) :: example_path
        character(len=*), intent(in) :: label
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        integer :: end_select_pos, print_pos

        call read_example(example_path, input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') label // ' lexing error: ' // &
                    trim(error_msg)
                error stop 1
            end if
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') label // ' parsing error: ' // &
                    trim(error_msg)
                error stop 1
            end if
        end if

        call emit_fortran(arena, prog_index, output_code)

        print_pos = index(output_code, 'print *, i')
        end_select_pos = index(output_code, 'end select')

        if (print_pos == 0) then
            write (error_unit, '(A)') label // &
                ' FAIL: trailing print missing from emitted code'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if
        if (end_select_pos == 0) then
            write (error_unit, '(A)') label // &
                ' FAIL: end select missing from emitted code'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        ! Trailing statement is a sibling of the select node only if it is
        ! emitted after the construct closes. Absorption into the last arm
        ! would place the print before end select.
        if (print_pos < end_select_pos) then
            write (error_unit, '(A)') label // &
                ' FAIL: trailing print absorbed into last arm (before end select)'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        print *, '[PASS] ' // label // ': trailing statement is a sibling'
    end subroutine verify_trailing_is_sibling

end program test_issue_2811_select_trailing
