program test_issue_2821_guard_name_decl
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_no_spurious_guard_decl( &
        'examples/f90/issue_2821_select_type_guard_name.f90')

    print *, ""
    print *, "Issue 2821 guard-name declaration test completed."

contains

    include 'common/read_example.inc'

    subroutine verify_no_spurious_guard_decl(example_path)
        character(len=*), intent(in) :: example_path
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example(example_path, input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        call abort_on_error(error_msg, 'lexing')

        call parse_tokens(tokens, arena, prog_index, error_msg)
        call abort_on_error(error_msg, 'parsing')

        call emit_fortran(arena, prog_index, output_code)

        ! The guard type-name "integer" must not be declared as a variable.
        if (index(output_code, 'real :: integer') > 0 .or. &
            index(output_code, ':: integer') > 0) then
            write (error_unit, '(A)') &
                'FAIL: guard type-name emitted as a spurious declaration'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        ! Guard arm must still be present (no regression in select type emission).
        if (index(output_code, 'type is (integer)') == 0) then
            write (error_unit, '(A)') &
                'FAIL: select type guard arm missing from emitted code'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        print *, '[PASS] no spurious guard type-name declaration emitted'
    end subroutine verify_no_spurious_guard_decl

    subroutine abort_on_error(error_msg, stage)
        character(:), allocatable, intent(in) :: error_msg
        character(len=*), intent(in) :: stage

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') stage//' error: '//trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine abort_on_error

end program test_issue_2821_guard_name_decl
