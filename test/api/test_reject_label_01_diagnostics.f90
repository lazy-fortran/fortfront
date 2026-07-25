program test_reject_label_01_diagnostics
    ! Issue #2889: fortfront must reject invalid statement labels.
    !
    ! Rules covered (Fortran 2023 clauses 6.2.5 and 6.3.2.3):
    !   - a label carries at most five digits
    !   - a label has at least one nonzero digit
    !   - a label must be followed by the statement it labels
    !   - in free source form a blank must separate label and statement
    !
    ! Every negative fixture has a corrected neighbour that must still parse,
    ! so an over-eager check fails this test too.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    integer :: failures

    failures = 0

    call assert_rejected('examples/f90/empty_label.f90', &
        'Statement label without statement')
    call assert_rejected('examples/f90/label_1.f90', &
        'Too many digits in statement label')
    call assert_rejected('examples/f90/label_1.f90', &
        'Zero is not a valid statement label')
    call assert_rejected('examples/f90/label_2.f90', &
        'Invalid character in statement label field')

    call assert_accepted('examples/f90/empty_label_corrected.f90')
    call assert_accepted('examples/f90/label_1_corrected.f90')
    call assert_accepted('examples/f90/label_2_corrected.f90')

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' label check(s) failed'
        error stop 1
    end if
    write (output_unit, '(A)') 'PASS: invalid statement labels rejected'

contains

    subroutine parse_example(filepath, parse_error)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(out) :: parse_error
        character(len=:), allocatable :: source
        character(len=:), allocatable :: lex_error
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        call read_example(filepath, source)
        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)
        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                write (error_unit, '(A)') 'FAIL: lexing error in '//filepath
                error stop 1
            end if
        end if
        call parse_tokens(tokens, arena, root_index, parse_error)
    end subroutine parse_example

    subroutine assert_rejected(filepath, expected_message)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: expected_message
        character(len=5000) :: parse_error

        call parse_example(filepath, parse_error)
        if (index(parse_error, expected_message) > 0) then
            write (output_unit, '(A)') 'PASS: '//filepath//' -> '//expected_message
            return
        end if
        failures = failures + 1
        write (error_unit, '(A)') 'FAIL: '//filepath// &
            ' did not report: '//expected_message
        write (error_unit, '(A)') '  got: '//trim(parse_error)
    end subroutine assert_rejected

    subroutine assert_accepted(filepath)
        character(len=*), intent(in) :: filepath
        character(len=5000) :: parse_error

        call parse_example(filepath, parse_error)
        if (len_trim(parse_error) == 0) then
            write (output_unit, '(A)') 'PASS: '//filepath//' still accepted'
            return
        end if
        failures = failures + 1
        write (error_unit, '(A)') 'FAIL: '//filepath//' was rejected'
        write (error_unit, '(A)') '  got: '//trim(parse_error)
    end subroutine assert_accepted

    include '../common/read_example.inc'

end program test_reject_label_01_diagnostics
