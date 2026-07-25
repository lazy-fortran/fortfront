program test_reject_literal_01_diagnostics
    ! Issue #2894: reject malformed literal forms.
    !
    ! Implemented rule: an unterminated character constant. The negative
    ! fixtures mirror gfortran.dg/unexpected_eof_2.f90 and
    ! gfortran.dg/unexpected_eof_3.f90, whose leading diagnostic is
    ! "Unterminated character constant".
    !
    ! The corrected neighbours pin the two forms that must stay accepted:
    ! a properly closed constant, and a constant continued with '&' across
    ! lines. Without them the check would be free to reject every constant.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use frontend_core, only: lex_source
    use lexer_core, only: token_t
    implicit none

    integer :: failures

    failures = 0

    ! unexpected_eof_2.f90: assignment whose constant is never closed and
    ! whose line does not end in a continuation marker.
    call expect_rejected( &
        'unexpected_eof_2', &
        'program p'//new_line('a')// &
        '   character(8) :: z'//new_line('a')// &
        '   z = ''abc&  ! comment'//new_line('a')// &
        '!end', &
        'Unterminated character constant')

    ! unexpected_eof_3.f90: same defect in an initialiser.
    call expect_rejected( &
        'unexpected_eof_3', &
        'program p'//new_line('a')// &
        '   character(8) :: z = ''abc& ! comment'//new_line('a')// &
        '!end', &
        'Unterminated character constant')

    ! A constant left open at end of file, with no line following at all.
    call expect_rejected( &
        'unterminated at end of file', &
        'program p'//new_line('a')// &
        '   character(8) :: z'//new_line('a')// &
        '   z = "abc', &
        'Unterminated character constant')

    ! Corrected neighbour: the same statements with the constant closed.
    call expect_accepted( &
        'unexpected_eof_2 corrected', &
        'program p'//new_line('a')// &
        '   character(8) :: z'//new_line('a')// &
        '   z = ''abc''  ! comment'//new_line('a')// &
        'end')

    call expect_accepted( &
        'unexpected_eof_3 corrected', &
        'program p'//new_line('a')// &
        '   character(8) :: z = ''abc'' ! comment'//new_line('a')// &
        'end')

    ! A character constant continued with '&' is legal and must survive.
    call expect_accepted( &
        'continued character constant', &
        'program p'//new_line('a')// &
        '   character(8) :: z'//new_line('a')// &
        '   z = ''abc&'//new_line('a')// &
        '        &def'''//new_line('a')// &
        'end')

    ! An apostrophe inside a comment opens no character constant.
    call expect_accepted( &
        'apostrophe inside comment', &
        'program p'//new_line('a')// &
        '   ! don''t reject this'//new_line('a')// &
        'end')

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' check(s) failed'
        error stop 1
    end if

    write (output_unit, '(A)') &
        'PASS: reject-literal-01 unterminated character constant diagnostics'

contains

    subroutine expect_rejected(label, source, expected_fragment)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected_fragment
        character(len=:), allocatable :: lex_error
        type(token_t), allocatable :: tokens(:)

        call lex_source(source, tokens, lex_error)

        if (len_trim(lex_error) == 0) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: '//label//' was accepted'
            return
        end if

        if (index(lex_error, expected_fragment) == 0) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: '//label// &
                ' diagnostic missing "'//expected_fragment//'"'
            write (error_unit, '(A)') '  got: '//trim(lex_error)
            return
        end if

        write (output_unit, '(A)') 'ok (rejected): '//label
    end subroutine expect_rejected

    subroutine expect_accepted(label, source)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: lex_error
        type(token_t), allocatable :: tokens(:)

        call lex_source(source, tokens, lex_error)

        if (len_trim(lex_error) /= 0) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: '//label//' was rejected'
            write (error_unit, '(A)') '  got: '//trim(lex_error)
            return
        end if

        write (output_unit, '(A)') 'ok (accepted): '//label
    end subroutine expect_accepted

end program test_reject_literal_01_diagnostics
