program test_reject_lex_01_diagnostics
    ! Issue #2890: reject invalid source and identifier characters.
    !
    ! Negative fixtures mirror gfortran.dg/illegal_char.f90,
    ! gfortran.dg/invalid_name.f90 and gfortran.dg/pr91959.f90. Each corrected
    ! neighbour must keep lexing cleanly so the check cannot pass by rejecting
    ! everything.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use frontend_core, only: lex_source
    use lexer_core, only: token_t
    implicit none

    integer :: failures

    failures = 0

    ! illegal_char.f90: non-printable byte 0xC8 outside a comment or string.
    call expect_rejected( &
        'illegal_char', &
        'program main'//new_line('a')// &
        '  tmp ='//char(200)//'   1.0'//new_line('a')// &
        '  print *,tmp'//new_line('a')// &
        'end', &
        'Invalid character 0xC8')

    ! invalid_name.f90: a name may not start with an underscore.
    call expect_rejected( &
        'invalid_name', &
        'SUBROUTINE _foo'//new_line('a')// &
        'END', &
        'Invalid character in name')

    ! A non-ASCII byte inside a name is the same defect as illegal_char.f90.
    ! fortfront used to drop such bytes silently and lex the remainder as a
    ! name, which turned examples/f90/issue_1344_character_length.f90 into a
    ! program gfortran rejects.
    call expect_rejected( &
        'non-ASCII byte in a name', &
        'program main'//new_line('a')// &
        '    character(len=12) :: '//char(206)//char(163)//'text'//new_line('a')// &
        'end program main', &
        'Invalid character 0xCE')

    ! pr91959.f90: '%' where no component selector can appear.
    call expect_rejected( &
        'pr91959', &
        'program p'//new_line('a')// &
        '   implicit none'//new_line('a')// &
        '   integer :: %a'//new_line('a')// &
        '   a = 1'//new_line('a')// &
        '   print *, a'//new_line('a')// &
        'end', &
        'Invalid character ''%''')

    ! Corrected neighbour for illegal_char.f90.
    call expect_accepted( &
        'illegal_char corrected', &
        'program main'//new_line('a')// &
        '  tmp = 1.0'//new_line('a')// &
        '  print *,tmp'//new_line('a')// &
        'end')

    ! Non-printable bytes inside comments and character constants stay legal:
    ! the Fortran character set restriction does not apply there.
    call expect_accepted( &
        'non-printable byte in comment', &
        'program main'//new_line('a')// &
        '  ! caf'//char(195)//char(169)//new_line('a')// &
        'end')

    call expect_accepted( &
        'non-printable byte in character constant', &
        'program main'//new_line('a')// &
        '  character(len=3) :: s'//new_line('a')// &
        '  s = "'//char(200)//'ab"'//new_line('a')// &
        'end')

    ! Corrected neighbour for invalid_name.f90, plus underscores that are legal
    ! because they are not the first character of the name.
    call expect_accepted( &
        'invalid_name corrected', &
        'SUBROUTINE foo'//new_line('a')// &
        'END')

    call expect_accepted( &
        'underscore inside names', &
        'program main'//new_line('a')// &
        '  integer :: my_var_1'//new_line('a')// &
        '  my_var_1 = 1'//new_line('a')// &
        'end')

    ! A kind suffix introduces an underscore that starts no name of its own.
    call expect_accepted( &
        'kind suffix underscore', &
        'program main'//new_line('a')// &
        '  real :: x'//new_line('a')// &
        '  x = 1.0_8'//new_line('a')// &
        'end')

    ! Corrected neighbour for pr91959.f90: '%' after a name, after a closing
    ! parenthesis and after a closing bracket is a component selector.
    call expect_accepted( &
        'pr91959 corrected', &
        'program p'//new_line('a')// &
        '   implicit none'//new_line('a')// &
        '   type :: t'//new_line('a')// &
        '      integer :: a'//new_line('a')// &
        '   end type t'//new_line('a')// &
        '   type(t) :: x'//new_line('a')// &
        '   type(t) :: arr(2)'//new_line('a')// &
        '   x%a = 1'//new_line('a')// &
        '   arr(1)%a = 2'//new_line('a')// &
        '   print *, x%a, arr(1)%a'//new_line('a')// &
        'end')

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' check(s) failed'
        error stop 1
    end if

    write (output_unit, '(A)') 'PASS: reject-lex-01 invalid character diagnostics'

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

end program test_reject_lex_01_diagnostics
