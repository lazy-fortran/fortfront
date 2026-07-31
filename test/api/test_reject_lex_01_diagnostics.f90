program test_reject_lex_01_diagnostics
    ! Issue #2890 [reject-lex-01]: invalid source characters and invalid
    ! identifier characters must be rejected with a source diagnostic, while
    ! the corrected neighbouring form still compiles.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    integer :: failures

    failures = 0

    ! gfortran.dg/illegal_char.f90
    call expect_rejected('illegal_char.f90', &
        'program main'//nl// &
        '  tmp ='//achar(200)//'   1.0'//nl// &
        '  print *,tmp'//nl// &
        'end', failures)
    call expect_accepted('illegal_char corrected neighbour', &
        'program main'//nl// &
        '  tmp =   1.0'//nl// &
        '  print *,tmp'//nl// &
        'end', failures)

    ! gfortran.dg/invalid_name.f90
    call expect_rejected('invalid_name.f90', &
        'SUBROUTINE _foo'//nl// &
        'END', failures)
    call expect_accepted('invalid_name corrected neighbour', &
        'SUBROUTINE foo'//nl// &
        'END', failures)

    ! gfortran.dg/pr91959.f90
    call expect_rejected('pr91959.f90', &
        'program p'//nl// &
        '   implicit none'//nl// &
        '   integer :: %a'//nl// &
        '   print *, 1'//nl// &
        'end', failures)
    call expect_accepted('pr91959 corrected neighbour', &
        'program p'//nl// &
        '   implicit none'//nl// &
        '   integer :: a'//nl// &
        '   a = 1'//nl// &
        '   print *, a'//nl// &
        'end', failures)

    ! Invalid bytes inside comments and character literals stay legal.
    call expect_accepted('non-ascii byte in comment and literal', &
        'program q'//nl// &
        '  ! caf'//achar(200)//' comment'//nl// &
        '  print *, "caf'//achar(200)//'"'//nl// &
        'end', failures)

    ! Legitimate component selection keeps working.
    call expect_accepted('component selector', &
        'program r'//nl// &
        '  type :: point_t'//nl// &
        '    real :: x'//nl// &
        '  end type point_t'//nl// &
        '  type(point_t) :: p'//nl// &
        '  p%x = 1.0'//nl// &
        '  print *, p%x'//nl// &
        'end program r', failures)

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' case(s) failed'
        stop 1
    end if
    print *, 'PASS: reject-lex-01 diagnostics'

contains

    subroutine compile_case(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: opts

        opts%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(source, result, opts)
    end subroutine compile_case

    subroutine expect_rejected(label, source, failure_count)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        integer, intent(inout) :: failure_count
        type(compiler_frontend_result_t) :: result
        logical :: reported

        call compile_case(source, result)
        reported = .not. result%success()
        if (reported) reported = allocated(result%error_msg)
        if (reported) reported = len_trim(result%error_msg) > 0

        if (reported) then
            print *, 'PASS: rejected ', label
        else
            failure_count = failure_count + 1
            write (error_unit, '(A,A)') 'FAIL: not rejected: ', label
        end if
    end subroutine expect_rejected

    subroutine expect_accepted(label, source, failure_count)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        integer, intent(inout) :: failure_count
        type(compiler_frontend_result_t) :: result

        call compile_case(source, result)
        if (result%success()) then
            print *, 'PASS: accepted ', label
        else
            failure_count = failure_count + 1
            write (error_unit, '(A,A)') 'FAIL: wrongly rejected: ', label
            if (allocated(result%error_msg)) then
                write (error_unit, '(A,A)') '  error: ', result%error_msg
            end if
        end if
    end subroutine expect_accepted

end program test_reject_lex_01_diagnostics
