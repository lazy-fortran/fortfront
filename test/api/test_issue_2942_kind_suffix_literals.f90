program test_issue_2942_kind_suffix_literals
    ! Issue #2942: a kind suffix on a literal whose mantissa ends in '.'
    ! (3._dp, 0._k1, 5._4) and on a logical literal (.false._8) is valid
    ! Fortran; gfortran -fsyntax-only accepts all cases below. The
    ! leading-underscore name rule from #2929 must not reject them, while a
    ! name that really starts with an underscore stays rejected.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    integer :: failures

    failures = 0

    ! Reduced case from the issue.
    call expect_accepted('real and logical kind suffix', &
        'program p'//nl// &
        '    integer, parameter :: dp = kind(0.d0)'//nl// &
        '    real(dp) :: a'//nl// &
        '    logical :: f'//nl// &
        '    a = 3._dp'//nl// &
        '    f = .false._8'//nl// &
        '    print *, a, f'//nl// &
        'end program p', failures)

    ! gfortran.dg/logical_3.f90
    call expect_accepted('logical_3.f90', &
        'function f()'//nl// &
        '  logical(8) :: f'//nl// &
        '  f = .false._8'//nl// &
        'end function f', failures)

    ! gfortran.dg/norm2_5.f90 (kind suffix on 0. inside a specification expr)
    call expect_accepted('norm2_5.f90', &
        'program test'//nl// &
        '  implicit none'//nl// &
        '  integer, parameter :: k1 = &'//nl// &
        '    max(selected_real_kind(precision(0.d0) + 1), kind(0.))'//nl// &
        '  integer, parameter :: k2 = &'//nl// &
        '    max(selected_real_kind(precision(0._k1) + 1), kind(0.d0))'//nl// &
        '  real(kind=k2) :: d2(10)'//nl// &
        '  d2 = 1'//nl// &
        '  print *, norm2(d2)'//nl// &
        'end program test', failures)

    ! lfortran/integration_tests/intrinsics_13.f90
    call expect_accepted('intrinsics_13.f90', &
        'program p'//nl// &
        ' implicit none'//nl// &
        ' integer, parameter :: p5 = kind(5._4)'//nl// &
        ' integer, parameter :: p6 = kind(5._8)'//nl// &
        ' print *, p5, p6'//nl// &
        'end program p', failures)

    ! lfortran/integration_tests/arrays_02.f90
    call expect_accepted('arrays_02.f90', &
        'program p'//nl// &
        ' use iso_fortran_env, only: dp => real64'//nl// &
        ' implicit none'//nl// &
        ' real(dp) :: a(3), b'//nl// &
        ' a(1) = 3._dp'//nl// &
        ' a(2) = 2._dp'//nl// &
        ' a(3) = 1._dp'//nl// &
        ' b = sum(a)'//nl// &
        ' if (abs(b - 6._dp) > 1e-12_dp) error stop'//nl// &
        'end program p', failures)

    ! lfortran/integration_tests/arrays_intrin_04.f90 (suffix in an expression)
    call expect_accepted('arrays_intrin_04.f90', &
        'program p'//nl// &
        ' use iso_fortran_env, only: dp => real64'//nl// &
        ' implicit none'//nl// &
        ' real(dp) :: x, xdiff'//nl// &
        ' x = 1 / 7._dp'//nl// &
        ' xdiff = abs(x - (8._dp / 7))'//nl// &
        ' print *, xdiff'//nl// &
        'end program p', failures)

    ! .true. with a kind suffix, and the uppercase spelling.
    call expect_accepted('true kind suffix', &
        'program p'//nl// &
        '  logical(4) :: t'//nl// &
        '  t = .TRUE._4'//nl// &
        '  print *, t'//nl// &
        'end program p', failures)

    ! Negative controls: a name may still not start with an underscore.
    call expect_rejected('leading underscore subroutine name', &
        'SUBROUTINE _foo'//nl// &
        'END', failures)
    call expect_rejected('leading underscore variable name', &
        'program p'//nl// &
        '   implicit none'//nl// &
        '   integer :: _a'//nl// &
        '   print *, 1'//nl// &
        'end', failures)
    call expect_rejected('underscore after an operator', &
        'program p'//nl// &
        '   implicit none'//nl// &
        '   integer :: a'//nl// &
        '   a = 1 + _2'//nl// &
        '   print *, a'//nl// &
        'end', failures)
    call expect_rejected('underscore after a dotted operator', &
        'program p'//nl// &
        '   implicit none'//nl// &
        '   logical :: a'//nl// &
        '   a = .true. .and._x'//nl// &
        '   print *, a'//nl// &
        'end', failures)

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' case(s) failed'
        stop 1
    end if
    print *, 'PASS: issue 2942 kind suffix literals'

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

end program test_issue_2942_kind_suffix_literals
