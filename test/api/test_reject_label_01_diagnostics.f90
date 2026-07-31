program test_reject_label_01_diagnostics
    ! Issue #2889 [reject-label-01]: invalid statement labels must be rejected
    ! with a source diagnostic, while the corrected neighbouring form still
    ! compiles.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    integer :: failures

    failures = 0

    ! gfortran.dg/empty_label.f90: statement label without a statement
    call expect_rejected('empty_label.f90', &
        'program p'//nl// &
        '100'//nl// &
        'end program p', failures)
    call expect_accepted('empty_label corrected neighbour', &
        'program p'//nl// &
        '100 continue'//nl// &
        'end program p', failures)

    ! gfortran.dg/label_1.f90: too many digits, and a zero label
    call expect_rejected('label_1.f90 too many digits', &
        'program a'//nl// &
        '0056780 continue'//nl// &
        'end program a', failures)
    call expect_rejected('label_1.f90 zero label', &
        'program a'//nl// &
        '0 continue'//nl// &
        'end program a', failures)
    call expect_accepted('label_1 corrected neighbour', &
        'program a'//nl// &
        '56780 continue'//nl// &
        '1 continue'//nl// &
        'end program a', failures)

    ! gfortran.dg/label_2.f90: invalid character directly after the label
    call expect_rejected('label_2.f90', &
        'program pr24640'//nl// &
        '   integer :: a'//nl// &
        '10: a=10'//nl// &
        'end program pr24640', failures)
    call expect_accepted('label_2 corrected neighbour', &
        'program pr24640'//nl// &
        '   integer :: a'//nl// &
        '10 a=10'//nl// &
        '   print *, a'//nl// &
        'end program pr24640', failures)

    ! Named construct labels keep working: they are identifiers, not labels.
    call expect_accepted('named construct label', &
        'program named'//nl// &
        '   integer :: i'//nl// &
        '   loop: do i = 1, 3'//nl// &
        '      print *, i'//nl// &
        '   end do loop'//nl// &
        'end program named', failures)

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' case(s) failed'
        stop 1
    end if
    print *, 'PASS: reject-label-01 diagnostics'

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
        if (reported) reported = index(to_lower(result%error_msg), 'label') > 0

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

    function to_lower(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lowered
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lowered(i:i) = achar(code + 32)
            else
                lowered(i:i) = text(i:i)
            end if
        end do
    end function to_lower

end program test_reject_label_01_diagnostics
