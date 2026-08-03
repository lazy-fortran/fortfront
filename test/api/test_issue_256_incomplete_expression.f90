program test_issue_256_incomplete_expression
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: output, error_msg
    logical :: all_passed

    all_passed = .true.

    call test_trailing_operator_diagnostic(all_passed)
    call test_complete_expression(all_passed)
    call test_operator_continuation(all_passed)

    if (.not. all_passed) error stop 1
    print *, 'PASS: Issue #256 incomplete-expression parser/API regression'

contains

    subroutine test_trailing_operator_diagnostic(passed)
        logical, intent(inout) :: passed
        character(len=*), parameter :: source = &
            'program test' // new_line('a') // &
            '    integer :: x' // new_line('a') // &
            '    x = 42 +' // new_line('a') // &
            '    print *, x' // new_line('a') // &
            'end program'

        call transform_lazy_fortran_string(source, output, error_msg)
        if (index(error_msg, '[INCOMPLETE_EXPRESSION]') == 0 .or. &
            index(error_msg, "operator '+' needs operand") == 0 .or. &
            index(error_msg, 'at line 3, column 12') == 0 .or. &
            index(output, '! COMPILATION FAILED') == 0) then
            print *, 'FAIL: trailing operator diagnostic:'
            print *, trim(error_msg)
            passed = .false.
        end if
    end subroutine test_trailing_operator_diagnostic

    subroutine test_complete_expression(passed)
        logical, intent(inout) :: passed

        call transform_lazy_fortran_string('x = 42 + 1', output, error_msg)
        if (len_trim(error_msg) > 0 .or. index(output, 'x = 42 + 1') == 0) then
            print *, 'FAIL: complete expression rejected:'
            print *, trim(error_msg)
            passed = .false.
        end if
    end subroutine test_complete_expression

    subroutine test_operator_continuation(passed)
        logical, intent(inout) :: passed
        character(len=*), parameter :: source = &
            'total = 40 +' // new_line('a') // &
            '& 2'

        call transform_lazy_fortran_string(source, output, error_msg)
        if (len_trim(error_msg) > 0 .or. index(output, 'total = 40 + 2') == 0) then
            print *, 'FAIL: explicit operator continuation rejected:'
            print *, trim(error_msg)
            passed = .false.
        end if
    end subroutine test_operator_continuation

end program test_issue_256_incomplete_expression
