program test_issue_2980_binary_result
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, code, error_msg

    call read_example('examples/lf/issue_2980_binary_result.lf', source)
    call transform_lazy_fortran_string(source, code, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation error: '//error_msg
        error stop 1
    end if

    ! result(y) must follow the real binary RHS `2*x` where x is a real dummy.
    ! Regression: it was inferred INTEGER (issue #2980).
    if (index(code, 'integer :: y') > 0 .or. &
        index(code, 'integer function twice') > 0) then
        write (error_unit, '(A)') 'FAIL: result(y) was not inferred real from real RHS'
        write (error_unit, '(A)') trim(code)
        error stop 1
    end if
    if (index(code, 'real') <= 0) then
        write (error_unit, '(A)') 'FAIL: expected a real result type'
        write (error_unit, '(A)') trim(code)
        error stop 1
    end if

    print '(A)', 'PASS: issue #2980 result(y) follows real binary RHS'

contains

    include '../common/read_example.inc'

end program test_issue_2980_binary_result
