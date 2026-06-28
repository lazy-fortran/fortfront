program test_issue_1049_incomplete_func
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source, output, error_msg
    logical :: passed

    print *, '=== Issue #1049: Incomplete func definition ==='

    ! Reproduction: Lazy Fortran-style function header without proper body
    ! Trailing newline avoids a gfortran 16 bug where a 64-character literal
    ! concat assigned to a deferred-length allocatable yields all-zero bytes.
    source = 'func incomplete_function(x: int)' // new_line('a') // &
        'result = incomplete_function(5)' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    passed = (index(error_msg, 'Incomplete function-like definition') > 0 .and. &
        index(error_msg, 'func') > 0 .and. &
        index(output, 'COMPILATION FAILED') > 0)

    if (passed) then
        print *, '  PASS: Detected incomplete func definition with clear diagnostic'
        stop 0
    else
        print *, '  FAIL: Did not detect incomplete func definition'
        print *, '    error_msg = ', trim(error_msg)
        print *, '    output (first 120 chars) = ', output(1:min(120, len(output)))
        stop 1
    end if
end program test_issue_1049_incomplete_func

