program test_issue_1711_implicit_range
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, result_code, error_msg

    ! Test input with implicit integer and real ranges
    source = "program test_implicit" // new_line('a') // &
             "    implicit integer (a-h)" // new_line('a') // &
             "    implicit real (o-z)" // new_line('a') // &
             "    " // new_line('a') // &
             "    apple = 5" // new_line('a') // &
             "    zebra = 3.14" // new_line('a') // &
             "    " // new_line('a') // &
             "    print *, apple, zebra" // new_line('a') // &
             "end program test_implicit"

    call transform_lazy_fortran_string(source, result_code, error_msg)

    ! Verify implicit statements are preserved with letter ranges
    if (index(result_code, "implicit integer (a-h)") == 0) then
        print *, "ERROR: implicit integer (a-h) not found in output"
        print *, "Output:", result_code
        error stop "FAIL: implicit integer statement with range not preserved"
    end if

    if (index(result_code, "implicit real (o-z)") == 0) then
        print *, "ERROR: implicit real (o-z) not found in output"
        print *, "Output:", result_code
        error stop "FAIL: implicit real statement with range not preserved"
    end if

    ! Should NOT generate duplicate implicit none when IMPLICIT ranges are present
    if (index(result_code, "implicit none") > 0) then
        print *, "ERROR: implicit none should not be added with implicit ranges"
        print *, "Output:", result_code
        error stop "FAIL: implicit none incorrectly added"
    end if

    ! Should not generate invalid syntax like real(o-z) or integer(a-h) in declarations
    if (index(result_code, "real(o-z)") > 0 .or. index(result_code, "integer(a-h)") > 0) then
        print *, "ERROR: invalid syntax generated"
        print *, "Output:", result_code
        error stop "FAIL: invalid syntax in declarations"
    end if

    print *, "PASS: test_issue_1711_implicit_range"
end program test_issue_1711_implicit_range
