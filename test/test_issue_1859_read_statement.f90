program test_issue_1859_read_statement
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg

    print *, "=== Issue #1859: READ statement variable inference ==="

    ! Test case from issue #1859
    input_code = "read *, x" // new_line('a') // &
                 "y = x * 2" // new_line('a') // &
                 "print *, 'Result:', y"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Transformation failed:", error_msg
        error stop 1
    end if

    ! Verify that x is declared
    if (index(output_code, "real :: x") == 0) then
        print *, "FAIL: Variable x not declared in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    ! Verify that y is declared
    if (index(output_code, "integer :: y") == 0) then
        print *, "FAIL: Variable y not declared in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    ! Verify that read statement is present
    if (index(output_code, "read(*, *) x") == 0) then
        print *, "FAIL: READ statement not found in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: READ statement variable inference working correctly"

end program test_issue_1859_read_statement
