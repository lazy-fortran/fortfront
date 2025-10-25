program test_issue_1860_print_undeclared
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg

    print *, "=== Issue #1860: Undeclared variable in print statement ==="

    ! Test case from issue #1860
    input_code = "print *, x"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Transformation failed:", error_msg
        error stop 1
    end if

    ! Verify that x is declared
    if (index(output_code, ":: x") == 0) then
        print *, "FAIL: Variable x not declared in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    ! Verify that implicit none is present
    if (index(output_code, "implicit none") == 0) then
        print *, "FAIL: implicit none not found in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    ! Verify that print statement is present
    if (index(output_code, "print *, x") == 0) then
        print *, "FAIL: print statement not found in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: Print statement undeclared variable handling working correctly"

end program test_issue_1860_print_undeclared
