program test_issue_281_mathematical_expressions
    ! Test for Issue #281: Mathematical expressions without assignment generate empty programs
    ! This test ensures that pure mathematical expressions are properly parsed and transformed

    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed

    print *, "Testing Issue #281: Mathematical expressions without assignment"

    ! Test Case 1: Simple addition
    input = "2 + 3"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = error_msg == "" .and. index(output, "2 + 3") > 0 .and. &
                 index(output, "program main") > 0 .and. index(output, "implicit none") > 0
    if (.not. test_passed) then
        print *, "FAILED: Simple addition (2 + 3)"
        print *, "Error:", error_msg
        print *, "Output:", output
        stop 1
    end if

    ! Test Case 2: Multiplication with floating point
    input = "42 * 3.14"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = error_msg == "" .and. &
                 (index(output, "42*3.14") > 0 .or. index(output, "42 * 3.14") > 0) .and. &
                 index(output, "program main") > 0
    if (.not. test_passed) then
        print *, "FAILED: Multiplication (42 * 3.14)"
        print *, "Error:", error_msg
        print *, "Output:", output
        stop 1
    end if

    ! Test Case 3: Complex parenthesized expression
    input = "(2 + 3) * 4 - 1"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = error_msg == "" .and. &
                 (index(output, "(2+3)*4-1") > 0 .or. index(output, "(2 + 3)*4 - 1") > 0) .and. &
                 index(output, "program main") > 0
    if (.not. test_passed) then
        print *, "FAILED: Complex expression ((2 + 3) * 4 - 1)"
        print *, "Error:", error_msg
        print *, "Output:", output
        stop 1
    end if

    ! Test Case 4: Division
    input = "10 / 2"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = error_msg == "" .and. &
                 (index(output, "10/2") > 0 .or. index(output, "10 / 2") > 0) .and. &
                 index(output, "program main") > 0
    if (.not. test_passed) then
        print *, "FAILED: Division (10 / 2)"
        print *, "Error:", error_msg
        print *, "Output:", output
        stop 1
    end if

    ! Test Case 5: Power operation
    input = "2 ** 3"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = error_msg == "" .and. &
                 (index(output, "2**3") > 0 .or. index(output, "2 ** 3") > 0) .and. &
                 index(output, "program main") > 0
    if (.not. test_passed) then
        print *, "FAILED: Power operation (2 ** 3)"
        print *, "Error:", error_msg
        print *, "Output:", output
        stop 1
    end if

    ! Test Case 6: Ensure print statements still work correctly
    input = "print *, 1 + 2 * 3"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = error_msg == "" .and. &
                 (index(output, "print *, 1 + 2*3") > 0 .or. index(output, "print *, 1 + 2 * 3") > 0) .and. &
                 index(output, "program main") > 0
    if (.not. test_passed) then
        print *, "FAILED: Print statement with expression (print *, 1 + 2 * 3)"
        print *, "Error:", error_msg
        print *, "Output:", output
        stop 1
    end if

    print *, "All tests passed! Issue #281 is FIXED."
    print *, "Mathematical expressions without assignment now generate valid Fortran programs."

end program test_issue_281_mathematical_expressions