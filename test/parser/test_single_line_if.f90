! Test for Issue #1259: Single-line if statements without 'then' keyword
! This test ensures that single-line if statements are accepted by the parser
program test_single_line_if
    implicit none

    ! These single-line if statements should compile without errors
    print *, "Testing single-line if statements (Issue #1259)"

    block
        integer :: x
        x = 10

        ! Single-line if with print statement
        if (x > 5) print *, "x is greater than 5"

        ! Single-line if with assignment
        if (x > 0) x = x + 1

        ! Single-line if with stop
        if (x < 0) stop 1
    end block

    print *, "PASS: Single-line if statements work"

end program test_single_line_if
