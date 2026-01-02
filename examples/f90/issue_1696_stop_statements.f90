! Issue #1696: STOP statement preservation
program issue_1696_stop_statements
    implicit none

    integer :: x

    x = 5
    if (x > 3) then
        stop 'Error: x is too large'
    end if

    print *, 'This should not print'
end program issue_1696_stop_statements
