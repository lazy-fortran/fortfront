program test_issue_1786_stop_preservation
    implicit none
    integer :: x

    x = 10
    if (x > 5) then
        print *, 'x is large'
        print *, 'Continuing after print'
    end if
    print *, 'This SHOULD print - it is after end if'
    if (x > 5) then
        print *, 'Checking uppercase END IF branch'
        stop 0
    END IF
    print *, 'Uppercase END IF branch preserved'
    stop 0
end program test_issue_1786_stop_preservation
