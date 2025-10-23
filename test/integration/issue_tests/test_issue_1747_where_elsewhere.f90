program test_issue_1747_where_elsewhere
    implicit none
    real :: arr(5)
    integer :: i

    do i = 1, 5
        arr(i) = real(i) - 3.0
    end do

    where (arr > 0.0)
        arr = arr*2.0
    elsewhere (arr < 0.0)
        arr = arr*0.5
    elsewhere
        arr = 0.0
    end where

    if (abs(arr(1) - (-1.0)) > 1e-6) then
        print *, 'Test failed: arr(1) should be -1.0, got', arr(1)
        stop 1
    end if
    if (abs(arr(2) - (-0.5)) > 1e-6) then
        print *, 'Test failed: arr(2) should be -0.5, got', arr(2)
        stop 1
    end if
    if (abs(arr(3) - 0.0) > 1e-6) then
        print *, 'Test failed: arr(3) should be 0.0, got', arr(3)
        stop 1
    end if
    if (abs(arr(4) - 2.0) > 1e-6) then
        print *, 'Test failed: arr(4) should be 2.0, got', arr(4)
        stop 1
    end if
    if (abs(arr(5) - 4.0) > 1e-6) then
        print *, 'Test failed: arr(5) should be 4.0, got', arr(5)
        stop 1
    end if

    print *, 'Test passed!'
end program test_issue_1747_where_elsewhere
