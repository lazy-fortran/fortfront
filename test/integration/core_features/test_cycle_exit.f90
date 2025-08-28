program test_cycle_exit
    implicit none
    integer :: i, j
    integer :: count
    integer :: result(10)
    
    ! Test basic CYCLE
    count = 0
    do i = 1, 5
        if (i == 3) cycle
        count = count + 1
        result(count) = i
    end do
    
    if (count /= 4) then
        print *, "CYCLE test failed: count should be 4"
        stop 1
    end if
    if (result(1) /= 1) then
        print *, "CYCLE test failed: result(1) should be 1"
        stop 1
    end if
    if (result(2) /= 2) then
        print *, "CYCLE test failed: result(2) should be 2"
        stop 1
    end if
    if (result(3) /= 4) then
        print *, "CYCLE test failed: result(3) should be 4"
        stop 1
    end if
    if (result(4) /= 5) then
        print *, "CYCLE test failed: result(4) should be 5"
        stop 1
    end if
    
    ! Test basic EXIT
    count = 0
    do i = 1, 10
        if (i == 7) exit
        count = count + 1
        result(count) = i
    end do
    
    if (count /= 6) then
        print *, "EXIT test failed: count should be 6"
        stop 1
    end if
    if (result(6) /= 6) then
        print *, "EXIT test failed: result(6) should be 6"
        stop 1
    end if
    
    ! Test labeled loops with CYCLE
    count = 0
    outer: do i = 1, 3
        inner: do j = 1, 3
            if (i == j) cycle outer
            count = count + 1
        end do inner
    end do outer
    
    if (count /= 3) then
        print *, "Labeled CYCLE test failed: count should be 3"
        stop 1
    end if
    
    ! Test labeled loops with EXIT
    count = 0
    outer2: do i = 1, 5
        inner2: do j = 1, 5
            count = count + 1
            if (i == 2 .and. j == 3) exit outer2
        end do inner2
    end do outer2
    
    if (count /= 8) then
        print *, "Labeled EXIT test failed: count should be 8"
        stop 1
    end if
    
    print *, "All tests passed!"
end program test_cycle_exit