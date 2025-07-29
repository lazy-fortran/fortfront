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
    
    if (count /= 4) error stop "CYCLE test failed: count should be 4"
    if (result(1) /= 1) error stop "CYCLE test failed: result(1) should be 1"
    if (result(2) /= 2) error stop "CYCLE test failed: result(2) should be 2"
    if (result(3) /= 4) error stop "CYCLE test failed: result(3) should be 4"
    if (result(4) /= 5) error stop "CYCLE test failed: result(4) should be 5"
    
    ! Test basic EXIT
    count = 0
    do i = 1, 10
        if (i == 7) exit
        count = count + 1
        result(count) = i
    end do
    
    if (count /= 6) error stop "EXIT test failed: count should be 6"
    if (result(6) /= 6) error stop "EXIT test failed: result(6) should be 6"
    
    ! Test labeled loops with CYCLE
    count = 0
    outer: do i = 1, 3
        inner: do j = 1, 3
            if (i == j) cycle outer
            count = count + 1
        end do inner
    end do outer
    
    if (count /= 3) error stop "Labeled CYCLE test failed: count should be 3"
    
    ! Test labeled loops with EXIT
    count = 0
    outer2: do i = 1, 5
        inner2: do j = 1, 5
            count = count + 1
            if (i == 2 .and. j == 3) exit outer2
        end do inner2
    end do outer2
    
    if (count /= 8) error stop "Labeled EXIT test failed: count should be 8"
    
    print *, "All tests passed!"
end program test_cycle_exit