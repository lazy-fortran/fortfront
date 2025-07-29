! Simple test for CYCLE and EXIT parsing
program test_cycle_exit_simple
    implicit none
    integer :: i, j
    
    ! Test 1: Simple cycle
    do i = 1, 10
        if (i == 5) cycle
        print *, i
    end do
    
    ! Test 2: Simple exit
    do i = 1, 10
        if (i == 7) exit
        print *, i
    end do
    
    ! Test 3: Labeled cycle and exit
    outer: do i = 1, 5
        inner: do j = 1, 5
            if (i == j) cycle inner
            if (i + j > 6) exit outer
            print *, i, j
        end do inner
    end do outer
    
    print *, "All tests complete!"
end program test_cycle_exit_simple