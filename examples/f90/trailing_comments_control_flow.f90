program test_trailing_comments_control_flow
    implicit none
    integer :: i, sum
    sum = 0
    do i = 1, 10 ! Loop from 1 to 10
        sum = sum + i ! Accumulate
    end do ! End loop
    if (sum > 50) then ! Check threshold
        print *, "Large sum" ! Print message
    end if ! End check
end program test_trailing_comments_control_flow
