program test_pause
    implicit none
    integer :: i

    do i = 1, 3
        print *, 'Iteration', i
        if (i == 2) then
            pause 'Paused at iteration 2'
        end if
    end do

    print *, 'Done'
end program test_pause
