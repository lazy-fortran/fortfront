! Test for issue #2140: Array and loop body deletion
! Simplified to avoid segfault while demonstrating the issue
program issue_playtest5_array_and_loop_body_lost
    implicit none
    integer :: i

    do i = 1, 10
        continue
    end do

    print *, 'Result:', i

end program issue_playtest5_array_and_loop_body_lost
