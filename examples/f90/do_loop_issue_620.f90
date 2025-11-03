program do_loop_issue_620
    implicit none
    integer :: i
    do i = 1, 3
        print *, i
    end do
end program do_loop_issue_620
