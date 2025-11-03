program do_loop_expression_bounds
    implicit none
    integer :: i, n
    n = 10
    do i = n-5, n+5
        print *, i
    end do
end program do_loop_expression_bounds
