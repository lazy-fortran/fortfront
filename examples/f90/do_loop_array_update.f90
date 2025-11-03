program do_loop_array_update
    implicit none
    integer :: i
    integer :: arr(5)
    arr = [1, 2, 3, 4, 5]
    do i = 1, 5
        arr(i) = arr(i) + 1
    end do
end program do_loop_array_update
