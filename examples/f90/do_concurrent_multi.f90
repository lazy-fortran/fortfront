program do_concurrent_multi
    implicit none
    integer :: i, j
    integer :: arr(3, 3)

    do concurrent (i = 1:3, j = 1:3)
        arr(i, j) = i + j
    end do

    print *, arr
end program do_concurrent_multi
