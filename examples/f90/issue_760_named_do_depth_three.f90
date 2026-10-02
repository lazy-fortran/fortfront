program named_do_depth_three
    implicit none
    integer :: i, j, k, total

    total = 0
    do i = 1, 2
        do j = 1, 2
            c: do k = 1, 3
            if (k == 2) cycle c
            total = total + k
        end do c
    end do
end do
print *, total
end program named_do_depth_three
