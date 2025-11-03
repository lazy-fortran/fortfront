program issue_1861_nested_do_print
    implicit none
    integer :: i, j
    integer :: matrix(2, 3)

    do i = 1, 2
        do j = 1, 3
            matrix(i, j) = i * 10 + j
        end do
    end do

    print *, matrix
end program issue_1861_nested_do_print
