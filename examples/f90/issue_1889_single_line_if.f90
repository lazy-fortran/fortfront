program issue_1889_single_line_if
    implicit none
    integer :: i, j

    do i = 1, 5
        if (j == 3) cycle
        if (i == 4) exit
    end do
end program issue_1889_single_line_if
