program issue_1889_single_line_if
    implicit none
    integer :: i, j

    j = 0
    do i = 1, 5
        if (i == 2) j = j + i
        if (j == 3) cycle
        if (i == 4) exit
    end do

    if (j > 0) j = j - 1
end program issue_1889_single_line_if
