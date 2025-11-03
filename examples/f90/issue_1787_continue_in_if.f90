program issue_1787_continue_in_if
    implicit none
    integer :: i

    do i = 1, 5
        if (i == 3) then
            continue
        end if
    end do
end program issue_1787_continue_in_if
