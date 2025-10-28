program close_do_loop
    implicit none
    integer :: unit
    integer :: i
    unit = 30
    open(unit=unit, status='scratch')
    do i = 1, 2
        if (i == 2) then
            close(unit)
        end if
    end do
end program close_do_loop
