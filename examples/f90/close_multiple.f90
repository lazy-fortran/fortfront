program close_multiple
    implicit none
    integer :: unit
    unit = 50
    open(unit=unit, status='scratch')
    if (unit > 0) then
        close(unit)
        close(unit)
    end if
end program close_multiple
