program close_if_block
    implicit none
    integer :: unit
    unit = 10
    open(unit=unit, status='scratch')
    if (unit > 0) then
        close(unit)
    end if
end program close_if_block
