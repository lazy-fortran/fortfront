program close_nested_if
    implicit none
    integer :: unit
    unit = 20
    open(unit=unit, status='scratch')
    if (unit > 0) then
        if (unit > 10) then
            close(unit)
        end if
    end if
end program close_nested_if
