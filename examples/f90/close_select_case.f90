program close_select_case
    implicit none
    integer :: unit
    integer :: choice
    unit = 40
    choice = 1
    open(unit=unit, status='scratch')
    select case (choice)
    case (1)
        close(unit)
    case default
        close(unit)
    end select
end program close_select_case
