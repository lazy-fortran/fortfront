program debug_parse_units
    use frontend
    implicit none
    
    character(len=*), parameter :: source = &
        "module m" // new_line('a') // &
        "    contains" // new_line('a') // &
        "    function twice(x)" // new_line('a') // &
        "        real :: twice, x" // new_line('a') // &
        "        twice = 2 * x" // new_line('a') // &
        "    end function" // new_line('a') // &
        "end module" // new_line('a') // &
        "" // new_line('a') // &
        "use m" // new_line('a') // &
        "print *, twice(3.0)"
    
    character(len=:), allocatable :: result, error_msg
    
    write(*,*) "Input source:"
    write(*,*) trim(source)
    write(*,*) ""
    write(*,*) "Generated output:"
    
    call transform_lazy_fortran_string(source, result, error_msg)
    
    if (len(error_msg) > 0) then
        write(*,*) "ERROR: ", trim(error_msg)
    else
        write(*,*) trim(result)
    end if
end program debug_parse_units