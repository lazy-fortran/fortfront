program test_exact_georg
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
    
    call transform_lazy_fortran_string(source, result, error_msg)
    
    if (len(error_msg) > 0) then
        write(*,*) "ERROR: ", trim(error_msg)
    else
        write(*,*) "Generated code:"
        write(*,*) trim(result)
    end if
    
    ! Check for the bug
    if (index(result, "program main") > 0 .and. &
        index(result, "module m") > 0) then
        ! Check if program is nested inside module
        if (index(result, "module m") < index(result, "program main") .and. &
            index(result, "program main") < index(result, "end module")) then
            write(*,*) "BUG DETECTED: Main program nested inside module"
        else
            write(*,*) "CORRECT: Separate units detected"
        end if
    end if
end program test_exact_georg