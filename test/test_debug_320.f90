program test_debug_320
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=*), parameter :: source = &
        "function twice(x) result(y)" // new_line('A') // &
        "    y = 2.0 * x" // new_line('A') // &
        "end function twice"
    
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Debug Issue #320 ==="
    print *, "Input:"
    print *, trim(source)
    print *
    
    call transform_lazy_fortran_string(source, output, error_msg)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "Error: ", trim(error_msg)
    else
        print *, "Output:"
        print *, trim(output)
        print *
        
        ! Check if the output contains variable declarations
        if (index(output, "real") > 0 .and. index(output, "intent(in)") > 0) then
            print *, "SUCCESS: Variable declarations found!"
        else
            print *, "ISSUE: No variable declarations found in output"
            print *, "Expected to see:"
            print *, "real, intent(in) :: x"
            print *, "real :: y"
        end if
    end if
end program test_debug_320