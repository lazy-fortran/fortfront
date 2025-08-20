program test_issue_320
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=*), parameter :: source = &
        "function twice(x) result(y)" // new_line('A') // &
        "    y = 2.0 * x" // new_line('A') // &
        "end function twice"
    
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "Testing Issue #320: Function variable declarations"
    print *, "Input:"
    print *, trim(source)
    print *
    
    call transform_lazy_fortran_string(source, output, error_msg)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "Compilation failed: ", trim(error_msg)
        stop 1
    else if (.not. allocated(output)) then
        print *, "Compilation failed: no output generated"
        stop 1
    else
        print *, "Output:"
        print *, trim(output)
        
        ! Check if declarations were generated
        if (index(output, "real :: x") > 0 .or. index(output, "real::x") > 0) then
            print *, "SUCCESS: Variable x declaration generated"
        else
            print *, "FAIL: Variable x declaration not found"
        end if
        
        if (index(output, "real :: y") > 0 .or. index(output, "real::y") > 0) then
            print *, "SUCCESS: Variable y declaration generated"
        else
            print *, "FAIL: Variable y declaration not found"
        end if
    end if
end program test_issue_320