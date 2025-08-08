program test_issue_178
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=*), parameter :: input = &
        "i = [2, 3, 4]" // new_line('a') // &
        "print*,i**3"
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    
    call transform_lazy_fortran_string_with_format(input, output, error_msg, options)
    
    if (allocated(error_msg)) then
        print *, "Error: ", error_msg
        stop 1
    end if
    
    print *, "Output:"
    print *, output
    
    ! Check if 'integer' is in the output
    if (index(output, "integer") > 0) then
        print *, "PASS: Variable i has integer type"
        stop 0
    else
        print *, "FAIL: Variable i should have integer type, not real"
        stop 1
    end if
    
end program test_issue_178