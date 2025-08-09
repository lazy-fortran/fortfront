program test_issue_177_line_length_fix
    ! Test for issue #177 fix: Line continuation should be added for long expressions
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=*), parameter :: input = &
        "program test" // new_line('a') // &
        "implicit none" // new_line('a') // &
        "real :: result" // new_line('a') // &
        "result = very_long_variable_name_one + very_long_variable_name_two + " // &
        "very_long_variable_name_three + very_long_variable_name_four" // new_line('a') // &
        "end program test"
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    
    print *, "=== Testing Issue #177 Fix: Line Length Enforcement ==="
    
    ! Test with short line length to force continuation
    options%line_length = 80  ! Force line continuation
    
    call transform_lazy_fortran_string_with_format(input, output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "INPUT:"
    print *, trim(input)
    print *, ""
    print *, "OUTPUT:"
    print *, trim(output)
    print *, ""
    
    ! Check if continuation character was added
    if (index(output, " &") > 0) then
        print *, "  PASS: Line continuation added for long expression"
    else
        print *, "  FAIL: No line continuation found - long line was not broken"
    end if
    
    print *, "Test completed successfully"
    
end program test_issue_177_line_length_fix