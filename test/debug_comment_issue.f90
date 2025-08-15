program debug_comment_issue
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    character(len=:), allocatable :: test_source
    
    ! Test the exact failing case from test_comment_preservation
    test_source = "! Header comment" // new_line('a') // &
                  "! Second line" // new_line('a') // &
                  "program test" // new_line('a') // &
                  "! Body comment" // new_line('a') // &
                  "x = 1" // new_line('a') // &
                  "end program"
    
    print *, "=== DIAGNOSTIC: Comment Preservation Issue ==="
    print *, ""
    print *, "Input source:"
    print *, trim(test_source)
    print *, ""
    
    call transform_lazy_fortran_string_with_format(test_source, output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Output:"
    print *, trim(output)
    print *, ""
    
    ! Detailed analysis
    if (index(output, "! Header comment") > 0) then
        print *, "✓ Header comment found"
    else
        print *, "✗ Header comment MISSING"
    end if
    
    if (index(output, "! Second line") > 0) then
        print *, "✓ Second line comment found"
    else
        print *, "✗ Second line comment MISSING"
    end if
    
    if (index(output, "! Body comment") > 0) then
        print *, "✓ Body comment found"
    else
        print *, "✗ Body comment MISSING"
    end if
    
end program debug_comment_issue