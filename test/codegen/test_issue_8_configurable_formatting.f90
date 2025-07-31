program test_issue_8_configurable_formatting
    ! Test for GitHub issue #8: No API for configurable code formatting
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output_2_spaces
    character(len=:), allocatable :: output_8_spaces
    character(len=:), allocatable :: error_msg
    type(format_options_t) :: format_opts_2, format_opts_8
    
    print *, "=== Issue #8: No API for configurable code formatting ==="
    
    ! Test source with nested structure
    source = "if (x > 0) then" // new_line('a') // &
             "y = x * 2" // new_line('a') // &
             "end if"
    
    ! Configure formatting with 2 spaces indentation
    format_opts_2%indent_size = 2
    format_opts_2%use_tabs = .false.
    
    ! Configure formatting with 8 spaces indentation
    format_opts_8%indent_size = 8
    format_opts_8%use_tabs = .false.
    
    call transform_lazy_fortran_string_with_format(source, output_2_spaces, error_msg, format_opts_2)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
    end if
    
    call transform_lazy_fortran_string_with_format(source, output_8_spaces, error_msg, format_opts_8)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
    end if
    
    print *, "Input source:"
    print *, trim(source)
    print *
    print *, "Output with 2-space indentation:"
    print *, trim(output_2_spaces)
    print *
    print *, "Output with 8-space indentation:"
    print *, trim(output_8_spaces)
    print *
    
    ! Check if different indentation levels are applied
    ! The y assignment is inside the if block, so it gets 2 levels of indentation
    if (index(output_2_spaces, "    y = x*2") > 0 .and. &
        index(output_8_spaces, "                y = x*2") > 0) then
        print *, "PASS: Configurable formatting API works correctly"
    else
        print *, "FAIL: Configurable formatting API not working"
        print *, "Expected in 2-space: '    y = x*2'"  
        print *, "Expected in 8-space: '                y = x*2'"
        stop 1
    end if
    
end program test_issue_8_configurable_formatting