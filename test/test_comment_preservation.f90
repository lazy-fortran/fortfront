program test_comment_preservation
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    logical :: test_passed
    
    test_passed = .true.
    
    ! Test 1: Single line comment
    print *, "Test 1: Single line comment"
    call transform_lazy_fortran_string_with_format( &
        "! This is a comment" // new_line('a') // &
        "x = 1", &
        output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)
        
        if (index(output, "! This is a comment") > 0 .or. &
            index(output, "!This is a comment") > 0) then
            print *, "  PASS: Comment preserved"
        else
            print *, "  FAIL: Comment missing from output"
            test_passed = .false.
        end if
    end if
    
    ! Test 2: Inline comment
    print *, ""
    print *, "Test 2: Inline comment"
    call transform_lazy_fortran_string_with_format( &
        "x = 1  ! inline comment" // new_line('a') // &
        "y = 2", &
        output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)
        
        if (index(output, "! inline comment") > 0 .or. &
            index(output, "!inline comment") > 0) then
            print *, "  PASS: Inline comment preserved"
        else
            print *, "  FAIL: Inline comment missing from output"
            test_passed = .false.
        end if
    end if
    
    ! Test 3: Multiple comments
    print *, ""
    print *, "Test 3: Multiple comments"
    call transform_lazy_fortran_string_with_format( &
        "! Header comment" // new_line('a') // &
        "! Second line" // new_line('a') // &
        "program test" // new_line('a') // &
        "! Body comment" // new_line('a') // &
        "x = 1" // new_line('a') // &
        "end program", &
        output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)
        
        if (index(output, "! Header comment") > 0 .or. &
            index(output, "!Header comment") > 0) then
            print *, "  PASS: Header comment preserved"
        else
            print *, "  FAIL: Header comment missing"
            test_passed = .false.
        end if
        
        if (index(output, "! Body comment") > 0 .or. &
            index(output, "!Body comment") > 0) then
            print *, "  PASS: Body comment preserved"
        else
            print *, "  FAIL: Body comment missing"
            test_passed = .false.
        end if
    end if
    
    if (test_passed) then
        print *, ""
        print *, "All tests passed!"
        stop 0
    else
        print *, ""
        print *, "Some tests failed!"
        stop 1
    end if
    
end program test_comment_preservation