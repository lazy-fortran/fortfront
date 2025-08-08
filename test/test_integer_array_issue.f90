program test_integer_array_issue
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    logical :: test_passed
    
    test_passed = .true.
    
    ! Test 1: Integer array literal
    print *, "Test 1: Integer array [2, 3, 4]"
    call transform_lazy_fortran_string_with_format( &
        "i = [2, 3, 4]" // new_line('a') // "print*,i**3", &
        output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)
        
        if (index(output, "integer") > 0) then
            print *, "  PASS: Variable i has integer type"
        else
            print *, "  FAIL: Variable i should have integer type, not real"
            test_passed = .false.
        end if
    end if
    
    ! Test 2: Real array literal (should stay real)
    print *, ""
    print *, "Test 2: Real array [1.0, 2.5, 3.14]"
    call transform_lazy_fortran_string_with_format( &
        "x = [1.0, 2.5, 3.14]" // new_line('a') // "print*,x", &
        output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)
        
        if (index(output, "real") > 0) then
            print *, "  PASS: Variable x has real type"
        else
            print *, "  FAIL: Variable x should have real type"
            test_passed = .false.
        end if
    end if
    
    ! Test 3: Mixed array literal (should be real)
    print *, ""
    print *, "Test 3: Mixed array [1, 2.0, 3]"
    call transform_lazy_fortran_string_with_format( &
        "y = [1, 2.0, 3]" // new_line('a') // "print*,y", &
        output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)
        
        if (index(output, "real") > 0) then
            print *, "  PASS: Variable y has real type (promoted from mixed)"
        else
            print *, "  FAIL: Variable y should have real type for mixed array"
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
    
end program test_integer_array_issue