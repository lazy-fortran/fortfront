program test_formatting_idempotent
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: first_output, second_output, third_output
    character(len=:), allocatable :: error_msg
    logical :: test_passed
    
    print *, "Testing that formatting is idempotent (preserves program structure)..."
    
    ! Test the exact issue from #93 - multiple implicit none statements
    test_code = "program test_format" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    integer :: x = 42" // new_line('a') // &
                "    print *, x" // new_line('a') // &
                "end program test_format"
    
    ! First formatting pass
    call transform_lazy_fortran_string(test_code, first_output, error_msg)
    if (len(error_msg) > 0) then
        print *, "ERROR: First formatting failed"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    ! Second formatting pass (should be identical)
    call transform_lazy_fortran_string(first_output, second_output, error_msg)
    if (len(error_msg) > 0) then
        print *, "ERROR: Second formatting failed"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    ! Third formatting pass (should still be identical)
    call transform_lazy_fortran_string(second_output, third_output, error_msg)
    if (len(error_msg) > 0) then
        print *, "ERROR: Third formatting failed"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    ! Check that all outputs are identical (formatting is idempotent)
    test_passed = .true.
    
    if (first_output /= second_output) then
        print *, "ERROR: First and second formatting outputs differ"
        print *, "First output:"
        print *, trim(first_output)
        print *, "Second output:"  
        print *, trim(second_output)
        test_passed = .false.
    end if
    
    if (second_output /= third_output) then
        print *, "ERROR: Second and third formatting outputs differ"
        print *, "Second output:"
        print *, trim(second_output)
        print *, "Third output:"
        print *, trim(third_output)
        test_passed = .false.
    end if
    
    ! Check that there's only one implicit none statement
    block
        integer :: implicit_count, pos, search_pos
        implicit_count = 0
        search_pos = 1
        
        do
            pos = index(third_output(search_pos:), "implicit none")
            if (pos == 0) exit
            implicit_count = implicit_count + 1
            search_pos = search_pos + pos + 13  ! Move past "implicit none" (13 chars)
        end do
        
        if (implicit_count /= 1) then
            print *, "ERROR: Found", implicit_count, "implicit none statements, expected 1"
            print *, "Final output:"
            print *, trim(third_output)
            test_passed = .false.
        end if
    end block
    
    if (test_passed) then
        print *, "✓ Formatting is idempotent - identical results on multiple passes"
        print *, "✓ No duplicate implicit none statements"
        print *, "✓ Program structure preserved during formatting"
        print *, "All tests passed!"
    else
        stop 1
    end if
    
end program test_formatting_idempotent