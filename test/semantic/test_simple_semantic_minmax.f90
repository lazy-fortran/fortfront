program test_simple_semantic_minmax
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code, output_code, error_msg
    
    print *, "Testing simple min/max semantic analysis..."
    
    ! Test 1: All integers (should work fine)
    test_code = "program test_int_minmax" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    integer :: i" // new_line('a') // &
                "    i = min(5, 3)" // new_line('a') // &
                "    i = max(5, 3)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ All-integer min/max compiled successfully"
    else
        print *, "ERROR: Failed with all-integer min/max"
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 2: All reals (should work fine)
    test_code = "program test_real_minmax" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    real :: r" // new_line('a') // &
                "    r = min(2.5, 1.8)" // new_line('a') // &
                "    r = max(2.5, 1.8)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ All-real min/max compiled successfully"
    else
        print *, "ERROR: Failed with all-real min/max"
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    print *, "Basic min/max semantic analysis tests passed!"
    
end program test_simple_semantic_minmax