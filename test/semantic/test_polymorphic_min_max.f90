program test_polymorphic_min_max
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code, output_code, error_msg
    
    print *, "Testing polymorphic min/max functions with type promotion..."
    
    ! Test 1: Mixed integer and real -> real result (key polymorphic test)
    test_code = "program test_mixed_min_max" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    real :: r_result" // new_line('a') // &
                "    r_result = min(1, 2.0)" // new_line('a') // &
                "    r_result = max(1, 2.0)" // new_line('a') // &
                "    print *, 'Mixed type min/max test passed!'" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Mixed integer/real min/max compiled successfully"
        print *, "Generated code:"
        print *, trim(output_code)
    else
        print *, "ERROR: Failed to handle mixed-type min/max"
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 2: Real and integer (other order)
    test_code = "program test_real_int_order" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    real :: r" // new_line('a') // &
                "    r = min(2.0, 1)" // new_line('a') // &
                "    r = max(2.0, 1)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Real-integer order min/max compiled successfully"
    else
        print *, "ERROR: Failed to handle real-integer order min/max"
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 3: Multiple mixed arguments
    test_code = "program test_multiple_mixed" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    real :: r" // new_line('a') // &
                "    r = min(5, 2.3, 1, 4.7)" // new_line('a') // &
                "    r = max(5, 2.3, 1, 4.7)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Multiple mixed arguments min/max compiled successfully"
    else
        print *, "ERROR: Failed to handle multiple mixed arguments"
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 4: All integers (should remain integer)
    test_code = "program test_all_integers" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    integer :: i" // new_line('a') // &
                "    i = min(5, 3, 8, 1)" // new_line('a') // &
                "    i = max(5, 3, 8, 1)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ All integers min/max compiled successfully"
    else
        print *, "ERROR: Failed to handle all-integer arguments"
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 5: All reals (should remain real)
    test_code = "program test_all_reals" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    real :: r" // new_line('a') // &
                "    r = min(2.5, 1.8, 3.2, 0.9)" // new_line('a') // &
                "    r = max(2.5, 1.8, 3.2, 0.9)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ All reals min/max compiled successfully"
    else
        print *, "ERROR: Failed to handle all-real arguments"  
        print *, "Error message:", trim(error_msg)
        error stop 1
    end if
    
    print *, "All polymorphic min/max semantic analysis tests passed!"
    
end program test_polymorphic_min_max