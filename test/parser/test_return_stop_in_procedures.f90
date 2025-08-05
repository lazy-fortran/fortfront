program test_return_stop_in_procedures
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    
    print *, "Testing return and stop statements in various procedure contexts..."
    
    ! Test 1: Return in subroutine
    test_code = "subroutine test_return()" // new_line('a') // &
                "  if (.true.) then" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "end subroutine test_return"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0 .and. index(output_code, "return") > 0) then
        print *, "✓ Return statement in subroutine parsed correctly"
    else
        print *, "ERROR: Failed to parse return in subroutine"
        if (len(error_msg) > 0) print *, "Error:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 2: Stop in subroutine
    test_code = "subroutine test_stop()" // new_line('a') // &
                "  if (.true.) then" // new_line('a') // &
                "    stop 'Error occurred'" // new_line('a') // &
                "  end if" // new_line('a') // &
                "end subroutine test_stop"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0 .and. index(output_code, "stop") > 0) then
        print *, "✓ Stop statement in subroutine parsed correctly"
    else
        print *, "ERROR: Failed to parse stop in subroutine"
        if (len(error_msg) > 0) print *, "Error:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 3: Return in function
    test_code = "function test_func(x) result(y)" // new_line('a') // &
                "  real :: x, y" // new_line('a') // &
                "  y = x * 2.0" // new_line('a') // &
                "  return" // new_line('a') // &
                "end function test_func"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0 .and. index(output_code, "return") > 0) then
        print *, "✓ Return statement in function parsed correctly"
    else
        print *, "ERROR: Failed to parse return in function"
        if (len(error_msg) > 0) print *, "Error:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 4: Multiple returns with control flow
    test_code = "subroutine complex_return(flag)" // new_line('a') // &
                "  logical :: flag" // new_line('a') // &
                "  if (flag) then" // new_line('a') // &
                "    print *, 'Early return'" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  print *, 'Normal flow'" // new_line('a') // &
                "  return" // new_line('a') // &
                "end subroutine complex_return"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        ! Count occurrences of 'return'
        block
            integer :: pos, count
            count = 0
            pos = 1
            do while (pos > 0)
                pos = index(output_code(pos:), "return")
                if (pos > 0) then
                    count = count + 1
                    pos = pos + 6  ! Move past "return"
                end if
            end do
            if (count >= 2) then
                print *, "✓ Multiple return statements parsed correctly"
            else
                print *, "ERROR: Expected multiple return statements, found", count
                error stop 1
            end if
        end block
    else
        print *, "ERROR: Failed to parse complex return"
        if (len(error_msg) > 0) print *, "Error:", trim(error_msg)
        error stop 1
    end if
    
    ! Test 5: Stop with different codes
    test_code = "subroutine test_stop_codes()" // new_line('a') // &
                "  stop" // new_line('a') // &
                "  stop 1" // new_line('a') // &
                "  stop 'Error message'" // new_line('a') // &
                "end subroutine test_stop_codes"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Various stop statement forms parsed correctly"
    else
        print *, "ERROR: Failed to parse stop statements"
        if (len(error_msg) > 0) print *, "Error:", trim(error_msg)
        error stop 1
    end if
    
    print *, "All return/stop statement tests passed!"
    
end program test_return_stop_in_procedures