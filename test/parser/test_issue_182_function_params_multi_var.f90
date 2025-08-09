program test_issue_182_function_params_multi_var
    ! Test for GitHub issue #182: Multi-variable declarations in function parameters
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: test_passed
    
    print *, "=== Issue #182: Multi-variable declarations in function parameters ==="
    
    ! Test the exact case from the issue
    call test_function_param_multi_var()
    
    print *, "Issue #182 test completed"
    
contains

    subroutine test_function_param_multi_var()
        print *, "Testing function with multi-variable parameter declarations..."
        
        source = "program test" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "function calc(x, y, z) result(res)" // new_line('a') // &
                 "real :: x, y, z, res" // new_line('a') // &
                 "res = x + y + z" // new_line('a') // &
                 "end function calc" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Check that output does NOT contain "Unparsed statement"
        if (index(output, "Unparsed statement") > 0) then
            print *, "  FAIL: Found 'Unparsed statement' in output"
            print *, "  Output: ", trim(output)
            stop 1
        end if
        
        ! Check that result clause is preserved
        if (index(output, "result(res)") == 0) then
            print *, "  FAIL: Result clause missing from function signature"
            print *, "  Output: ", trim(output)
            stop 1
        end if
        
        ! Check that variables are properly declared without duplication
        test_passed = .true.
        
        ! Count occurrences of variable declarations
        if (count_occurrences(output, ":: x") > 1) then
            print *, "  FAIL: Variable 'x' declared multiple times"
            test_passed = .false.
        end if
        
        if (count_occurrences(output, ":: y") > 1) then
            print *, "  FAIL: Variable 'y' declared multiple times"
            test_passed = .false.
        end if
        
        if (count_occurrences(output, ":: z") > 1) then
            print *, "  FAIL: Variable 'z' declared multiple times"
            test_passed = .false.
        end if
        
        if (count_occurrences(output, ":: res") > 1) then
            print *, "  FAIL: Variable 'res' declared multiple times"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "  PASS: Multi-variable function parameter declarations work correctly"
        else
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_function_param_multi_var
    
    function count_occurrences(text, pattern) result(count)
        character(len=*), intent(in) :: text, pattern
        integer :: count
        integer :: pos, start_pos
        
        count = 0
        start_pos = 1
        
        do
            pos = index(text(start_pos:), pattern)
            if (pos == 0) exit
            count = count + 1
            start_pos = start_pos + pos + len(pattern) - 1
        end do
    end function count_occurrences
    
end program test_issue_182_function_params_multi_var