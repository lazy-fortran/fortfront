program test_issue_182_function_params_multi_var
    ! Test for GitHub issue #182: Multi-variable declarations in function parameters
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #182: Function parameter parsing with result clauses ==="
    
    ! Test 1: Basic result clause parsing
    call test_result_clause_parsing()
    
    ! Test 2: Function with parameters and result clause
    call test_function_params_with_result()
    
    ! Test 3: Ensure no unparsed statements
    call test_no_unparsed_statements()
    
    print *, "All issue #182 tests completed"
    
contains

    subroutine test_result_clause_parsing()
        print *, "Testing result clause parsing..."
        
        source = "function simple() result(res)" // new_line('a') // &
                 "real :: res" // new_line('a') // &
                 "res = 42.0" // new_line('a') // &
                 "end function simple"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        if (index(output, "result(res)") == 0) then
            print *, "  FAIL: Result clause not preserved"
            stop 1
        end if
        
        print *, "  PASS: Result clause correctly parsed and preserved"
    end subroutine test_result_clause_parsing

    subroutine test_function_params_with_result()
        print *, "Testing function parameters with result clause..."
        
        source = "function calc(x, y) result(sum)" // new_line('a') // &
                 "real :: x, y, sum" // new_line('a') // &
                 "sum = x + y" // new_line('a') // &
                 "end function calc"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        ! Must preserve both parameters and result clause
        if (index(output, "calc(x, y)") == 0) then
            print *, "  FAIL: Function parameters not preserved"
            stop 1
        end if
        
        if (index(output, "result(sum)") == 0) then
            print *, "  FAIL: Result clause not preserved with parameters"
            stop 1
        end if
        
        print *, "  PASS: Function parameters and result clause both preserved"
    end subroutine test_function_params_with_result

    subroutine test_no_unparsed_statements()
        print *, "Testing elimination of unparsed statements..."
        
        source = "program test" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "function calc(x, y, z) result(res)" // new_line('a') // &
                 "real :: x, y, z, res" // new_line('a') // &
                 "res = x + y + z" // new_line('a') // &
                 "end function calc" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        if (index(output, "Unparsed statement") > 0) then
            print *, "  FAIL: Still generating unparsed statements"
            print *, "  Output: ", trim(output)
            stop 1
        end if
        
        print *, "  PASS: No unparsed statements generated"
    end subroutine test_no_unparsed_statements
    
end program test_issue_182_function_params_multi_var