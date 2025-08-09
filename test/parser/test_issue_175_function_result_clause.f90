program test_issue_175_function_result_clause
    ! Test for GitHub issue #175: Function result clause preservation
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #175: Function result clause preservation ==="
    
    call test_result_clause_with_params()
    
    print *, "Issue #175 test completed"
    
contains

    subroutine test_result_clause_with_params()
        print *, "Testing function with parameters and result clause..."
        
        source = "function calc(x, y) result(res)" // new_line('a') // &
                 "real :: x, y, res" // new_line('a') // &
                 "res = x + y" // new_line('a') // &
                 "end function calc"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        ! Main issue: result clause must be preserved
        if (index(output, "result(res)") == 0) then
            print *, "  FAIL: Result clause removed from function signature"
            stop 1
        end if
        
        print *, "  PASS: Result clause correctly preserved"
        
        ! Check that function parameters are preserved
        if (index(output, "calc(x, y)") == 0) then
            print *, "  FAIL: Function parameters not preserved"
            stop 1
        end if
        
        print *, "  PASS: Function parameters preserved with result clause"
        
    end subroutine test_result_clause_with_params
    
end program test_issue_175_function_result_clause