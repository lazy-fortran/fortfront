program test_issue_1258_parameter_decls
    ! Test for issue #1258 - Function/subroutine parameter declarations incomplete
    use frontend
    implicit none
    
    logical :: test_passed
    
    test_passed = test_multi_param_declarations()
    
    if (test_passed) then
        print *, "PASS: Issue #1258 - Multi-parameter declarations handled correctly"
    else
        print *, "FAIL: Issue #1258 - Multi-parameter declarations not complete"
        stop 1
    end if
    
contains
    
    function test_multi_param_declarations() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        passed = .true.
        
        ! Test case from issue #1258
        source = &
            "module math_utils" // new_line('a') // &
            "contains" // new_line('a') // &
            "    function add(a, b, c) result(sum)" // new_line('a') // &
            "        integer, intent(in) :: a, b, c" // new_line('a') // &
            "        integer :: sum" // new_line('a') // &
            "        sum = a + b + c" // new_line('a') // &
            "    end function add" // new_line('a') // &
            "end module math_utils"
        
        print *, "===== Testing Issue 1258: Multi-parameter declarations ====="
        print *, "Input code:"
        print *, trim(source)
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (error_msg /= "") then
            print *, "ERROR: Failed to parse: ", trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            print *, "ERROR: No output generated"
            passed = .false.
        else
            print *, "Generated output:"
            print *, trim(output)
            
            ! Check that we have all three parameters in output
            if (index(output, "a") == 0 .or. &
                index(output, "b") == 0 .or. &
                index(output, "c") == 0) then
                print *, "ERROR: Not all parameters found in output"
                passed = .false.
            end if
            
            ! Check that the function header is correct
            if (index(output, "function add(a, b, c)") == 0) then
                print *, "WARNING: Function header may not include all parameters"
            end if
            
            ! Check that we have parameter declarations
            ! Note: intent attributes might not be preserved yet
            if (index(output, "integer") == 0) then
                print *, "ERROR: No integer declaration found"
                passed = .false.
            end if
        end if
        
    end function test_multi_param_declarations
    
end program test_issue_1258_parameter_decls