program test_issue_1664_result_keywords
    implicit none
    
    if (.not. test_result_with_keyword_name()) then
        error stop "Test failed: result clause with keyword name"
    end if
    
    print *, "PASS: Issue #1664 - result clause with keyword names"
    
contains

    function test_result_with_keyword_name() result(passed)
        logical :: passed
        integer :: result_val
        
        ! Test function with result variable named 'in' (a keyword)
        result_val = func_with_in_result()
        passed = (result_val == 42)
        
    end function test_result_with_keyword_name
    
    function func_with_in_result() result(in)
        integer :: in
        in = 42
    end function func_with_in_result

end program test_issue_1664_result_keywords
