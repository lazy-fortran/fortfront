program test_issue_175_function_result_clause
    ! Test for GitHub issue #175: Function result clause and intent attributes
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #175: Function Result Clause and Intent Attributes ==="
    
    ! Test the exact case from the issue
    call test_function_result_clause()
    
    print *, "Issue #175 test completed"
    
contains

    subroutine test_function_result_clause()
        print *, "Testing function result clause and intent attributes..."
        
        ! Exact case from the issue
        source = "program test" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "function calc(x, y) result(res)" // new_line('a') // &
                 "real :: x, y, res" // new_line('a') // &
                 "res = x + y" // new_line('a') // &
                 "end function calc" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        print *, "INPUT:"
        print *, trim(source)
        print *, ""
        print *, "OUTPUT:"
        print *, trim(output)
        print *, ""
        
        ! Check 1: Result clause preservation
        if (index(output, "result(res)") > 0) then
            print *, "  PASS: Result clause preserved in function signature"
        else
            print *, "  FAIL: Result clause missing from function signature"
        end if
        
        ! Check 2: Intent attributes - check if parameters don't have unwanted intent(in)
        if (index(output, "intent(in)") > 0) then
            print *, "  WARNING: Found intent(in) - parameters may have incorrect attributes"
        else
            print *, "  PASS: No automatic intent(in) attributes added"
        end if
        
        ! Check 3: Declaration duplication
        if (count_occurrences(output, ":: x") > 1) then
            print *, "  FAIL: Variable 'x' declared multiple times"
        else
            print *, "  PASS: Variable 'x' declared only once"
        end if
        
        if (count_occurrences(output, ":: y") > 1) then
            print *, "  FAIL: Variable 'y' declared multiple times"  
        else
            print *, "  PASS: Variable 'y' declared only once"
        end if
        
        ! Overall assessment
        if (index(output, "result(res)") > 0) then
            print *, "  GREAT NEWS: Result clause functionality works!"
            print *, "  This suggests Issue #175 is partially or fully resolved"
            if (count_occurrences(output, ":: x") == 1 .and. count_occurrences(output, ":: y") == 1) then
                print *, "  EXCELLENT: No duplicate declarations either!"
            else
                print *, "  NOTE: Some duplicate declarations remain"
            end if
        else
            print *, "  Issue #175 still exists - result clauses not preserved"
        end if
        
    end subroutine test_function_result_clause
    
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
    
end program test_issue_175_function_result_clause