program test_issue_176_variable_initialization
    ! Test for GitHub issue #176: Variable initialization values removed
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #176: Variable Initialization Values Removed ==="
    
    ! Test the exact case from the issue
    call test_variable_initialization()
    
    print *, "Issue #176 test completed"
    
contains

    subroutine test_variable_initialization()
        print *, "Testing variable initialization preservation..."
        
        source = "program test" // new_line('a') // &
                 "implicit none" // new_line('a') // &
                 "integer :: x = 42" // new_line('a') // &
                 "real :: y = 3.14" // new_line('a') // &
                 "logical :: flag = .true." // new_line('a') // &
                 "integer :: arr(3) = [1, 2, 3]" // new_line('a') // &
                 "integer, parameter :: N = 10" // new_line('a') // &
                 "print *, x, y, flag, arr, N" // new_line('a') // &
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
        
        ! Check for integer initialization
        if (index(output, "= 42") == 0) then
            print *, "  FAIL: Integer initialization (= 42) was removed"
        else
            print *, "  PASS: Integer initialization preserved"
        end if
        
        ! Check for real initialization  
        if (index(output, "= 3.14") == 0) then
            print *, "  FAIL: Real initialization (= 3.14) was removed"
        else
            print *, "  PASS: Real initialization preserved"
        end if
        
        ! Check for logical initialization
        if (index(output, "= .true.") == 0) then
            print *, "  FAIL: Logical initialization (= .true.) was removed"
        else
            print *, "  PASS: Logical initialization preserved"
        end if
        
        ! Check for array initialization
        if (index(output, "= [1, 2, 3]") == 0) then
            print *, "  FAIL: Array initialization (= [1, 2, 3]) was removed"
        else
            print *, "  PASS: Array initialization preserved"
        end if
        
        ! Check for parameter initialization
        if (index(output, "= 10") == 0) then
            print *, "  FAIL: Parameter initialization (= 10) was removed"
        else
            print *, "  PASS: Parameter initialization preserved"
        end if
        
        ! Overall assessment
        if (index(output, "= 42") == 0 .or. &
            index(output, "= 3.14") == 0 .or. &
            index(output, "= .true.") == 0 .or. &
            index(output, "= [1, 2, 3]") == 0 .or. &
            index(output, "= 10") == 0) then
            print *, "  OVERALL RESULT: Initialization values removed (CRITICAL ISSUE CONFIRMED)"
            print *, "  "
            print *, "  ROOT CAUSE: Parser does not capture initialization expressions"
            print *, "  REQUIRED FIXES:"
            print *, "  - Update declaration parsing to capture '= expr' parts"
            print *, "  - Extend AST declaration nodes to store init expressions"
            print *, "  - Update codegen to emit initialization values"
            print *, "  "  
            print *, "  IMPACT: Changes program semantics - variables lose default values"
            print *, "  SEVERITY: HIGH - can cause runtime errors and incorrect behavior"
        else
            print *, "  OVERALL RESULT: All initialization values preserved - issue may be fixed!"
        end if
        
    end subroutine test_variable_initialization
    
end program test_issue_176_variable_initialization