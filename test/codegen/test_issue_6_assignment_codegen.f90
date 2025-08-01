program test_issue_6_assignment_codegen
    ! Test for GitHub issue #6: Assignment statements are dropped by emit_fortran
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: test_passed
    
    print *, "=== Issue #6: Assignment statements dropped in code generation ==="
    
    ! Test 1: Simple assignment should be preserved
    call test_simple_assignment()
    
    ! Test 2: Expression assignment should be preserved  
    call test_expression_assignment()
    
    ! Test 3: Multiple assignments should all be preserved
    call test_multiple_assignments()
    
    print *, "All issue #6 tests completed"
    
contains

    subroutine test_simple_assignment()
        print *, "Testing simple assignment preservation..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: x" // new_line('a') // &
                 "    x = 42.0" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        
        ! Check if assignment is preserved
        if (index(output, "x = 42.0") > 0 .or. index(output, "x = 42.0d0") > 0) then
            print *, "  PASS: Assignment statement preserved"
        else
            print *, "  FAIL: Assignment statement dropped"
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_simple_assignment
    
    subroutine test_expression_assignment()
        print *, "Testing expression assignment preservation..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: a = 1.0" // new_line('a') // &
                 "    real :: b = 2.0" // new_line('a') // &
                 "    real :: result" // new_line('a') // &
                 "    result = a * b" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        
        ! Check if assignment is preserved
        if (index(output, "result = a * b") > 0 .or. index(output, "result = a*b") > 0) then
            print *, "  PASS: Expression assignment preserved"
        else
            print *, "  FAIL: Expression assignment dropped"
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_expression_assignment
    
    subroutine test_multiple_assignments()
        print *, "Testing multiple assignments preservation..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    integer :: i, j, k" // new_line('a') // &
                 "    i = 1" // new_line('a') // &
                 "    j = 2" // new_line('a') // &
                 "    k = i + j" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        
        ! Check if all assignments are preserved
        test_passed = .true.
        if (index(output, "i = 1") == 0) then
            print *, "  FAIL: Assignment 'i = 1' dropped"
            test_passed = .false.
        end if
        if (index(output, "j = 2") == 0) then
            print *, "  FAIL: Assignment 'j = 2' dropped"
            test_passed = .false.
        end if
        if (index(output, "k = i + j") == 0 .and. index(output, "k = i+j") == 0) then
            print *, "  FAIL: Assignment 'k = i + j' dropped"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "  PASS: All assignments preserved"
        else
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_multiple_assignments
    
end program test_issue_6_assignment_codegen