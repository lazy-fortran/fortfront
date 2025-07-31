program test_issue_5_multi_var_declarations
    ! Test for GitHub issue #5: Multi-variable declarations not parsed correctly
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: test_passed
    integer :: var_count
    
    print *, "=== Issue #5: Multi-variable declarations not parsed correctly ==="
    
    ! Test 1: Simple multi-variable declaration
    call test_simple_multi_var()
    
    ! Test 2: Multi-variable with different types
    call test_mixed_multi_var()
    
    ! Test 3: Multi-variable with arrays
    call test_array_multi_var()
    
    ! Test 4: Multi-variable preserves usage in assignments
    call test_multi_var_usage()
    
    print *, "All issue #5 tests completed"
    
contains

    subroutine test_simple_multi_var()
        print *, "Testing simple multi-variable declaration..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: a, b, c" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Count how many variables are declared
        var_count = 0
        if (index(output, ":: a") > 0 .or. index(output, " a,") > 0 .or. index(output, " a" // new_line('a')) > 0) var_count = var_count + 1
        if (index(output, " b") > 0) var_count = var_count + 1
        if (index(output, " c") > 0) var_count = var_count + 1
        
        if (var_count == 3) then
            print *, "  PASS: All 3 variables preserved"
        else
            print *, "  FAIL: Only ", var_count, " variables preserved (expected 3)"
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_simple_multi_var
    
    subroutine test_mixed_multi_var()
        print *, "Testing mixed multi-variable declarations..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    integer :: i, j, k" // new_line('a') // &
                 "    real :: x, y, z" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Check if all integer variables are preserved
        test_passed = .true.
        if (index(output, " i") == 0) then
            print *, "  FAIL: Variable 'i' not found"
            test_passed = .false.
        end if
        if (index(output, " j") == 0) then
            print *, "  FAIL: Variable 'j' not found"
            test_passed = .false.
        end if
        if (index(output, " k") == 0) then
            print *, "  FAIL: Variable 'k' not found"
            test_passed = .false.
        end if
        
        ! Check if all real variables are preserved
        if (index(output, " x") == 0) then
            print *, "  FAIL: Variable 'x' not found"
            test_passed = .false.
        end if
        if (index(output, " y") == 0) then
            print *, "  FAIL: Variable 'y' not found"
            test_passed = .false.
        end if
        if (index(output, " z") == 0) then
            print *, "  FAIL: Variable 'z' not found"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "  PASS: All variables preserved"
        else
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_mixed_multi_var
    
    subroutine test_array_multi_var()
        print *, "Testing multi-variable with arrays..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: arr1(10), arr2(20), scalar" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Check if all variables are preserved
        test_passed = .true.
        if (index(output, "arr1") == 0) then
            print *, "  FAIL: Array 'arr1' not found"
            test_passed = .false.
        end if
        if (index(output, "arr2") == 0) then
            print *, "  FAIL: Array 'arr2' not found"
            test_passed = .false.
        end if
        if (index(output, "scalar") == 0) then
            print *, "  FAIL: Variable 'scalar' not found"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "  PASS: All variables including arrays preserved"
        else
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_array_multi_var
    
    subroutine test_multi_var_usage()
        print *, "Testing multi-variable usage in assignments..."
        
        source = "program test" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    real :: result, a, b, c, d" // new_line('a') // &
                 "    result = a * b + c * d" // new_line('a') // &
                 "end program test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Check if all variables are preserved
        test_passed = .true.
        var_count = 0
        if (index(output, "result") > 0) var_count = var_count + 1
        if (index(output, " a") > 0) var_count = var_count + 1
        if (index(output, " b") > 0) var_count = var_count + 1
        if (index(output, " c") > 0) var_count = var_count + 1
        if (index(output, " d") > 0) var_count = var_count + 1
        
        if (var_count /= 5) then
            print *, "  FAIL: Only ", var_count, " variables preserved (expected 5)"
            test_passed = .false.
        end if
        
        ! Check if assignment is preserved
        if (index(output, "result = a * b + c * d") == 0 .and. &
            index(output, "result = a*b + c*d") == 0 .and. &
            index(output, "result = a*b+c*d") == 0) then
            print *, "  FAIL: Assignment statement not preserved"
            test_passed = .false.
        end if
        
        if (test_passed) then
            print *, "  PASS: All variables and assignment preserved"
        else
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_multi_var_usage
    
end program test_issue_5_multi_var_declarations