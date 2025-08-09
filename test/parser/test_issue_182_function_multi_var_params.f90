program test_issue_182_function_multi_var_params
    ! RED PHASE: Test for GitHub issue #182
    ! Parser limitation: Multi-variable declarations in function parameters produce 'Unparsed statement'
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Testing Issue #182: Function Multi-Variable Parameter Declarations ==="
    
    ! Test case from the issue
    call test_multi_var_function_params()
    
    print *, "All Issue #182 tests completed"
    
contains

    subroutine test_multi_var_function_params()
        print *, "Testing multi-variable function parameter declarations..."
        
        ! Test case exactly as described in issue #182
        source = 'program test' // new_line('a') // &
                 'contains' // new_line('a') // &
                 'function calc(x, y, z) result(res)' // new_line('a') // &
                 'real :: x, y, z, res' // new_line('a') // &
                 'res = x + y + z' // new_line('a') // &
                 'end function calc' // new_line('a') // &
                 'end program test'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR: ", trim(error_msg)
            stop 1
        end if
        
        ! Check that there are no "Unparsed statement" comments
        if (index(output, "! Unparsed statement") > 0 .or. &
            index(output, "\! Unparsed statement") > 0) then
            print *, "  FAIL: Found 'Unparsed statement' in output"
            print *, "  Output: ", output
            stop 1
        end if
        
        ! Check that result clause is preserved (this is also part of issue #175)
        if (index(output, "result(res)") == 0) then
            print *, "  FAIL: Function result clause should be preserved"
            print *, "  Output: ", output
            stop 1
        end if
        
        ! Main issue #182 test: Check for duplicate declarations
        ! The bug creates both "real(8), intent(in) :: x, y, z" AND separate "real :: x", etc.
        if ((index(output, "real(8), intent(in) :: x, y, z") > 0 .or. &
             index(output, "real(8), intent(in) :: x, y, z") > 0) .and. &
            (index(output, "real :: x") > 0 .and. &
             index(output, "real :: y") > 0 .and. &
             index(output, "real :: z") > 0)) then
            print *, "  FAIL: Found duplicate variable declarations (both multi-var and individual)"
            print *, "  Output: ", output
            stop 1
        end if
        
        ! Check that variables are not declared multiple times in different ways
        if (count_occurrences(output, ":: x") > 1 .or. &
            count_occurrences(output, ":: y") > 1 .or. &
            count_occurrences(output, ":: z") > 1) then
            print *, "  FAIL: Variables declared multiple times"
            print *, "  Output: ", output
            stop 1
        end if
        
        print *, "  PASS: Multi-variable function parameters handled correctly"
    end subroutine test_multi_var_function_params

    integer function count_occurrences(text, pattern)
        character(len=*), intent(in) :: text, pattern
        integer :: pos, count
        count = 0
        pos = 1
        do
            pos = index(text(pos:), pattern)
            if (pos == 0) exit
            count = count + 1
            pos = pos + len(pattern)
        end do
        count_occurrences = count
    end function count_occurrences
    
end program test_issue_182_function_multi_var_params