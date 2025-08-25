program test_issue_182_comprehensive
    ! Comprehensive test for GitHub issue #182: Multi-variable declarations in function parameters
    ! Combines all aspects: result clauses, multi-variable parameters, unparsed statements
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #182 Comprehensive: Function Multi-Variable Parameters ==="
    
    ! Test all aspects in order of complexity
    call test_basic_result_clause()
    call test_params_with_result()
    call test_multi_var_without_duplicates()
    call test_no_unparsed_statements()
    
    print *, "All Issue #182 comprehensive tests passed"
    
contains

    subroutine test_basic_result_clause()
        print *, "Testing basic result clause parsing..."
        
        source = "function simple() result(res)" // new_line('a') // &
                 "real :: res" // new_line('a') // &
                 "res = 42.0" // new_line('a') // &
                 "end function simple"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg, "Basic result clause")
        call assert_contains(output, "result(res)", "Result clause preservation")
        
        print *, "  PASS: Basic result clause parsing"
    end subroutine test_basic_result_clause

    subroutine test_params_with_result()
        print *, "Testing function parameters with result clause..."
        
        source = "function calc(x, y) result(sum)" // new_line('a') // &
                 "real :: x, y, sum" // new_line('a') // &
                 "sum = x + y" // new_line('a') // &
                 "end function calc"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg, "Parameters with result clause")
        call assert_contains(output, "calc(x, y)", "Function parameters preservation")
        call assert_contains(output, "result(sum)", "Result clause preservation with params")
        
        print *, "  PASS: Parameters with result clause"
    end subroutine test_params_with_result

    subroutine test_multi_var_without_duplicates()
        print *, "Testing multi-variable declarations without duplicates..."
        
        source = 'program test' // new_line('a') // &
                 'contains' // new_line('a') // &
                 'function calc(x, y, z) result(res)' // new_line('a') // &
                 'real :: x, y, z, res' // new_line('a') // &
                 'res = x + y + z' // new_line('a') // &
                 'end function calc' // new_line('a') // &
                 'end program test'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg, "Multi-variable declarations")
        call assert_contains(output, "result(res)", "Result clause in multi-var context")
        
        ! Main issue #182 check: No duplicate declarations
        if ((index(output, "real(8), intent(in) :: x, y, z") > 0 .or. &
             index(output, "real, intent(in) :: x, y, z") > 0) .and. &
            (index(output, "real :: x") > 0 .and. &
             index(output, "real :: y") > 0 .and. &
             index(output, "real :: z") > 0)) then
            print *, "  FAIL: Found duplicate variable declarations"
            print *, "  Output: ", output
            stop 1
        end if
        
        ! Check no variable is declared multiple times
        if (count_occurrences(output, ":: x") > 1 .or. &
            count_occurrences(output, ":: y") > 1 .or. &
            count_occurrences(output, ":: z") > 1) then
            print *, "  FAIL: Variables declared multiple times"
            print *, "  Output: ", output
            stop 1
        end if
        
        print *, "  PASS: Multi-variable declarations without duplicates"
    end subroutine test_multi_var_without_duplicates

    subroutine test_no_unparsed_statements()
        print *, "Testing elimination of unparsed statements..."
        
        source = "program complex_test" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "function multi_calc(a, b, c, d) result(total)" // new_line('a') // &
                 "real :: a, b, c, d, total" // new_line('a') // &
                 "total = a + b * c - d" // new_line('a') // &
                 "end function multi_calc" // new_line('a') // &
                 "end program complex_test"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        call assert_no_error(error_msg, "Complex multi-variable function")
        call assert_not_contains(output, "! Unparsed statement", "No unparsed statements")
        call assert_not_contains(output, "\! Unparsed statement", "No unparsed statements (escaped)")
        
        print *, "  PASS: No unparsed statements generated"
    end subroutine test_no_unparsed_statements

    ! Helper routines
    subroutine assert_no_error(error_msg, test_name)
        character(len=:), allocatable, intent(in) :: error_msg
        character(len=*), intent(in) :: test_name
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR in ", test_name, ": ", trim(error_msg)
            stop 1
        end if
    end subroutine assert_no_error

    subroutine assert_contains(text, pattern, test_name)
        character(len=*), intent(in) :: text, pattern, test_name
        if (index(text, pattern) == 0) then
            print *, "  FAIL in ", test_name, ": Expected to find '", pattern, "'"
            print *, "  Output: ", text
            stop 1
        end if
    end subroutine assert_contains

    subroutine assert_not_contains(text, pattern, test_name)
        character(len=*), intent(in) :: text, pattern, test_name
        if (index(text, pattern) > 0) then
            print *, "  FAIL in ", test_name, ": Should not find '", pattern, "'"
            print *, "  Output: ", text
            stop 1
        end if
    end subroutine assert_not_contains

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
    
end program test_issue_182_comprehensive