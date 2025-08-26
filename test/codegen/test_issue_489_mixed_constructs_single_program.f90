program test_issue_489_mixed_constructs_single_program
    ! Test for GitHub Issue #489: Fix multiple program main blocks in code generation
    ! RED -> GREEN phase test
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    
    integer :: test_count, tests_passed
    
    test_count = 0
    tests_passed = 0
    
    print *, "=== Issue #489 Mixed Constructs Single Program Tests ==="
    
    call test_subroutine_then_call()
    call test_function_then_call()
    call test_multiple_subroutines_then_calls()
    call test_mixed_function_subroutine()
    call test_declarations_then_subroutines_then_calls()
    call test_complex_mixed_constructs()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", tests_passed, "/", test_count, " tests"
    
    if (tests_passed == test_count) then
        print *, "All mixed constructs tests passed!"
        stop 0
    else
        print *, "Some mixed constructs tests failed!"
        stop 1
    end if
    
contains

    subroutine test_subroutine_then_call()
        character(len=:), allocatable :: input_code, output_code
        character(len=:), allocatable :: error_msg
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, "Testing subroutine then call..."
        
        ! Input: subroutine definition followed by call
        input_code = "subroutine test()" // new_line('A') // &
                     "  call other()" // new_line('A') // &
                     "end subroutine" // new_line('A') // &
                     "call test()"
        
        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        
        if (error_msg /= '') then
            print *, "  ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
        else
            ! Should not contain multiple "program main" declarations
            if (count_occurrences(output_code, "program main") > 1) then
                print *, "  FAIL: Found multiple 'program main' declarations"
                print *, "  Output:", output_code
                test_passed = .false.
            end if
            
            ! Should not contain multiple "end program main" declarations
            if (count_occurrences(output_code, "end program main") > 1) then
                print *, "  FAIL: Found multiple 'end program main' declarations" 
                test_passed = .false.
            end if
            
            ! Should have exactly one "contains" section
            if (count_occurrences(output_code, "contains") /= 1) then
                print *, "  FAIL: Expected exactly one 'contains' section"
                test_passed = .false.
            end if
        end if
        
        if (test_passed) then
            tests_passed = tests_passed + 1
            print *, "  PASS"
        else
            print *, "  Output was:", output_code
        end if
    end subroutine test_subroutine_then_call

    subroutine test_function_then_call()
        character(len=:), allocatable :: input_code, output_code
        character(len=:), allocatable :: error_msg
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, "Testing function then call..."
        
        ! Input: function definition followed by call
        input_code = "integer function add(a, b)" // new_line('A') // &
                     "  integer :: a, b" // new_line('A') // &
                     "  add = a + b" // new_line('A') // &
                     "end function" // new_line('A') // &
                     "print *, add(1, 2)"
        
        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        
        if (error_msg /= '') then
            print *, "  ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
        else
            ! Should not contain multiple "program main" declarations
            if (count_occurrences(output_code, "program main") > 1) then
                print *, "  FAIL: Found multiple 'program main' declarations in function test"
                test_passed = .false.
            end if
            
            ! Should have exactly one "contains" section
            if (count_occurrences(output_code, "contains") /= 1) then
                print *, "  FAIL: Expected exactly one 'contains' section in function test"
                test_passed = .false.
            end if
        end if
        
        if (test_passed) then
            tests_passed = tests_passed + 1
            print *, "  PASS"
        else
            print *, "  Output was:", output_code
        end if
    end subroutine test_function_then_call

    subroutine test_multiple_subroutines_then_calls()
        character(len=:), allocatable :: input_code, output_code
        character(len=:), allocatable :: error_msg
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, "Testing multiple subroutines then calls..."
        
        ! Input: multiple subroutines followed by multiple calls
        input_code = "subroutine sub1()" // new_line('A') // &
                     "  print *, 'sub1'" // new_line('A') // &
                     "end subroutine" // new_line('A') // &
                     "subroutine sub2()" // new_line('A') // &
                     "  print *, 'sub2'" // new_line('A') // &
                     "end subroutine" // new_line('A') // &
                     "call sub1()" // new_line('A') // &
                     "call sub2()"
        
        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        
        if (error_msg /= '') then
            print *, "  ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
        else
            ! Should not contain multiple "program main" declarations
            if (count_occurrences(output_code, "program main") > 1) then
                print *, "  FAIL: Found multiple 'program main' declarations"
                test_passed = .false.
            end if
            
            ! Should have exactly one "contains" section
            if (count_occurrences(output_code, "contains") /= 1) then
                print *, "  FAIL: Expected exactly one 'contains' section"
                test_passed = .false.
            end if
        end if
        
        if (test_passed) then
            tests_passed = tests_passed + 1
            print *, "  PASS"
        else
            print *, "  Output was:", output_code
        end if
    end subroutine test_multiple_subroutines_then_calls

    subroutine test_mixed_function_subroutine()
        character(len=:), allocatable :: input_code, output_code
        character(len=:), allocatable :: error_msg
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, "Testing mixed function and subroutine..."
        
        ! Input: mix of functions and subroutines with calls
        input_code = "integer function calc(x)" // new_line('A') // &
                     "  integer :: x" // new_line('A') // &
                     "  calc = x * 2" // new_line('A') // &
                     "end function" // new_line('A') // &
                     "subroutine display(value)" // new_line('A') // &
                     "  integer :: value" // new_line('A') // &
                     "  print *, value" // new_line('A') // &
                     "end subroutine" // new_line('A') // &
                     "call display(calc(5))"
        
        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        
        if (error_msg /= '') then
            print *, "  ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
        else
            ! Should not contain multiple "program main" declarations
            if (count_occurrences(output_code, "program main") > 1) then
                print *, "  FAIL: Found multiple 'program main' declarations"
                test_passed = .false.
            end if
            
            ! Should have exactly one "contains" section
            if (count_occurrences(output_code, "contains") /= 1) then
                print *, "  FAIL: Expected exactly one 'contains' section"
                test_passed = .false.
            end if
        end if
        
        if (test_passed) then
            tests_passed = tests_passed + 1
            print *, "  PASS"
        else
            print *, "  Output was:", output_code
        end if
    end subroutine test_mixed_function_subroutine

    subroutine test_declarations_then_subroutines_then_calls()
        character(len=:), allocatable :: input_code, output_code
        character(len=:), allocatable :: error_msg
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, "Testing declarations then subroutines then calls..."
        
        ! Input: variable declarations, then subroutines, then executable statements
        input_code = "integer :: result" // new_line('A') // &
                     "subroutine compute()" // new_line('A') // &
                     "  result = 42" // new_line('A') // &
                     "end subroutine" // new_line('A') // &
                     "call compute()" // new_line('A') // &
                     "print *, result"
        
        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        
        if (error_msg /= '') then
            print *, "  ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
        else
            ! Should not contain multiple "program main" declarations
            if (count_occurrences(output_code, "program main") > 1) then
                print *, "  FAIL: Found multiple 'program main' declarations"
                test_passed = .false.
            end if
            
            ! Should have exactly one "contains" section
            if (count_occurrences(output_code, "contains") /= 1) then
                print *, "  FAIL: Expected exactly one 'contains' section"
                test_passed = .false.
            end if
        end if
        
        if (test_passed) then
            tests_passed = tests_passed + 1
            print *, "  PASS"
        else
            print *, "  Output was:", output_code
        end if
    end subroutine test_declarations_then_subroutines_then_calls

    subroutine test_complex_mixed_constructs()
        character(len=:), allocatable :: input_code, output_code
        character(len=:), allocatable :: error_msg
        logical :: test_passed
        
        test_count = test_count + 1
        test_passed = .true.
        
        print *, "Testing complex mixed constructs..."
        
        ! Input: complex mix with multiple types of constructs
        input_code = "integer :: n = 10" // new_line('A') // &
                     "real :: data(10)" // new_line('A') // &
                     "subroutine init_data()" // new_line('A') // &
                     "  integer :: i" // new_line('A') // &
                     "  do i = 1, n" // new_line('A') // &
                     "    data(i) = real(i)" // new_line('A') // &
                     "  end do" // new_line('A') // &
                     "end subroutine" // new_line('A') // &
                     "real function average()" // new_line('A') // &
                     "  average = sum(data) / n" // new_line('A') // &
                     "end function" // new_line('A') // &
                     "call init_data()" // new_line('A') // &
                     "print *, 'Average:', average()"
        
        call transform_lazy_fortran_string(input_code, output_code, error_msg)
        
        if (error_msg /= '') then
            print *, "  ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
        else
            ! Should not contain multiple "program main" declarations
            if (count_occurrences(output_code, "program main") > 1) then
                print *, "  FAIL: Found multiple 'program main' declarations"
                test_passed = .false.
            end if
            
            ! Should have exactly one "contains" section
            if (count_occurrences(output_code, "contains") /= 1) then
                print *, "  FAIL: Expected exactly one 'contains' section"
                test_passed = .false.
            end if
        end if
        
        if (test_passed) then
            tests_passed = tests_passed + 1
            print *, "  PASS"
        else
            print *, "  Output was:", output_code
        end if
    end subroutine test_complex_mixed_constructs

    ! Helper function to count occurrences of a substring
    integer function count_occurrences(text, pattern)
        character(len=*), intent(in) :: text, pattern
        integer :: pos, next_pos, count_val
        
        count_val = 0
        pos = 1
        
        do
            next_pos = index(text(pos:), pattern)
            if (next_pos == 0) exit
            count_val = count_val + 1
            pos = pos + next_pos - 1 + len(pattern)
        end do
        
        count_occurrences = count_val
    end function count_occurrences

    ! Helper function to normalize whitespace for comparison
    function normalize_whitespace(text) result(normalized)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: normalized
        integer :: i, j
        character(len=len(text)) :: temp
        logical :: was_space
        
        j = 1
        was_space = .false.
        
        do i = 1, len(text)
            if (text(i:i) == ' ' .or. text(i:i) == char(9) .or. &
                text(i:i) == char(10) .or. text(i:i) == char(13)) then
                if (.not. was_space .and. j > 1) then
                    temp(j:j) = ' '
                    j = j + 1
                end if
                was_space = .true.
            else
                temp(j:j) = text(i:i)
                j = j + 1
                was_space = .false.
            end if
        end do
        
        normalized = trim(temp(1:j-1))
    end function normalize_whitespace

end program test_issue_489_mixed_constructs_single_program