program test_semantic_preservation
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed
    integer :: total_tests, passed_tests
    
    total_tests = 0
    passed_tests = 0
    
    print *, "=== AST Transformation Correctness Tests ==="
    print *, ""
    
    ! Test 1: Type information preservation
    total_tests = total_tests + 1
    input = "x = 42" // new_line('A') // &
            "y = 3.14"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. &
                   index(output, "integer :: x") > 0 .and. &
                   index(output, "real") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 1 - Type preservation:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 2: Variable scope correctness
    total_tests = total_tests + 1
    input = "x = 5" // new_line('A') // &
            "function test()" // new_line('A') // &
            "  y = 10" // new_line('A') // &
            "  return y" // new_line('A') // &
            "end function"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. &
                   index(output, "integer :: x") > 0 .and. &
                   index(output, "integer :: y") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 2 - Scope preservation:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 3: Expression evaluation order
    total_tests = total_tests + 1
    input = "result = a + b * c"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. &
                   index(output, "result = a + b * c") > 0)  ! Should preserve precedence
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 3 - Expression order:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 4: Control flow integrity
    total_tests = total_tests + 1
    input = "if (x > 0) then" // new_line('A') // &
            "  y = x" // new_line('A') // &
            "else" // new_line('A') // &
            "  y = -x" // new_line('A') // &
            "end if"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. &
                   index(output, "if (x > 0) then") > 0 .and. &
                   index(output, "else") > 0 .and. &
                   index(output, "end if") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 4 - Control flow:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 5: Array operations preservation
    total_tests = total_tests + 1
    input = "arr = [1, 2, 3]" // new_line('A') // &
            "sum_val = sum(arr)"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. &
                   index(output, "sum(arr)") > 0)  ! Function call preserved
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 5 - Array operations:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 6: Variable declaration consolidation
    total_tests = total_tests + 1
    input = "a = 1" // new_line('A') // &
            "b = 2" // new_line('A') // &
            "c = 3"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. &
                   count_substr(output, "integer ::") <= 1)  ! Should consolidate declarations
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 6 - Declaration consolidation:", merge("PASSED", "FAILED", test_passed)
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
    
    if (passed_tests == total_tests) then
        print *, "All AST transformation correctness tests passed!"
        stop 0
    else
        print *, "Some tests failed - AST transformation needs improvement"
        stop 1
    end if

contains

    !> Count occurrences of substring in string
    function count_substr(str, substr) result(count)
        character(len=*), intent(in) :: str, substr
        integer :: count
        integer :: pos, start
        
        count = 0
        start = 1
        
        do
            pos = index(str(start:), substr)
            if (pos == 0) exit
            count = count + 1
            start = start + pos + len(substr) - 1
        end do
    end function count_substr
    
end program test_semantic_preservation