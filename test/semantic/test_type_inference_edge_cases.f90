program test_type_inference_edge_cases
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed
    integer :: total_tests, passed_tests
    
    total_tests = 0
    passed_tests = 0
    
    print *, "=== Semantic Analysis Edge Case Tests ==="
    print *, ""
    
    ! Test 1: Recursive function type inference
    total_tests = total_tests + 1
    input = "fact(n) = if (n <= 1) then 1 else n * fact(n-1)"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. index(output, "recursive") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 1 - Recursive function:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 2: Variable shadowing across scopes
    total_tests = total_tests + 1
    input = "x = 42" // new_line('A') // &
            "function f()" // new_line('A') // &
            "  x = 3.14" // new_line('A') // &
            "  return x" // new_line('A') // &
            "end function"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. index(output, "integer :: x") > 0 .and. &
                   index(output, "real") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 2 - Variable shadowing:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 3: Complex array type inference
    total_tests = total_tests + 1
    input = "matrix = [[1, 2], [3.14, 4]]" // new_line('A') // &
            "result = matrix(1, 2) + matrix(2, 1)"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. index(output, "real") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 3 - Complex array inference:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 4: Function parameter type propagation
    total_tests = total_tests + 1
    input = "add(a, b) = a + b" // new_line('A') // &
            "result = add(1, 2.5)"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. index(output, "real") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 4 - Parameter propagation:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 5: Polymorphic type constraints
    total_tests = total_tests + 1
    input = "max_val(x, y) = if (x > y) then x else y" // new_line('A') // &
            "int_max = max_val(5, 3)" // new_line('A') // &
            "real_max = max_val(2.1, 3.7)"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. index(output, "integer") > 0 .and. &
                   index(output, "real") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 5 - Polymorphic constraints:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 6: Type unification with arrays
    total_tests = total_tests + 1
    input = "arr1 = [1, 2, 3]" // new_line('A') // &
            "arr2 = [4.0, 5.0, 6.0]" // new_line('A') // &
            "combined = arr1 + arr2"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (error_msg == "" .and. index(output, "real") > 0)
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 6 - Array type unification:", merge("PASSED", "FAILED", test_passed)
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
    
    if (passed_tests == total_tests) then
        print *, "All semantic analysis edge case tests passed!"
        stop 0
    else
        print *, "Some tests failed - semantic analysis needs improvement"
        stop 1
    end if
    
end program test_type_inference_edge_cases