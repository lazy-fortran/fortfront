program test_error_recovery
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed
    integer :: total_tests, passed_tests
    
    total_tests = 0
    passed_tests = 0
    
    print *, "=== Parser Error Recovery Tests ==="
    print *, ""
    
    ! Test 1: Incomplete control structures
    total_tests = total_tests + 1
    input = "if (x > 5) then" // new_line('A') // &
            "  print *, 'greater'" // new_line('A') // &
            "! missing end if"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (len_trim(output) > 0)  ! Should recover gracefully
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 1 - Missing end if:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 2: Malformed array syntax
    total_tests = total_tests + 1
    input = "arr = [1, 2, 3" // new_line('A') // &
            "print *, arr"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (len_trim(output) > 0)  ! Should handle unclosed bracket
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 2 - Unclosed bracket:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 3: Invalid operator sequences
    total_tests = total_tests + 1
    input = "x = 1 + + 2" // new_line('A') // &
            "y = 3"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (len_trim(output) > 0)  ! Should recover from operator error
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 3 - Double operator:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 4: Partial function definitions
    total_tests = total_tests + 1
    input = "function incomplete(" // new_line('A') // &
            "x = 42"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (len_trim(output) > 0)  ! Should handle incomplete function
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 4 - Incomplete function:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 5: Mixed syntax styles
    total_tests = total_tests + 1
    input = "INTEGER X" // new_line('A') // &  ! F77 style
            "real(8) :: y = 3.14" // new_line('A') // &  ! F90 style
            "X = 5"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (len_trim(output) > 0)  ! Should handle mixed styles
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 5 - Mixed F77/F90:", merge("PASSED", "FAILED", test_passed)
    
    ! Test 6: Nested incomplete structures
    total_tests = total_tests + 1
    input = "if (a > 0) then" // new_line('A') // &
            "  do i = 1, 10" // new_line('A') // &
            "    print *, i" // new_line('A') // &
            "  ! missing end do and end if"
    call transform_lazy_fortran_string(input, output, error_msg)
    test_passed = (len_trim(output) > 0)  ! Should recover from nested errors
    if (test_passed) passed_tests = passed_tests + 1
    print *, "Test 6 - Nested incomplete:", merge("PASSED", "FAILED", test_passed)
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
    
    if (passed_tests == total_tests) then
        print *, "All parser error recovery tests passed!"
        stop 0
    else
        print *, "Some tests failed - parser recovery needs improvement"
        stop 1
    end if
    
end program test_error_recovery