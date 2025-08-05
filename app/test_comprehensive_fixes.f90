program test_comprehensive_fixes
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(format_options_t) :: options
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Comprehensive Fix Testing ==="
    print *, ""
    
    ! Test 1: Issue #96 - Complex mathematical expressions
    print *, "Test 1: Complex mathematical expressions (Issue #96)"
    input_code = 'program test_math' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    real :: a = 1.0, b = 2.0, c = 3.0' // new_line('A') // &
                 '    real :: result' // new_line('A') // &
                 '    result = (a + b) * c / (a - b) + sqrt(a**2 + b**2)' // new_line('A') // &
                 '    result = sin(a) + cos(b) + tan(c)' // new_line('A') // &
                 '    result = exp(a) * log(b) + abs(c)' // new_line('A') // &
                 'end program test_math'
    
    call transform_lazy_fortran_string_with_format(input_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else
        print *, "  PASSED: Complex mathematical expressions work"
    end if
    print *, ""
    
    ! Test 2: Issue #98 - Program structure preservation
    print *, "Test 2: Program structure preservation (Issue #98)"
    input_code = 'program test_structure' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: x = 42' // new_line('A') // &
                 '    print *, x' // new_line('A') // &
                 'end program test_structure'
    
    call transform_lazy_fortran_string_with_format(input_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else if (index(output_code, "program test_structure") == 0 .or. &
             index(output_code, "end program test_structure") == 0) then
        print *, "  FAILED: Program structure not preserved"
        all_tests_passed = .false.
    else
        print *, "  PASSED: Program structure preserved correctly"
    end if
    print *, ""
    
    ! Test 3: Variable usage in conditionals (Issue #99 related)
    print *, "Test 3: Variable usage in conditionals"
    input_code = 'program test_conditionals' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    integer :: x = 5' // new_line('A') // &
                 '    if (x > 0) then' // new_line('A') // &
                 '        print *, "positive"' // new_line('A') // &
                 '    else if (x < 0) then' // new_line('A') // &
                 '        print *, "negative"' // new_line('A') // &
                 '    else' // new_line('A') // &
                 '        print *, "zero"' // new_line('A') // &
                 '    end if' // new_line('A') // &
                 'end program test_conditionals'
    
    call transform_lazy_fortran_string_with_format(input_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else
        print *, "  PASSED: Conditional expressions work"
    end if
    print *, ""
    
    ! Test 4: Mixed types and complex expressions
    print *, "Test 4: Mixed types and complex expressions"
    input_code = 'program test_mixed' // new_line('A') // &
                 '    implicit none' // new_line('A') // &
                 '    real :: r = 3.14159' // new_line('A') // &
                 '    integer :: i = 42' // new_line('A') // &
                 '    logical :: flag = .true.' // new_line('A') // &
                 '    character(len=10) :: msg = "hello"' // new_line('A') // &
                 '    real :: result' // new_line('A') // &
                 '    result = real(i) + r' // new_line('A') // &
                 '    if (flag .and. (result > 0.0)) then' // new_line('A') // &
                 '        print *, msg' // new_line('A') // &
                 '    end if' // new_line('A') // &
                 'end program test_mixed'
    
    call transform_lazy_fortran_string_with_format(input_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "  FAILED: ", error_msg
        all_tests_passed = .false.
    else
        print *, "  PASSED: Mixed types and complex expressions work"
    end if
    print *, ""
    
    ! Overall result
    print *, "=== Test Summary ==="
    if (all_tests_passed) then
        print *, "✅ ALL TESTS PASSED - Comprehensive fixes successful!"
        print *, "Issues #96, #98, and #99 appear to be resolved."
    else
        print *, "❌ SOME TESTS FAILED - Additional work needed."
        stop 1
    end if
    
end program test_comprehensive_fixes