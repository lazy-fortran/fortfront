program test_issue_259_input_validation_refinement
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Issue #259 Input Validation Refinement ==='
    print *, 'Testing that input validation refinement fixes are working correctly'
    print *

    ! Test Category 1: Comments-only input should be accepted
    call run_test('Comments-only input with single comment', &
                  test_single_comment_input())
    
    call run_test('Comments-only input with multiple comments', &
                  test_multiple_comment_input())
    
    call run_test('Comment with whitespace variations', &
                  test_comment_whitespace_variations())
    
    ! Test Category 2: Mathematical expressions should be accepted
    call run_test('Simple addition expression', &
                  test_simple_addition_expression())
    
    call run_test('Expression with parentheses', &
                  test_expression_with_parentheses())
    
    call run_test('Identifier multiplication', &
                  test_identifier_multiplication())
    
    call run_test('Complex variable expression', &
                  test_complex_variable_expression())
    
    ! These should pass (demonstrating current validation works for some cases)
    call run_test('Expression with numbers (should work)', &
                  test_expression_with_numbers())
    
    call run_test('Expression with function call (should work)', &
                  test_expression_with_function_call())
    
    ! Test Category 3: Lazy Fortran constructs should be valid
    call run_test('Variable assignment without declaration', &
                  test_variable_assignment())
    
    call run_test('Array indexing expression', &
                  test_array_indexing())
    
    call run_test('Mixed identifier and number expressions', &
                  test_mixed_identifier_number())

    ! Report results
    print *
    print *, 'Issue #259 Validation Refinement Results:'
    print *, '  Total test cases:', test_count
    print *, '  Tests passing:', passed_count
    print *, '  Tests failing (demonstrating bugs):', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All validation tests pass - Issue #259 has been fixed!'
        print *, 'All previously problematic inputs are now correctly accepted:'
        print *, '- Comments-only input is accepted'
        print *, '- Mathematical expressions without keywords are accepted'
        print *, '- Lazy Fortran constructs are properly validated'
        stop 0
    else
        print *, 'FAILURE: Some validation tests still failing:'
        print *, '  Tests failing:', test_count - passed_count
        print *, 'Issue #259 validation refinement is not complete.'
        stop 1
    end if

contains

    subroutine run_test(test_name, result)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: result
        
        test_count = test_count + 1
        if (result) then
            passed_count = passed_count + 1
            print *, '  PASS:', test_name
        else
            print *, '  FAIL:', test_name
        end if
    end subroutine

    ! Given: A source string containing only a comment
    ! When: The input validation is performed
    ! Then: The input should be accepted (not rejected)
    function test_single_comment_input() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = '! This is a comment'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for comments-only input
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A source string containing multiple comments
    ! When: The input validation is performed
    ! Then: The input should be accepted (not rejected)
    function test_multiple_comment_input() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = '! Comment 1' // new_line('a') // &
                 '! Comment 2' // new_line('a') // &
                 '! Comment 3'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for comments-only input
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A comment with various whitespace patterns
    ! When: The input validation is performed
    ! Then: The input should be accepted (not rejected)
    function test_comment_whitespace_variations() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = '    ! Indented comment with spaces'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for comments-only input
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A simple mathematical addition expression
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_simple_addition_expression() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'a + b'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for mathematical expressions
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A simple identifier multiplication (no numbers)
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_identifier_multiplication() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'a * b'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for identifier expressions
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A complex variable expression without numbers
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_complex_variable_expression() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'x + y - z'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for variable expressions
        passed = len_trim(error_msg) == 0
    end function

    ! Given: An expression with numbers (should work with current validation)
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_expression_with_numbers() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'x * y + z / 2.0'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for expressions with numbers
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A mathematical expression with parentheses for precedence
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_expression_with_parentheses() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = '(a + b) * c'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for expressions with parentheses
        passed = len_trim(error_msg) == 0
    end function

    ! Given: An expression with a function call
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_expression_with_function_call() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'sqrt(x) + y'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for expressions with function calls
        passed = len_trim(error_msg) == 0
    end function

    ! Given: A variable assignment without explicit type declaration (lazy Fortran)
    ! When: The input validation is performed
    ! Then: The assignment should be accepted as valid lazy Fortran
    function test_variable_assignment() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'x = 42'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for assignment expressions
        passed = len_trim(error_msg) == 0
    end function

    ! Given: An array indexing expression without explicit declarations
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_array_indexing() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'arr(i)'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for array indexing
        passed = len_trim(error_msg) == 0
    end function

    ! Given: An expression mixing identifiers and numbers without keywords
    ! When: The input validation is performed
    ! Then: The expression should be accepted as valid lazy Fortran
    function test_mixed_identifier_number() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'result = 3.14 * radius'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should succeed (empty error_msg) for mixed expressions
        passed = len_trim(error_msg) == 0
    end function

end program test_issue_259_input_validation_refinement