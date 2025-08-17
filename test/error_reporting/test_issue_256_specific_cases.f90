program test_issue_256_specific_cases
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Issue #256 Specific Test Cases ==='
    print *, 'Testing the exact scenarios mentioned in the issue description'
    print *

    ! Test the exact case mentioned in the issue description
    call run_test('Comments-only input from issue description', &
                  test_issue_example())
    
    ! Test cases that should pass validation
    call run_test('Valid lazy Fortran without keywords should pass', &
                  test_valid_lazy_fortran())
    
    call run_test('Mixed comments and code should pass', &
                  test_mixed_comments_code())
    
    call run_test('Expression without keywords should pass', &
                  test_expression_without_keywords())
    
    ! Test cases that should fail validation with meaningful errors
    call run_test('Complete nonsense should fail with error', &
                  test_complete_nonsense())
    
    call run_test('Invalid syntax should fail with error', &
                  test_invalid_syntax())

    ! Test boundary cases
    call run_test('Many random words should fail appropriately', &
                  test_many_random_words())
    
    call run_test('Single comment line should pass', &
                  test_single_comment())

    ! Report results
    print *
    print *, 'Issue #256 Specific Cases Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests passed:', passed_count
    print *, '  Tests failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All Issue #256 specific cases pass!'
        print *, 'The validation logic correctly handles the reported scenarios.'
        stop 0
    else
        print *, 'FAILURE: Some Issue #256 specific cases still fail.'
        print *, 'Validation logic needs refinement for these exact scenarios.'
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

    ! Test the exact example from the issue description
    function test_issue_example() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: The exact case from Issue #256 description
        source = '! This is just a comment'
        
        ! When: We process it
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Should be accepted and NOT produce the error mentioned in issue
        ! The issue mentions this currently produces:
        ! "Input does not appear to be valid Fortran code. No recognized Fortran keywords found."
        passed = len_trim(error_msg) == 0 .and. &
                index(output, '! COMPILATION FAILED') == 0 .and. &
                index(error_msg, 'No recognized Fortran keywords found') == 0
        
        if (.not. passed) then
            print *, '    Error message: "', error_msg, '"'
            print *, '    Output contains COMPILATION FAILED:', index(output, '! COMPILATION FAILED') > 0
        end if
    end function

    function test_valid_lazy_fortran() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'x = 42' // new_line('a') // &
                'y = sin(x)' // new_line('a') // &
                'print *, y'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'x = 42') > 0 .and. &
                index(output, '! COMPILATION FAILED') == 0
    end function

    function test_mixed_comments_code() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = '! Calculate the result' // new_line('a') // &
                'result = a + b' // new_line('a') // &
                '! Print the result' // new_line('a') // &
                'print *, result'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'result = a + b') > 0
    end function

    function test_expression_without_keywords() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'area = 3.14159 * radius * radius'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'area = 3.14159') > 0
    end function

    function test_complete_nonsense() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'complete garbage input 123 *** %%% invalid nonsense @#$%'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should fail with meaningful error message (not silent fallback)
        passed = len_trim(error_msg) > 0 .or. &
                index(output, '! COMPILATION FAILED') > 0
        
        if (.not. passed) then
            print *, '    Silent fallback detected - no error for garbage input'
            print *, '    Error message: "', error_msg, '"'
            print *, '    Output starts with: "', output(1:min(100, len(output))), '"'
        end if
    end function

    function test_invalid_syntax() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'invalid_syntax_here $$$ nonsense_123 @@@'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should fail with error (not produce empty/minimal program)
        passed = len_trim(error_msg) > 0 .or. &
                index(output, '! COMPILATION FAILED') > 0
    end function

    function test_many_random_words() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! More than 10 tokens without clear structure
        source = 'random word another word more words here and there everywhere nonsense totally'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should produce error for clearly non-Fortran input
        passed = len_trim(error_msg) > 0 .or. &
                index(output, '! COMPILATION FAILED') > 0
    end function

    function test_single_comment() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        source = '! Single comment'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        passed = len_trim(error_msg) == 0 .and. &
                index(output, '! COMPILATION FAILED') == 0
    end function

end program test_issue_256_specific_cases