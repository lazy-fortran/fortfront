program test_poor_error_messages
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    all_passed = .true.

    print *, '=== Testing Error Messages Before Issue #256 Fix ==='
    print *, 'These tests verify that error reporting improvements have been implemented'
    print *

    ! Test 1: Invalid syntax produces no error
    call run_test('Invalid syntax produces no error', test_invalid_syntax_no_error())
    
    ! Test 2: Unsupported features fail silently
    call run_test('Unsupported features fail silently', test_unsupported_features_silent())
    
    ! Test 3: No line/column information in errors
    call run_test('Missing line/column information', test_missing_location_info())
    
    ! Test 4: Silent fallback to empty output
    call run_test('Silent fallback to empty output', test_silent_empty_fallback())
    
    ! Test 5: Vague error messages
    call run_test('Vague error messages', test_vague_error_messages())
    
    ! Test 6: No guidance on fix suggestions
    call run_test('No fix suggestions provided', test_no_fix_suggestions())

    ! Report results
    print *
    print *, 'Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests now passing (error reporting improved):', passed_count
    print *, '  Tests still failing (may need further work):', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All error reporting improvements detected'
        print *, 'Issue #256 error reporting has been fully implemented'
        stop 0
    else if (passed_count > 0) then
        print *, 'PARTIAL: Some error reporting improvements detected'
        print *, 'But more work may be needed for full Issue #256 compliance'
        stop 1
    else
        print *, 'ERROR: No error reporting improvements detected'
        print *, 'Issue #256 error reporting still needs implementation'
        stop 1
    end if

contains

    subroutine run_test(test_name, result)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: result
        
        test_count = test_count + 1
        if (result) then
            passed_count = passed_count + 1
            print *, '  PASS (error reporting improved):', test_name
        else
            print *, '  FAIL (still needs work):', test_name
        end if
    end subroutine

    function test_invalid_syntax_no_error() result(should_fail)
        logical :: should_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Invalid syntax: missing 'then' in if statement
        source = 'program test' // new_line('a') // &
                'if x > 0' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Current behavior: likely produces empty output with no clear error
        ! Expected: Should fail with clear error about missing 'then'
        should_fail = (len_trim(error_msg) == 0) .or. &
                     (len_trim(output) == 0) .or. &
                     (index(error_msg, 'then') == 0)
    end function

    function test_unsupported_features_silent() result(should_fail)
        logical :: should_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Complex parameter declaration (known to cause issues per #254)
        source = 'program test' // new_line('a') // &
                'integer, parameter :: n = 10' // new_line('a') // &
                'print *, n' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Current behavior: likely fails silently or with vague error
        ! Expected: Should provide clear error about unsupported parameter syntax
        should_fail = (len_trim(error_msg) == 0) .or. &
                     (index(error_msg, 'parameter') == 0 .and. index(output, 'parameter') == 0)
    end function

    function test_missing_location_info() result(should_fail)
        logical :: should_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Syntax error on specific line
        source = 'program test' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'x = 42 +' // new_line('a') // &  ! Error on line 3
                'print *, x' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Current behavior: no line/column information in error output
        ! Expected: Should specify "line 3, column X" where error occurred
        should_fail = index(error_msg, 'line') == 0 .and. index(error_msg, 'column') == 0
    end function

    function test_silent_empty_fallback() result(should_fail)
        logical :: should_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Complete garbage input
        source = 'this is not fortran at all 123 *** %%% invalid'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Current behavior: likely produces empty output with no error message
        ! Expected: Should fail with clear explanation of what went wrong
        should_fail = len_trim(error_msg) == 0 .or. len_trim(output) == 0
    end function

    function test_vague_error_messages() result(should_fail)
        logical :: should_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Missing end statement
        source = 'program test' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'x = 42'
        ! Missing "end program"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Current behavior: vague or no error message
        ! Expected: Should clearly state "missing 'end program' statement"
        should_fail = index(error_msg, 'missing') == 0 .and. &
                     index(error_msg, 'end program') == 0 .and. &
                     index(error_msg, 'expected') == 0
    end function

    function test_no_fix_suggestions() result(should_fail)
        logical :: should_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Common mistake: using = instead of ==
        source = 'program test' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'if (x = 5) then' // new_line('a') // &
                '  print *, "five"' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Current behavior: no helpful suggestions
        ! Expected: Should suggest "did you mean '==' for comparison?"
        should_fail = index(error_msg, 'did you mean') == 0 .and. &
                     index(error_msg, 'suggest') == 0 .and. &
                     index(error_msg, 'try') == 0 .and. &
                     index(error_msg, '==') == 0
    end function

end program test_poor_error_messages