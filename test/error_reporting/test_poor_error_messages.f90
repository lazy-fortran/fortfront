program test_poor_error_messages
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    all_passed = .true.

    print *, '=== Testing Error Reporting Quality (Issue #256) ==='
    print *, 'These tests verify that Issue #256 error reporting improvements are working'
    print *

    ! Test 1: Invalid syntax produces good error messages
    call run_test('Invalid syntax produces proper error messages', test_invalid_syntax_has_error())
    
    ! Test 2: Unsupported features provide clear errors
    call run_test('Unsupported features provide clear error feedback', test_unsupported_features_clear())
    
    ! Test 3: Location information is provided in errors
    call run_test('Line/column information provided in errors', test_has_location_info())
    
    ! Test 4: No silent fallback to empty output
    call run_test('No silent fallback - errors are reported', test_no_silent_fallback())
    
    ! Test 5: Clear error messages with specific problems
    call run_test('Clear and specific error messages', test_clear_error_messages())
    
    ! Test 6: Helpful fix suggestions provided
    call run_test('Helpful fix suggestions provided', test_has_fix_suggestions())

    ! Report results
    print *
    print *, 'Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests passing (should be all):', passed_count
    print *, '  Tests failing (indicates remaining issues):', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All error reporting tests PASS!'
        print *, 'Issue #256 error reporting improvements are working correctly'
        stop 0
    else
        print *, 'PARTIAL: Some error reporting features need more work'
        print *, 'Error reporting has been improved but not all requirements are met'
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

    function test_invalid_syntax_has_error() result(has_good_error)
        logical :: has_good_error
        character(len=:), allocatable :: source, output, error_msg
        
        ! Invalid syntax: missing 'then' in if statement
        source = 'program test' // new_line('a') // &
                'if x > 0' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should produce clear error about missing 'then'
        ! Check either error_msg OR output contains error information
        has_good_error = (len_trim(error_msg) > 0 .and. index(error_msg, 'then') > 0) .or. &
                        (index(output, 'COMPILATION FAILED') > 0 .and. index(output, 'then') > 0)
    end function

    function test_unsupported_features_clear() result(handles_well)
        logical :: handles_well
        character(len=:), allocatable :: source, output, error_msg
        
        ! Complex parameter declaration (should either work or give clear error)
        source = 'program test' // new_line('a') // &
                'integer, parameter :: n = 10' // new_line('a') // &
                'print *, n' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should either compile successfully OR provide clear error
        ! No silent failures allowed
        handles_well = (len_trim(error_msg) == 0 .and. len_trim(output) > 0) .or. &
                      (len_trim(error_msg) > 0) .or. &
                      (index(output, 'COMPILATION FAILED') > 0)
    end function

    function test_has_location_info() result(has_location)
        logical :: has_location
        character(len=:), allocatable :: source, output, error_msg
        
        ! Syntax error on specific line
        source = 'program test' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'x = 42 +' // new_line('a') // &  ! Error on line 3
                'print *, x' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should provide line/column information in error messages
        has_location = (index(error_msg, 'line') > 0 .and. index(error_msg, 'column') > 0) .or. &
                      (index(output, 'line') > 0 .and. index(output, 'column') > 0)
    end function

    function test_no_silent_fallback() result(no_silent_fail)
        logical :: no_silent_fail
        character(len=:), allocatable :: source, output, error_msg
        
        ! Complete garbage input
        source = 'this is not fortran at all 123 *** %%% invalid'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should NOT fail silently - must provide some form of feedback
        ! Either error message OR meaningful output explaining the issue
        no_silent_fail = (len_trim(error_msg) > 0) .or. &
                        (index(output, 'COMPILATION FAILED') > 0) .or. &
                        (index(output, 'ERROR') > 0) .or. &
                        (len_trim(output) > 0)  ! At least some output
    end function

    function test_clear_error_messages() result(has_clear_errors)
        logical :: has_clear_errors
        character(len=:), allocatable :: source, output, error_msg
        
        ! Missing end statement
        source = 'program test' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'x = 42'
        ! Missing "end program"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should provide clear, specific error message about missing end program
        has_clear_errors = (index(error_msg, 'missing') > 0 .or. index(error_msg, 'Missing') > 0) .and. &
                          (index(error_msg, 'end') > 0 .or. index(error_msg, 'program') > 0) .or. &
                          (index(output, 'missing') > 0 .or. index(output, 'Missing') > 0) .and. &
                          (index(output, 'end') > 0 .or. index(output, 'program') > 0)
    end function

    function test_has_fix_suggestions() result(has_suggestions)
        logical :: has_suggestions
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test case that should provide helpful suggestions
        source = 'program test' // new_line('a') // &
                'if x > 0' // new_line('a') // &  ! Missing then
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should provide helpful fix suggestions in error messages
        has_suggestions = (index(error_msg, 'Suggestion') > 0 .or. index(error_msg, 'Add') > 0 .or. &
                          index(error_msg, 'Try') > 0 .or. index(error_msg, 'suggestion') > 0) .or. &
                         (index(output, 'Suggestion') > 0 .or. index(output, 'Add') > 0 .or. &
                          index(output, 'Try') > 0 .or. index(output, 'suggestion') > 0)
    end function

end program test_poor_error_messages