program test_issue_256_validation
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Issue #256 Validation: Error Message Quality ==='
    print *, 'Testing that all requirements from Issue #256 are met'
    print *

    ! Requirement 1: Clear, actionable error messages
    call run_test('Clear error messages with specific problems', &
                  test_clear_error_messages())
    
    ! Requirement 2: Line/column information
    call run_test('Line and column information provided', &
                  test_location_information())
    
    ! Requirement 3: Actionable suggestions
    call run_test('Helpful fix suggestions provided', &
                  test_fix_suggestions())
    
    ! Requirement 4: No silent fallbacks
    call run_test('No silent fallback to empty programs', &
                  test_no_silent_fallback())
    
    ! Requirement 5: Meaningful output for invalid input
    call run_test('Meaningful output for invalid syntax', &
                  test_meaningful_output())
    
    ! Requirement 6: Source context in error messages
    call run_test('Source line context provided', &
                  test_source_context())

    ! Report results
    print *
    print *, 'Issue #256 Validation Results:'
    print *, '  Total requirements tested:', test_count
    print *, '  Requirements satisfied:', passed_count
    print *, '  Requirements not met:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All Issue #256 requirements are satisfied!'
        print *, 'Error reporting has been significantly improved.'
        stop 0
    else
        print *, 'FAILURE: Some Issue #256 requirements are not met.'
        print *, 'Additional work needed on error reporting.'
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

    function test_clear_error_messages() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test invalid if statement
        source = 'program test' // new_line('a') // &
                'if x > 0' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should have clear error message mentioning the specific problem
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, 'then') > 0 .and. &
                index(error_msg, 'Missing') > 0
    end function

    function test_location_information() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test error with specific location
        source = 'program test' // new_line('a') // &
                'integer :: x' // new_line('a') // &
                'if x > 0' // new_line('a') // &  ! Error on line 3
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should have line and column information
        passed = index(error_msg, 'line') > 0 .and. &
                index(error_msg, 'column') > 0
    end function

    function test_fix_suggestions() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test error that can provide suggestion
        source = 'if x > 0' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end if'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should provide helpful suggestion
        passed = index(error_msg, 'Suggestion') > 0 .or. &
                index(error_msg, 'Add') > 0 .or. &
                index(error_msg, 'then') > 0
    end function

    function test_no_silent_fallback() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test completely invalid input
        source = 'this is not valid fortran syntax at all *** 123'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should NOT silently succeed - must have error message
        passed = len_trim(error_msg) > 0
    end function

    function test_meaningful_output() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test invalid syntax
        source = 'garbage input 123 *** invalid'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Output should indicate compilation failure, not look like valid code
        passed = index(output, '! COMPILATION FAILED') > 0 .or. &
                index(output, '! Error:') > 0
    end function

    function test_source_context() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test error with source context
        source = 'program test' // new_line('a') // &
                'if x > 0' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should show the actual source line and/or pointer
        passed = index(error_msg, 'Source:') > 0 .or. &
                index(error_msg, '^') > 0 .or. &
                index(error_msg, 'if x > 0') > 0
    end function

end program test_issue_256_validation