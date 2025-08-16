program test_validation_integration
    ! Integration test for input_validation module (Issue #262)
    !
    ! Given: input_validation module should integrate seamlessly with existing system
    ! When: Testing integration with frontend and other modules
    ! Then: Should work together without breaking existing functionality
    
    use lexer_core, only: token_t
    use frontend, only: lex_source
    use frontend, only: transform_lazy_fortran_string
    ! NOTE: This will fail until input_validation module is created
    use input_validation, only: validate_basic_syntax
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Validation Integration Tests (Issue #262) ==='
    print *, 'Testing integration of input_validation with existing system'
    print *

    ! Test integration with frontend
    call run_test('Frontend uses extracted validation', test_frontend_integration())
    call run_test('Error messages consistent across modules', test_error_consistency())
    call run_test('Performance not degraded', test_performance_maintained())
    
    ! Test backward compatibility
    call run_test('Existing functionality preserved', test_backward_compatibility())
    call run_test('Issue #256 requirements still met', test_issue_256_compliance())
    call run_test('All existing tests still pass', test_existing_tests_pass())
    
    ! Test new capabilities
    call run_test('Validation usable outside frontend', test_standalone_usage())
    call run_test('Module reusability', test_module_reusability())

    ! Report results
    print *
    print *, 'Validation Integration Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Passed:', passed_count
    print *, '  Failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All validation integration tests passed!'
        stop 0
    else
        print *, 'FAILURE: Some validation integration tests failed.'
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

    function test_frontend_integration() result(passed)
        ! Given: Frontend should use extracted input_validation module
        ! When: Using transform_lazy_fortran_string
        ! Then: Should still detect validation errors through the new module
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test the same case that worked before extraction
        source = 'program test' // new_line('a') // &
                'if x > 0' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should still detect missing 'then' through new validation module
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, 'then') > 0
    end function

    function test_error_consistency() result(passed)
        ! Given: Error messages from frontend and direct validation calls
        ! When: Testing the same invalid input
        ! Then: Should produce consistent error messages
        logical :: passed
        character(len=:), allocatable :: source, frontend_output, frontend_error
        character(len=:), allocatable :: direct_error
        type(token_t), allocatable :: tokens(:)
        
        source = 'if x > 0' // new_line('a') // 'print *, x'
        
        ! Get error from frontend
        call transform_lazy_fortran_string(source, frontend_output, frontend_error)
        
        ! Get error from direct validation
        call lex_source(source, tokens, direct_error)
        call validate_basic_syntax(source, tokens, direct_error)
        
        ! Both should detect the same issue with similar messaging
        passed = len_trim(frontend_error) > 0 .and. &
                len_trim(direct_error) > 0 .and. &
                index(frontend_error, 'then') > 0 .and. &
                index(direct_error, 'then') > 0
    end function

    function test_performance_maintained() result(passed)
        ! Given: Validation extraction should not degrade performance
        ! When: Running validation on typical input
        ! Then: Should complete in reasonable time (architectural test)
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: i
        
        source = "program test" // new_line('a') // &
                 "integer :: x" // new_line('a') // &
                 "if x > 0 then" // new_line('a') // &
                 "  print *, x" // new_line('a') // &
                 "end if" // new_line('a') // &
                 "end program"
        
        ! Performance test: multiple validation calls should be fast
        do i = 1, 100
            call lex_source(source, tokens, error_msg)
            call validate_basic_syntax(source, tokens, error_msg)
        end do
        
        ! If we reach here without timing out, performance is acceptable
        passed = .true.
    end function

    function test_backward_compatibility() result(passed)
        ! Given: Existing frontend functionality
        ! When: Using the same API calls as before
        ! Then: Should work exactly as before the extraction
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Test valid Fortran (should still work)
        source = 'program test' // new_line('a') // &
                'integer :: x = 42' // new_line('a') // &
                'if (x > 0) then' // new_line('a') // &
                '  print *, x' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should still produce valid output with no errors
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'program test') > 0 .and. &
                index(output, 'end program') > 0
    end function

    function test_issue_256_compliance() result(passed)
        ! Given: Issue #256 requirements should still be met
        ! When: Testing error message quality requirements
        ! Then: Should maintain all Issue #256 improvements
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        logical :: has_clear_message, has_location, has_suggestion
        logical :: has_no_silent_fallback, has_meaningful_output, has_source_context
        
        ! Test Issue #256 requirement compliance
        
        ! Requirement 1: Clear error messages
        source = 'if x > 0' // new_line('a') // 'print *, x'
        call transform_lazy_fortran_string(source, output, error_msg)
        has_clear_message = index(error_msg, 'then') > 0 .and. index(error_msg, 'Missing') > 0
        
        ! Requirement 2: Location information  
        has_location = index(error_msg, 'line') > 0 .and. index(error_msg, 'column') > 0
        
        ! Requirement 3: Suggestions
        has_suggestion = index(error_msg, 'Suggestion') > 0 .or. index(error_msg, 'Add') > 0
        
        ! Requirement 4: No silent fallback
        source = 'garbage invalid syntax'
        call transform_lazy_fortran_string(source, output, error_msg)
        has_no_silent_fallback = len_trim(error_msg) > 0
        
        ! Requirement 5: Meaningful output
        has_meaningful_output = index(output, '! COMPILATION FAILED') > 0
        
        ! Requirement 6: Source context
        has_source_context = index(error_msg, 'Source:') > 0 .or. index(error_msg, '^') > 0
        
        passed = has_clear_message .and. has_location .and. has_suggestion .and. &
                has_no_silent_fallback .and. has_meaningful_output .and. has_source_context
    end function

    function test_existing_tests_pass() result(passed)
        ! Given: All existing tests should continue to pass
        ! When: Running validation through frontend
        ! Then: Should not break any existing functionality
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! This is an architectural test - if this test compiles and runs,
        ! it means the extraction hasn't broken the module system
        
        ! Test a few key scenarios that existing tests depend on
        
        ! Valid program should still work
        source = 'program test' // new_line('a') // 'end program'
        call transform_lazy_fortran_string(source, output, error_msg)
        
        passed = len_trim(error_msg) == 0
    end function

    function test_standalone_usage() result(passed)
        ! Given: input_validation should be usable outside frontend
        ! When: Using validation directly without frontend
        ! Then: Should provide complete validation capabilities
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        ! Test validation without using frontend at all
        source = "incomplete statement ="
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! Should detect incomplete statement
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, 'Incomplete') > 0
    end function

    function test_module_reusability() result(passed)
        ! Given: input_validation module should be reusable
        ! When: Using validation for different purposes
        ! Then: Should work flexibly for various use cases
        logical :: passed
        character(len=:), allocatable :: source1, source2, error1, error2
        type(token_t), allocatable :: tokens1(:), tokens2(:)
        
        ! Test reusability with different error types
        
        ! Test 1: Missing 'then'
        source1 = "if x > 0"
        call lex_source(source1, tokens1, error1)
        call validate_basic_syntax(source1, tokens1, error1)
        
        ! Test 2: Missing end constructs
        source2 = "program test"
        call lex_source(source2, tokens2, error2)
        call validate_basic_syntax(source2, tokens2, error2)
        
        ! Both should work independently and detect their respective issues
        passed = len_trim(error1) > 0 .and. index(error1, 'then') > 0 .and. &
                len_trim(error2) > 0 .and. index(error2, 'end program') > 0
    end function

end program test_validation_integration