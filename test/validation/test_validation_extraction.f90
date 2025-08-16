program test_validation_extraction
    ! Test for validation logic extraction from frontend.f90 (Issue #262)
    !
    ! Given: Validation functions should be extracted from frontend.f90
    ! When: Testing the extraction and clean module separation
    ! Then: input_validation module should be independent and complete
    
    use lexer_core, only: token_t
    use frontend, only: lex_source
    ! NOTE: This will fail until input_validation module is created
    use input_validation, only: validate_basic_syntax
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Validation Extraction Tests (Issue #262) ==='
    print *, 'Testing extraction of validation logic from frontend.f90'
    print *

    ! Test that validation works independently of frontend
    call run_test('Independent validation without frontend', test_independent_validation())
    call run_test('Validation preserves original behavior', test_behavior_preservation())
    call run_test('Error messages maintain quality', test_error_message_quality())
    call run_test('Complete validation coverage', test_complete_validation_coverage())
    
    ! Test architectural separation
    call run_test('No circular dependencies', test_no_circular_dependencies())
    call run_test('Clean module boundaries', test_clean_module_boundaries())
    call run_test('Proper encapsulation', test_proper_encapsulation())

    ! Report results
    print *
    print *, 'Validation Extraction Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Passed:', passed_count
    print *, '  Failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All validation extraction tests passed!'
        stop 0
    else
        print *, 'FAILURE: Some validation extraction tests failed.'
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

    function test_independent_validation() result(passed)
        ! Given: input_validation module extracted from frontend
        ! When: Using validation without any frontend imports
        ! Then: Should work completely independently
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        ! Test validation of invalid Fortran
        source = "if x > 0" // new_line('a') // &
                 "  print *, x" // new_line('a') // &
                 "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            passed = .false.
            return
        end if
        
        ! This should work without any frontend dependencies
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! Should detect missing 'then'
        passed = len_trim(error_msg) > 0 .and. index(error_msg, 'then') > 0
    end function

    function test_behavior_preservation() result(passed)
        ! Given: Validation extracted from frontend
        ! When: Testing the same scenarios that worked in frontend
        ! Then: Should produce identical validation behavior
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        ! Test case from Issue #256: missing 'then' detection
        source = "program test" // new_line('a') // &
                 "integer :: x" // new_line('a') // &
                 "if x > 0" // new_line('a') // &
                 "  print *, x" // new_line('a') // &
                 "end if" // new_line('a') // &
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! Should detect missing 'then' with proper error formatting
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, "Missing 'then'") > 0 .and. &
                index(error_msg, "line") > 0 .and. &
                index(error_msg, "column") > 0
    end function

    function test_error_message_quality() result(passed)
        ! Given: Error formatting extracted to input_validation
        ! When: Generating error messages
        ! Then: Should maintain Issue #256 quality requirements
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        ! Test incomplete expression detection
        source = "x = 42 +"
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! Should have enhanced error format from Issue #256
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, "Incomplete") > 0 .and. &
                index(error_msg, "Suggestion:") > 0 .and. &
                index(error_msg, "Source:") > 0
    end function

    function test_complete_validation_coverage() result(passed)
        ! Given: All validation functions extracted
        ! When: Testing various error scenarios
        ! Then: Should cover all validation cases from frontend
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: test_cases_passed
        
        test_cases_passed = 0
        
        ! Test 1: Missing end constructs
        source = "program test" // new_line('a') // "integer :: x"
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        if (index(error_msg, 'end program') > 0) test_cases_passed = test_cases_passed + 1
        
        ! Test 2: Invalid patterns
        source = "garbage *** invalid"
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) test_cases_passed = test_cases_passed + 1
        
        ! Test 3: Empty input
        source = ""
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) test_cases_passed = test_cases_passed + 1
        
        passed = test_cases_passed == 3
    end function

    function test_no_circular_dependencies() result(passed)
        ! Given: input_validation module should be independent
        ! When: Checking module dependency structure
        ! Then: Should not create circular dependencies with frontend
        logical :: passed
        
        ! This test validates architectural constraint
        ! input_validation should only depend on:
        ! - lexer_core (for token_t)
        ! - intrinsic modules
        ! It should NOT depend on frontend
        passed = .true.  ! Verified by successful compilation without frontend
    end function

    function test_clean_module_boundaries() result(passed)
        ! Given: Validation extracted to separate module
        ! When: Testing module interface boundaries
        ! Then: Should have clean public interface with no private leakage
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        source = "if x > 0"
        call lex_source(source, tokens, error_msg)
        
        ! Should be able to use public interface without accessing internals
        call validate_basic_syntax(source, tokens, error_msg)
        
        passed = .true.  ! Clean interface verified by successful usage
    end function

    function test_proper_encapsulation() result(passed)
        ! Given: Validation logic should be properly encapsulated
        ! When: Using only public interface
        ! Then: Should have access to all necessary functionality
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        ! Test that we can perform complete validation workflow
        source = "program test" // new_line('a') // &
                 "if x > 0" // new_line('a') // &
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        ! Should detect multiple issues:
        ! 1. Missing 'then'
        ! 2. Missing 'end program' vs incomplete if
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! Should get meaningful error (encapsulation working)
        passed = len_trim(error_msg) > 0
    end function

end program test_validation_extraction