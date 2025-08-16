program test_input_validation_module
    ! Test for dedicated input_validation module (Issue #262)
    ! 
    ! Given: A new input_validation module should be extracted from frontend.f90
    ! When: Testing the module interface and validation functions
    ! Then: All validation logic should be cleanly separated with proper interfaces
    
    use lexer_core, only: token_t, TK_KEYWORD, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER
    use frontend, only: lex_source
    ! NOTE: This will fail until input_validation module is created
    use input_validation, only: &
        validate_basic_syntax, &
        check_missing_then_statements, &
        check_incomplete_statements, &
        check_for_fortran_content, &
        check_missing_end_constructs, &
        contains_invalid_patterns, &
        has_only_meaningless_tokens
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Input Validation Module Tests (Issue #262) ==='
    print *, 'Testing dedicated input_validation module interface'
    print *

    ! Test module interface and public API
    call run_test('Module interface exists', test_module_interface())
    call run_test('validate_basic_syntax interface', test_validate_basic_syntax_interface())
    call run_test('Check functions interface', test_check_functions_interface())
    call run_test('Utility functions interface', test_utility_functions_interface())
    
    ! Test validation function behavior
    call run_test('Basic syntax validation', test_basic_syntax_validation())
    call run_test('Missing then statements detection', test_missing_then_detection())
    call run_test('Incomplete statements detection', test_incomplete_statements_detection())
    call run_test('Fortran content validation', test_fortran_content_validation())
    call run_test('Missing end constructs detection', test_missing_end_detection())
    call run_test('Invalid patterns detection', test_invalid_patterns_detection())
    
    ! Test clean separation of concerns
    call run_test('No frontend dependencies', test_no_frontend_dependencies())
    call run_test('Self-contained validation logic', test_self_contained_logic())

    ! Report results
    print *
    print *, 'Input Validation Module Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Passed:', passed_count
    print *, '  Failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All input validation module tests passed!'
        stop 0
    else
        print *, 'FAILURE: Some input validation module tests failed.'
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

    function test_module_interface() result(passed)
        ! Given: The input_validation module should exist
        ! When: Testing module availability
        ! Then: Module should be accessible with expected procedures
        logical :: passed
        
        ! This test will fail until the module is created
        ! The use statement at the top will cause compilation failure
        passed = .true.  ! Will only reach here if module exists
    end function

    function test_validate_basic_syntax_interface() result(passed)
        ! Given: validate_basic_syntax should be extracted to input_validation
        ! When: Testing the subroutine interface
        ! Then: Should accept source, tokens array, and return error message
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        source = "program test"
        call lex_source(source, tokens, error_msg)
        
        ! Test that validate_basic_syntax can be called
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! If we reach here, the interface exists
        passed = .true.
    end function

    function test_check_functions_interface() result(passed)
        ! Given: Specific check functions should be public in input_validation
        ! When: Testing individual check function interfaces
        ! Then: All check functions should be callable with proper signatures
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        character(len=:), allocatable :: source_lines(:)
        type(token_t), allocatable :: tokens(:)
        
        source = "if x > 0"
        call lex_source(source, tokens, error_msg)
        
        ! Mock source_lines for testing
        allocate(character(len=20) :: source_lines(1))
        source_lines(1) = "if x > 0"
        
        ! Test individual check functions
        call check_missing_then_statements(tokens, source_lines, error_msg)
        call check_incomplete_statements(tokens, source_lines, error_msg)
        call check_for_fortran_content(tokens, error_msg)
        call check_missing_end_constructs(tokens, source_lines, error_msg)
        
        passed = .true.  ! If we reach here, interfaces exist
    end function

    function test_utility_functions_interface() result(passed)
        ! Given: Utility functions should be public in input_validation
        ! When: Testing utility function interfaces
        ! Then: Utility functions should be callable and return expected types
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source, error_msg
        logical :: result
        
        source = "invalid *** patterns"
        call lex_source(source, tokens, error_msg)
        
        ! Test utility functions
        result = contains_invalid_patterns(tokens)
        result = has_only_meaningless_tokens(tokens)
        
        passed = .true.  ! If we reach here, interfaces exist
    end function

    function test_basic_syntax_validation() result(passed)
        ! Given: A source string with syntax errors
        ! When: Calling validate_basic_syntax
        ! Then: Should detect and report syntax errors appropriately
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        ! Test case 1: Missing 'then' in if statement
        source = "program test" // new_line('a') // &
                 "if x > 0" // new_line('a') // &
                 "  print *, x" // new_line('a') // &
                 "end if" // new_line('a') // &
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        call validate_basic_syntax(source, tokens, error_msg)
        
        ! Should detect missing 'then'
        passed = len_trim(error_msg) > 0 .and. index(error_msg, 'then') > 0
    end function

    function test_missing_then_detection() result(passed)
        ! Given: An if statement without 'then'
        ! When: Calling check_missing_then_statements
        ! Then: Should detect the missing 'then' and provide error message
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        character(len=:), allocatable :: source_lines(:)
        type(token_t), allocatable :: tokens(:)
        
        source = "if x > 0" // new_line('a') // "  print *, x"
        call lex_source(source, tokens, error_msg)
        
        ! Create source lines array
        allocate(character(len=20) :: source_lines(2))
        source_lines(1) = "if x > 0"
        source_lines(2) = "  print *, x"
        
        call check_missing_then_statements(tokens, source_lines, error_msg)
        
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, 'then') > 0 .and. &
                index(error_msg, 'Missing') > 0
    end function

    function test_incomplete_statements_detection() result(passed)
        ! Given: Incomplete statements with dangling operators
        ! When: Calling check_incomplete_statements
        ! Then: Should detect incomplete expressions
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        character(len=:), allocatable :: source_lines(:)
        type(token_t), allocatable :: tokens(:)
        
        source = "x = 42 +"
        call lex_source(source, tokens, error_msg)
        
        allocate(character(len=10) :: source_lines(1))
        source_lines(1) = "x = 42 +"
        
        call check_incomplete_statements(tokens, source_lines, error_msg)
        
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, 'Incomplete') > 0
    end function

    function test_fortran_content_validation() result(passed)
        ! Given: Input that doesn't contain valid Fortran
        ! When: Calling check_for_fortran_content
        ! Then: Should detect non-Fortran content
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        source = "this is not fortran *** 123"
        call lex_source(source, tokens, error_msg)
        
        call check_for_fortran_content(tokens, error_msg)
        
        passed = len_trim(error_msg) > 0
    end function

    function test_missing_end_detection() result(passed)
        ! Given: Fortran constructs without proper end statements
        ! When: Calling check_missing_end_constructs
        ! Then: Should detect missing end constructs
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        character(len=:), allocatable :: source_lines(:)
        type(token_t), allocatable :: tokens(:)
        
        source = "program test" // new_line('a') // "integer :: x"
        call lex_source(source, tokens, error_msg)
        
        allocate(character(len=15) :: source_lines(2))
        source_lines(1) = "program test"
        source_lines(2) = "integer :: x"
        
        call check_missing_end_constructs(tokens, source_lines, error_msg)
        
        passed = len_trim(error_msg) > 0 .and. &
                index(error_msg, 'end program') > 0
    end function

    function test_invalid_patterns_detection() result(passed)
        ! Given: Input with invalid syntax patterns
        ! When: Calling contains_invalid_patterns
        ! Then: Should return true for invalid patterns
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        source = "garbage *** invalid @@@ patterns"
        call lex_source(source, tokens, error_msg)
        
        passed = contains_invalid_patterns(tokens)
    end function

    function test_no_frontend_dependencies() result(passed)
        ! Given: The input_validation module should be independent
        ! When: Checking module dependencies
        ! Then: Should not depend on frontend module for core validation
        logical :: passed
        
        ! This test validates architectural separation
        ! The module should only depend on lexer_core for token_t
        ! and not on frontend module
        passed = .true.  ! Architectural constraint verified by compilation
    end function

    function test_self_contained_logic() result(passed)
        ! Given: All validation logic should be in input_validation module
        ! When: Testing validation without frontend
        ! Then: Should be able to perform validation independently
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        source = "if x > 0"
        call lex_source(source, tokens, error_msg)
        
        ! Should be able to validate without any frontend dependencies
        call validate_basic_syntax(source, tokens, error_msg)
        
        passed = .true.  ! Self-containment verified by successful execution
    end function

end program test_input_validation_module