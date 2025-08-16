program test_validation_utilities
    ! Test for validation utility functions (Issue #262)
    !
    ! Given: Error formatting and utility functions should be in input_validation
    ! When: Testing error formatting and utility functions
    ! Then: Should provide clean interfaces for error reporting and text processing
    
    use lexer_core, only: token_t
    use frontend, only: lex_source
    ! NOTE: These will fail until input_validation module is created
    use input_validation, only: &
        format_enhanced_error, &
        format_syntax_error, &
        split_into_lines
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Validation Utilities Tests (Issue #262) ==='
    print *, 'Testing error formatting and utility functions'
    print *

    ! Test error formatting functions
    call run_test('Enhanced error formatting', test_enhanced_error_formatting())
    call run_test('Syntax error formatting', test_syntax_error_formatting())
    call run_test('Error formatting with context', test_error_formatting_with_context())
    call run_test('Error formatting without suggestion', test_error_formatting_no_suggestion())
    
    ! Test utility functions
    call run_test('Split into lines utility', test_split_into_lines())
    call run_test('Split multiline source', test_split_multiline_source())
    call run_test('Split empty source', test_split_empty_source())
    
    ! Test error message structure
    call run_test('Error contains location info', test_error_location_info())
    call run_test('Error contains suggestion', test_error_suggestion())
    call run_test('Error type classification', test_error_type_classification())

    ! Report results
    print *
    print *, 'Validation Utilities Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Passed:', passed_count
    print *, '  Failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All validation utilities tests passed!'
        stop 0
    else
        print *, 'FAILURE: Some validation utilities tests failed.'
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

    function test_enhanced_error_formatting() result(passed)
        ! Given: Enhanced error formatting function
        ! When: Formatting an error with all parameters
        ! Then: Should return properly formatted error message
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=20) :: source_lines(2))
        source_lines(1) = "if x > 0"
        source_lines(2) = "  print *, x"
        
        formatted_error = format_enhanced_error( &
            "Missing 'then' keyword", &
            1, 1, &
            source_lines, &
            "Add 'then' after condition", &
            "SYNTAX_ERROR")
        
        passed = len_trim(formatted_error) > 0 .and. &
                index(formatted_error, "Missing 'then'") > 0 .and. &
                index(formatted_error, "line") > 0 .and. &
                index(formatted_error, "SYNTAX_ERROR") > 0
    end function

    function test_syntax_error_formatting() result(passed)
        ! Given: Legacy syntax error formatting function
        ! When: Formatting a syntax error
        ! Then: Should return formatted error using enhanced formatter
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=15) :: source_lines(1))
        source_lines(1) = "invalid syntax"
        
        formatted_error = format_syntax_error( &
            "Invalid statement", &
            1, 5, &
            source_lines, &
            "Check syntax")
        
        passed = len_trim(formatted_error) > 0 .and. &
                index(formatted_error, "Invalid statement") > 0 .and. &
                index(formatted_error, "SYNTAX_ERROR") > 0
    end function

    function test_error_formatting_with_context() result(passed)
        ! Given: Error formatting with source context
        ! When: Including source lines in error
        ! Then: Should show source context in formatted message
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=25) :: source_lines(3))
        source_lines(1) = "program test"
        source_lines(2) = "if x > 0"
        source_lines(3) = "end program"
        
        formatted_error = format_enhanced_error( &
            "Syntax error in if statement", &
            2, 1, &
            source_lines, &
            "Add 'then' keyword", &
            "SYNTAX_ERROR")
        
        passed = len_trim(formatted_error) > 0 .and. &
                index(formatted_error, "Source:") > 0 .and. &
                index(formatted_error, "if x > 0") > 0
    end function

    function test_error_formatting_no_suggestion() result(passed)
        ! Given: Error formatting without suggestion
        ! When: Calling format_syntax_error without suggestion parameter
        ! Then: Should provide default suggestion
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=10) :: source_lines(1))
        source_lines(1) = "bad syntax"
        
        formatted_error = format_syntax_error( &
            "Parse error", &
            1, 1, &
            source_lines, &
            "")  ! Empty suggestion
        
        passed = len_trim(formatted_error) > 0 .and. &
                index(formatted_error, "Check syntax") > 0
    end function

    function test_split_into_lines() result(passed)
        ! Given: A multiline source string
        ! When: Calling split_into_lines
        ! Then: Should split into array of individual lines
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: lines(:)
        
        source = "line 1" // new_line('a') // &
                 "line 2" // new_line('a') // &
                 "line 3"
        
        call split_into_lines(source, lines)
        
        passed = size(lines) == 3 .and. &
                trim(lines(1)) == "line 1" .and. &
                trim(lines(2)) == "line 2" .and. &
                trim(lines(3)) == "line 3"
    end function

    function test_split_multiline_source() result(passed)
        ! Given: Complex multiline Fortran source
        ! When: Splitting into lines
        ! Then: Should handle various line structures correctly
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: lines(:)
        
        source = "program test" // new_line('a') // &
                 "  integer :: x = 42" // new_line('a') // &
                 "  if (x > 0) then" // new_line('a') // &
                 "    print *, x" // new_line('a') // &
                 "  end if" // new_line('a') // &
                 "end program"
        
        call split_into_lines(source, lines)
        
        passed = size(lines) == 6 .and. &
                trim(lines(1)) == "program test" .and. &
                index(lines(2), "integer") > 0 .and. &
                index(lines(6), "end program") > 0
    end function

    function test_split_empty_source() result(passed)
        ! Given: Empty or whitespace-only source
        ! When: Splitting into lines
        ! Then: Should handle gracefully
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: lines(:)
        
        source = ""
        call split_into_lines(source, lines)
        
        ! Should handle empty source without crashing
        passed = allocated(lines)
    end function

    function test_error_location_info() result(passed)
        ! Given: Error formatting with line and column info
        ! When: Formatting error with specific location
        ! Then: Should include accurate location information
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=30) :: source_lines(1))
        source_lines(1) = "program test ! comment here"
        
        formatted_error = format_enhanced_error( &
            "Error in program statement", &
            1, 9, &  ! Column 9 points to "test"
            source_lines, &
            "Fix the program name", &
            "SYNTAX_ERROR")
        
        passed = index(formatted_error, "line 1") > 0 .and. &
                index(formatted_error, "column 9") > 0
    end function

    function test_error_suggestion() result(passed)
        ! Given: Error formatting with helpful suggestion
        ! When: Including suggestion in error message
        ! Then: Should prominently display suggestion
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=20) :: source_lines(1))
        source_lines(1) = "incomplete statement"
        
        formatted_error = format_enhanced_error( &
            "Incomplete statement detected", &
            1, 1, &
            source_lines, &
            "Add missing semicolon or continue statement", &
            "INCOMPLETE_STATEMENT")
        
        passed = index(formatted_error, "Suggestion:") > 0 .and. &
                index(formatted_error, "semicolon") > 0
    end function

    function test_error_type_classification() result(passed)
        ! Given: Different error types
        ! When: Formatting errors with type classification
        ! Then: Should include error type in formatted message
        logical :: passed
        character(len=:), allocatable :: formatted_error
        character(len=:), allocatable :: source_lines(:)
        
        allocate(character(len=10) :: source_lines(1))
        source_lines(1) = "bad input"
        
        ! Test different error types
        formatted_error = format_enhanced_error( &
            "Invalid input", 1, 1, source_lines, "Fix input", "INVALID_INPUT")
        
        passed = index(formatted_error, "[INVALID_INPUT]") > 0
        
        if (passed) then
            formatted_error = format_enhanced_error( &
                "Missing construct", 1, 1, source_lines, "Add missing", "MISSING_END")
            passed = index(formatted_error, "[MISSING_END]") > 0
        end if
    end function

end program test_validation_utilities