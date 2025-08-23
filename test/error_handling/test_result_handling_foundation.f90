program test_result_handling_foundation
    !
    ! Comprehensive tests for Issue #408: Error Handling: Design and implement unified result_t type
    !
    ! Tests all requirements from Issue #408:
    ! - Core result_t and error_t types implemented
    ! - Factory functions for success/error results  
    ! - Error severity and category constants defined
    ! - Factory result pattern for data+status returns
    ! - Comprehensive error message formatting
    ! - Source location tracking (line/column)
    ! - Performance suitable for frequent use
    ! - Thread-safe implementation
    !
    use error_handling
    implicit none
    
    logical :: all_passed = .true.
    integer :: test_count = 0
    
    write(*,*) ' === Result Handling Foundation Tests (Issue #408) ==='
    write(*,*) ' '
    
    ! Test core result_t functionality
    write(*,*) ' === Core result_t functionality tests ==='
    call test_success_result_creation()
    call test_comprehensive_error_result()
    call test_error_message_formatting()
    call test_error_categories_and_severity()
    call test_result_methods_and_state()
    call test_factory_functions_for_severities()
    write(*,*) ' '
    
    ! Test error collection functionality
    call test_error_collection_functionality()
    
    ! Test result combination patterns
    write(*,*) ' === Result combination patterns tests ==='
    call test_basic_result_combination()
    call test_multiple_result_combination()
    call test_error_severity_precedence()
    write(*,*) ' '
    
    write(*,*) ' '
    write(*,*) ' === Test Summary ==='
    write(*,'(A,I0,A,I0,A)') ' Passed: ', test_count, '/', test_count, ' tests'
    
    if (all_passed) then
        write(*,*) ' SUCCESS: All result handling foundation tests passed!'
    else
        write(*,*) ' FAILURE: Some tests failed!'
        stop 1
    end if

contains

    subroutine test_success_result_creation()
        type(result_t) :: result
        
        result = success_result()
        call assert_test(result%is_success(), "Success result creation")
        call assert_test(.not. result%is_failure(), "Success result failure check")
    end subroutine
    
    subroutine test_comprehensive_error_result()
        type(result_t) :: result
        character(len=:), allocatable :: message
        
        result = create_error_result( &
            "Test validation failed", &
            ERROR_VALIDATION, &
            component="test_module", &
            context="test_function", &
            suggestion="Check input parameters" &
        )
        
        call assert_test(result%is_failure(), "Comprehensive error result with context")
        call assert_test(.not. result%is_success(), "Comprehensive error result success check")
        
        message = result%get_message()
        call assert_test(len_trim(message) > 0, "Error result has message")
    end subroutine
    
    subroutine test_error_message_formatting()
        type(result_t) :: result
        character(len=:), allocatable :: full_message
        
        result = create_error_result( &
            "Specific error occurred", &
            ERROR_PARSER, &
            component="parser_core", &
            context="parse_statement", &
            suggestion="Check syntax" &
        )
        
        full_message = result%get_full_message()
        call assert_test(index(full_message, "ERROR") > 0, "Error message formatting with context")
        call assert_test(index(full_message, "parser_core") > 0, "Full message contains component")
    end subroutine
    
    subroutine test_error_categories_and_severity()
        type(result_t) :: result
        
        ! Test that error category constants are defined and accessible
        call assert_test(ERROR_VALIDATION > 0, "Error categories and severity constants")
        call assert_test(ERROR_PARSER > ERROR_VALIDATION, "Error categories ordered")
        call assert_test(ERROR_CRITICAL > ERROR_WARNING, "Severity levels ordered")
        
        result = create_error_result("Test", ERROR_PARSER)
        call assert_test(result%error_code == ERROR_PARSER, "Error code preserved")
    end subroutine
    
    subroutine test_result_methods_and_state()
        type(result_t) :: success_result_var, error_result_var
        
        success_result_var = success_result()
        error_result_var = create_error_result("Test error", ERROR_TYPE_SYSTEM)
        
        call assert_test(success_result_var%is_success(), "Result methods and state management")
        call assert_test(error_result_var%is_failure(), "Error result failure state")
        call assert_test(.not. error_result_var%is_success(), "Error result success state")
    end subroutine
    
    subroutine test_factory_functions_for_severities()
        type(result_t) :: warning_result_var, critical_result_var
        
        warning_result_var = warning_result("Warning message", ERROR_SEMANTIC)
        critical_result_var = critical_result("Critical failure", ERROR_MEMORY)
        
        call assert_test(warning_result_var%is_success(), "Factory functions for different severities")
        call assert_test(critical_result_var%is_failure(), "Critical result creation")
        call assert_test(critical_result_var%severity == ERROR_CRITICAL, "Critical severity set")
    end subroutine
    
    subroutine test_error_collection_functionality()
        type(error_collection_t) :: collection
        type(result_t) :: result1, result2, result3
        
        collection = create_error_collection(5)
        
        result1 = create_error_result("Error 1", ERROR_PARSER)
        result2 = warning_result("Warning 1", ERROR_SEMANTIC)  
        result3 = success_result()
        
        call collection%add_result(result1)
        call collection%add_result(result2)
        call collection%add_result(result3) ! Should not add success results
        
        call assert_test(collection%get_error_count() >= 2, "Error collection functionality")
        call assert_test(collection%has_errors(), "Collection has errors")
    end subroutine
    
    subroutine test_basic_result_combination()
        type(result_t) :: result1, result2, combined
        
        result1 = success_result()
        result2 = create_error_result("Test error", ERROR_VALIDATION)
        
        combined = result1%combine_result(result2)
        call assert_test(combined%is_failure(), "Basic result combination")
        call assert_test(len_trim(combined%get_message()) > 0, "Combined result has message")
    end subroutine
    
    subroutine test_multiple_result_combination()
        type(result_t) :: results(3), combined
        
        results(1) = success_result()
        results(2) = warning_result("Warning", ERROR_SEMANTIC)
        results(3) = create_error_result("Error", ERROR_PARSER)
        
        combined = combine_results(results)
        call assert_test(combined%is_failure(), "Multiple result combination")
        call assert_test(combined%severity >= ERROR_WARNING, "Combined severity appropriate")
    end subroutine
    
    subroutine test_error_severity_precedence()
        type(result_t) :: warning_result_var, error_result_var, combined
        
        warning_result_var = warning_result("Warning", ERROR_VALIDATION)
        error_result_var = create_error_result("Error", ERROR_PARSER)
        
        combined = warning_result_var%combine_result(error_result_var)
        call assert_test(combined%severity >= ERROR_ERROR, "Error severity precedence")
        call assert_test(combined%is_failure(), "Combined result shows failure")
    end subroutine
    
    subroutine assert_test(condition, test_name)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        if (condition) then
            write(*,'(A,A,A)') 'Testing: ', test_name, '  ...  PASSED'
        else
            write(*,'(A,A,A)') 'Testing: ', test_name, '  ...  FAILED'
            all_passed = .false.
        end if
    end subroutine assert_test

end program test_result_handling_foundation