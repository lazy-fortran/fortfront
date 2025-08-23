program test_result_handling_foundation
    ! Comprehensive test for Issue #408: Unified result_t type foundation
    ! 
    ! Tests all core functionality required by Issue #408:
    ! - Core result_t type with comprehensive error handling
    ! - Factory functions for success/error results  
    ! - Error severity and category constants
    ! - Error message formatting with context
    ! - Result combination and collections
    use iso_fortran_env, only: error_unit
    use error_handling
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    print *, "=== Result Handling Foundation Tests (Issue #408) ==="
    print *, ""
    
    ! Test core result_t functionality
    call test_core_result_handling()
    
    ! Test error collection functionality
    call test_error_collection_functionality()
    
    ! Test result combination patterns
    call test_result_combination_patterns()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "SUCCESS: All result handling foundation tests passed!"
        stop 0
    else
        print *, "FAILURE: Some result handling tests failed!"
        stop 1
    end if

contains

    subroutine test_core_result_handling()
        print *, "=== Core result_t functionality tests ==="
        
        ! Test 1: Basic success result creation
        call test_success_result_creation()
        
        ! Test 2: Error result with full context
        call test_comprehensive_error_result()
        
        ! Test 3: Error message formatting
        call test_error_message_formatting()
        
        ! Test 4: Error categories and severity
        call test_error_categories_and_severity()
        
        ! Test 5: Result methods and properties
        call test_result_methods()
        
        ! Test 6: Factory functions
        call test_factory_functions()
        
        print *, ""
    end subroutine test_core_result_handling
    
    subroutine test_result_combination_patterns()
        print *, "=== Result combination patterns tests ==="
        
        ! Test 1: Basic result combination
        call test_basic_result_combination()
        
        ! Test 2: Multiple result combining
        call test_multiple_result_combination()
        
        ! Test 3: Error severity precedence
        call test_error_severity_precedence()
        
        print *, ""
    end subroutine test_result_combination_patterns
    
    subroutine test_success_result_creation()
        type(result_t) :: result
        
        call test_start("Success result creation")
        
        result = success_result()
        
        if (result%is_success() .and. .not. result%is_failure()) then
            call test_pass()
        else
            call test_fail()
        end if
    end subroutine test_success_result_creation
    
    subroutine test_comprehensive_error_result()
        type(result_t) :: result
        character(len=:), allocatable :: msg
        
        call test_start("Comprehensive error result with context")
        
        result = create_error_result( &
            "Parse error: expected identifier", &
            code=ERROR_PARSER, &
            component="parser_declarations", &
            context="parse_type_specifier", &
            suggestion="Check for missing variable name" &
        )
        
        msg = result%get_full_message()
        
        if (result%is_failure() .and. &
            result%severity == ERROR_ERROR .and. &
            result%error_code == ERROR_PARSER .and. &
            len(msg) > 20) then
            call test_pass()
        else
            call test_fail()
            print *, "  Error message: ", msg
        end if
    end subroutine test_comprehensive_error_result
    
    subroutine test_result_methods()
        type(result_t) :: result
        
        call test_start("Result methods and state management")
        
        ! Test success result methods
        result = success_result()
        if (.not. result%is_success() .or. result%is_failure()) then
            call test_fail()
            return
        end if
        
        ! Test error result methods
        call result%set_error("Test error message", code=123, component="test_component")
        if (result%is_success() .or. .not. result%is_failure() .or. result%error_code /= 123) then
            call test_fail()
            return
        end if
        
        ! Test clear functionality
        call result%clear()
        if (.not. result%is_success() .or. result%is_failure()) then
            call test_fail()
            return
        end if
        
        call test_pass()
    end subroutine test_result_methods
    
    subroutine test_error_message_formatting()
        type(result_t) :: result
        character(len=:), allocatable :: formatted, simple_msg
        
        call test_start("Error message formatting with context")
        
        result = create_error_result( &
            "Undefined variable", &
            code=ERROR_SEMANTIC, &
            component="semantic_analyzer", &
            context="variable lookup", &
            suggestion="Declare variable before use" &
        )
        
        formatted = result%get_full_message()
        simple_msg = result%get_message()
        
        if (index(formatted, "Undefined variable") > 0 .and. &
            index(formatted, "semantic_analyzer") > 0 .and. &
            index(formatted, "ERROR") > 0 .and. &
            len(simple_msg) > 5) then
            call test_pass()
        else
            call test_fail()
            print *, "  Formatted message: ", formatted
            print *, "  Simple message: ", simple_msg
        end if
    end subroutine test_error_message_formatting
    
    subroutine test_error_categories_and_severity()
        call test_start("Error categories and severity constants")
        
        ! Test that all required constants exist and have expected values
        if (ERROR_INFO == 1 .and. &
            ERROR_WARNING == 2 .and. &
            ERROR_ERROR == 3 .and. &
            ERROR_CRITICAL == 4 .and. &
            ERROR_VALIDATION == 100 .and. &
            ERROR_TYPE_SYSTEM == 200 .and. &
            ERROR_MEMORY == 300 .and. &
            ERROR_IO == 400 .and. &
            ERROR_PARSER == 500 .and. &
            ERROR_SEMANTIC == 600 .and. &
            ERROR_INTERNAL == 700) then
            call test_pass()
        else
            call test_fail()
        end if
    end subroutine test_error_categories_and_severity
    
    subroutine test_factory_functions()
        type(result_t) :: warning_res, critical_res
        
        call test_start("Factory functions for different severities")
        
        warning_res = warning_result("Warning message", code=ERROR_VALIDATION)
        critical_res = critical_result("Critical error", code=ERROR_INTERNAL)
        
        if (warning_res%severity == ERROR_WARNING .and. &
            warning_res%is_success() .and. &
            critical_res%severity == ERROR_CRITICAL .and. &
            critical_res%is_failure()) then
            call test_pass()
        else
            call test_fail()
        end if
    end subroutine test_factory_functions
    
    subroutine test_basic_result_combination()
        type(result_t) :: result1, result2, combined
        
        call test_start("Basic result combination")
        
        ! Success + Success = Success
        result1 = success_result()
        result2 = success_result()
        combined = result1%combine_result(result2)
        
        if (.not. combined%is_success()) then
            call test_fail()
            return
        end if
        
        ! Success + Error = Error
        result1 = success_result()
        result2 = create_error_result("Test error")
        combined = result1%combine_result(result2)
        
        if (combined%is_success()) then
            call test_fail()
            return
        end if
        
        call test_pass()
    end subroutine test_basic_result_combination
    
    subroutine test_multiple_result_combination()
        type(result_t) :: results(3), combined
        
        call test_start("Multiple result combination")
        
        results(1) = success_result()
        results(2) = warning_result("Warning message")
        results(3) = create_error_result("Error message")
        
        combined = combine_results(results)
        
        if (combined%is_success() .or. combined%severity /= ERROR_ERROR) then
            call test_fail()
        else
            call test_pass()
        end if
    end subroutine test_multiple_result_combination
    
    subroutine test_error_severity_precedence()
        type(result_t) :: warning_result_obj, critical_result_obj, combined
        
        call test_start("Error severity precedence")
        
        warning_result_obj = warning_result("Warning message")
        critical_result_obj = critical_result("Critical error")
        
        combined = warning_result_obj%combine_result(critical_result_obj)
        
        if (combined%severity /= ERROR_CRITICAL) then
            call test_fail()
        else
            call test_pass()
        end if
    end subroutine test_error_severity_precedence
    
    subroutine test_error_collection_functionality()
        type(error_collection_t) :: collection
        character(len=:), allocatable :: summary
        
        call test_start("Error collection functionality")
        
        collection = create_error_collection(initial_capacity=4)
        
        call collection%add_error("First error", severity=ERROR_ERROR)
        call collection%add_error("Warning message", severity=ERROR_WARNING)  
        call collection%add_error("Critical failure", severity=ERROR_CRITICAL)
        
        if (collection%get_error_count() == 3 .and. &
            collection%has_errors() .and. &
            collection%has_critical_errors() .and. &
            collection%get_worst_severity() == ERROR_CRITICAL) then
            
            ! Test summary generation
            summary = collection%get_summary()
            if (index(summary, "Total: 3") > 0) then
                call test_pass()
            else
                call test_fail()
                print *, "  Summary issue: ", summary
            end if
        else
            call test_fail()
            print *, "  Error count: ", collection%get_error_count()
            print *, "  Has errors: ", collection%has_errors()
            print *, "  Has critical: ", collection%has_critical_errors()
            print *, "  Worst severity: ", collection%get_worst_severity()
        end if
    end subroutine test_error_collection_functionality
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A)', advance='no') "Testing: " // test_name // "  ... "
    end subroutine test_start
    
    subroutine test_pass()
        pass_count = pass_count + 1
        print *, "PASSED"
    end subroutine test_pass
    
    subroutine test_fail()
        print *, "FAILED"
    end subroutine test_fail
    
end program test_result_handling_foundation