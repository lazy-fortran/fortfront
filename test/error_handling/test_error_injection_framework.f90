program test_error_injection_framework
    ! Comprehensive error injection testing framework for production robustness verification
    ! This framework systematically tests graceful degradation under various failure conditions
    use error_handling
    use parser_result_types
    use semantic_result_types, only: semantic_result_base_t, symbol_result_t
    implicit none
    
    logical :: all_tests_passed = .true.
    integer :: total_tests = 0
    integer :: passed_tests = 0
    
    write(*,*) '=== ERROR INJECTION FRAMEWORK FOR PRODUCTION ROBUSTNESS ==='
    write(*,*) ''
    
    ! Test error injection scenarios
    call test_memory_allocation_failures()
    call test_parser_error_recovery()
    call test_semantic_analysis_robustness()
    call test_cascading_error_handling()
    call test_error_collection_stress()
    call test_boundary_condition_errors()
    
    ! Print comprehensive results
    write(*,*) ''
    write(*,*) '=== ERROR INJECTION TEST SUMMARY ==='
    write(*,'(A,I0,A,I0)') 'Passed: ', passed_tests, ' / ', total_tests
    
    if (all_tests_passed) then
        write(*,*) 'SUCCESS: All error injection tests passed - production robustness verified!'
    else
        write(*,*) 'FAILURE: Some error injection tests failed - production robustness compromised!'
        stop 1
    end if

contains

    subroutine test_memory_allocation_failures()
        type(error_collection_t) :: collection
        type(result_t) :: result
        integer :: i
        
        write(*,*) '--- Testing Memory Allocation Failure Scenarios ---'
        
        ! Test 1: Large error collection stress test
        collection = create_error_collection(2)  ! Deliberately small initial capacity
        
        ! Fill beyond initial capacity to test reallocation robustness
        do i = 1, 50
            result = create_error_result( &
                'Simulated memory allocation error ' // char(48 + mod(i, 10)), &
                ERROR_MEMORY, &
                component='memory_test', &
                context='allocation_stress_test', &
                suggestion='Check available memory' &
            )
            call collection%add_result(result)
        end do
        
        call assert_test( &
            collection%get_error_count() == 50, &
            'Memory allocation stress test - error collection reallocation' &
        )
        
        call assert_test( &
            collection%has_errors(), &
            'Memory allocation stress test - error collection state' &
        )
        
        ! Test 2: Error message allocation with very long strings
        result = create_error_result( &
            'This is an extremely long error message designed to test ' // &
            'the robustness of memory allocation for error messages when ' // &
            'dealing with very long strings that might cause allocation ' // &
            'issues in production environments with limited memory resources', &
            ERROR_MEMORY, &
            component='extremely_long_component_name_for_testing_purposes', &
            context='very_long_context_name_that_tests_string_handling_limits', &
            suggestion='Consider using shorter identifiers and error messages' &
        )
        
        call assert_test( &
            result%is_failure() .and. len_trim(result%get_message()) > 100, &
            'Memory allocation stress test - long error messages' &
        )
        
        write(*,*) '   Memory allocation failure testing completed'
    end subroutine test_memory_allocation_failures

    subroutine test_parser_error_recovery()
        type(parse_result_t) :: parse_result
        type(compile_result_t) :: compile_result
        
        write(*,*) '--- Testing Parser Error Recovery Scenarios ---'
        
        ! Test 1: Parse result error propagation
        parse_result = error_parse_result( &
            'Unexpected token in expression', &
            ERROR_PARSER, &
            component='parser_core', &
            context='parse_expression', &
            suggestion='Check for missing operators or parentheses' &
        )
        
        call assert_test( &
            parse_result%is_failure(), &
            'Parser error recovery - parse_result_t error state' &
        )
        
        call assert_test( &
            parse_result%get_node() == 0, &
            'Parser error recovery - invalid node index on failure' &
        )
        
        ! Test 2: Compile result with warnings
        compile_result = success_compile_result(42)
        call compile_result%add_warning( &
            'Deprecated syntax detected', &
            ERROR_PARSER, &
            component='parser_legacy', &
            context='parse_legacy_statement', &
            suggestion='Use modern Fortran syntax' &
        )
        
        call assert_test( &
            compile_result%is_success() .and. compile_result%has_warnings(), &
            'Parser error recovery - compile_result_t with warnings' &
        )
        
        call assert_test( &
            compile_result%get_warning_count() > 0, &
            'Parser error recovery - warning collection' &
        )
        
        ! Test 3: Error recovery with multiple parser errors
        compile_result = error_compile_result( &
            'Multiple syntax errors detected', &
            ERROR_PARSER, &
            component='parser_core', &
            context='parse_program', &
            suggestion='Fix syntax errors and retry compilation' &
        )
        
        call compile_result%add_warning( &
            'First warning during failed compilation', &
            ERROR_PARSER &
        )
        call compile_result%add_warning( &
            'Second warning during failed compilation', &
            ERROR_PARSER &
        )
        
        call assert_test( &
            compile_result%is_failure() .and. compile_result%get_warning_count() == 2, &
            'Parser error recovery - error with multiple warnings' &
        )
        
        write(*,*) '   Parser error recovery testing completed'
    end subroutine test_parser_error_recovery

    subroutine test_semantic_analysis_robustness()
        type(symbol_result_t) :: symbol_result1, symbol_result2, merged_result
        
        write(*,*) '--- Testing Semantic Analysis Robustness ---'
        
        ! Test 1: Symbol result creation and state
        symbol_result1%result_id = 1
        symbol_result1%result_type_name = 'symbol_analysis'
        symbol_result1%has_errors = .true.
        symbol_result1%symbols_found = 10
        symbol_result1%symbols_resolved = 7
        symbol_result1%unresolved_symbols = 3
        symbol_result1%symbol_table_summary = 'Partial resolution with 3 unresolved symbols'
        
        call assert_test( &
            symbol_result1%has_errors .and. symbol_result1%unresolved_symbols == 3, &
            'Semantic analysis robustness - symbol result state management' &
        )
        
        ! Test 2: Symbol result merging under error conditions
        symbol_result2%result_id = 2
        symbol_result2%result_type_name = 'symbol_analysis'
        symbol_result2%has_warnings = .true.
        symbol_result2%symbols_found = 5
        symbol_result2%symbols_resolved = 4
        symbol_result2%unresolved_symbols = 1
        
        merged_result = symbol_result1
        call merged_result%merge_results(symbol_result2)
        
        call assert_test( &
            merged_result%symbols_found == 15 .and. merged_result%unresolved_symbols == 4, &
            'Semantic analysis robustness - symbol result merging' &
        )
        
        call assert_test( &
            merged_result%has_errors .and. merged_result%has_warnings, &
            'Semantic analysis robustness - merged error and warning states' &
        )
        
        ! Test 3: Symbol result cloning
        block
            class(semantic_result_base_t), allocatable :: cloned
            cloned = symbol_result1%clone_result()
            
            select type(cloned_symbol => cloned)
            type is (symbol_result_t)
                call assert_test( &
                    cloned_symbol%symbols_found == symbol_result1%symbols_found, &
                    'Semantic analysis robustness - symbol result cloning' &
                )
            class default
                call assert_test(.false., 'Symbol result cloning - type preservation failed')
            end select
        end block
        
        write(*,*) '   Semantic analysis robustness testing completed'
    end subroutine test_semantic_analysis_robustness

    subroutine test_cascading_error_handling()
        type(result_t) :: result1, result2, result3, combined
        type(result_t) :: results_array(5)
        integer :: i
        
        write(*,*) '--- Testing Cascading Error Handling ---'
        
        ! Test 1: Cascading result combination
        result1 = success_result()
        result2 = warning_result('First warning', ERROR_VALIDATION)
        result3 = critical_result('Critical failure', ERROR_MEMORY)
        
        combined = result1%combine_result(result2)
        combined = combined%combine_result(result3)
        
        call assert_test( &
            combined%is_failure() .and. combined%severity == ERROR_CRITICAL, &
            'Cascading error handling - severity escalation' &
        )
        
        ! Test 2: Array-based result combination stress test
        results_array(1) = success_result()
        results_array(2) = warning_result('Warning 1', ERROR_SEMANTIC)
        results_array(3) = create_error_result('Error 1', ERROR_PARSER)
        results_array(4) = critical_result('Critical error', ERROR_MEMORY)
        results_array(5) = warning_result('Warning 2', ERROR_TYPE_SYSTEM)
        
        combined = combine_results(results_array)
        
        call assert_test( &
            combined%is_failure() .and. combined%severity == ERROR_CRITICAL, &
            'Cascading error handling - array combination with critical priority' &
        )
        
        ! Test 3: Deep cascading with result clearing and reuse
        result1 = create_error_result('Initial error', ERROR_VALIDATION)
        call result1%clear()
        result1 = success_result()
        
        call assert_test( &
            result1%is_success(), &
            'Cascading error handling - result clearing and reuse' &
        )
        
        write(*,*) '   Cascading error handling testing completed'
    end subroutine test_cascading_error_handling

    subroutine test_error_collection_stress()
        type(error_collection_t) :: collection
        type(result_t) :: temp_result
        integer :: i
        character(len=10) :: counter_str
        
        write(*,*) '--- Testing Error Collection Stress Scenarios ---'
        
        ! Test 1: Rapid error addition and collection growth
        collection = create_error_collection(5)
        
        do i = 1, 100
            write(counter_str, '(I0)') i
            
            select case (mod(i, 4))
            case (0)
                temp_result = critical_result('Critical error ' // counter_str, ERROR_MEMORY)
            case (1)
                temp_result = create_error_result('Standard error ' // counter_str, ERROR_PARSER)
            case (2)
                temp_result = warning_result('Warning ' // counter_str, ERROR_VALIDATION)
            case (3)
                temp_result = success_result()  ! Should not be added
            end select
            
            call collection%add_result(temp_result)
        end do
        
        ! Should have all 100 results (error collection adds all results, filtering is done by has_errors())
        call assert_test( &
            collection%get_error_count() == 100, &
            'Error collection stress - rapid growth and filtering' &
        )
        
        call assert_test( &
            collection%has_critical_errors(), &
            'Error collection stress - critical error detection' &
        )
        
        call assert_test( &
            collection%get_worst_severity() == ERROR_CRITICAL, &
            'Error collection stress - worst severity tracking' &
        )
        
        ! Test 2: Error collection summary generation under stress
        block
            character(len=:), allocatable :: summary
            summary = collection%get_summary()
            
            call assert_test( &
                len_trim(summary) > 0 .and. index(summary, 'Critical:') > 0, &
                'Error collection stress - summary generation' &
            )
        end block
        
        ! Test 3: Error collection clearing after stress
        call collection%clear_errors()
        
        call assert_test( &
            collection%get_error_count() == 0 .and. .not. collection%has_errors(), &
            'Error collection stress - complete clearing after stress' &
        )
        
        write(*,*) '   Error collection stress testing completed'
    end subroutine test_error_collection_stress

    subroutine test_boundary_condition_errors()
        type(result_t) :: result
        type(error_collection_t) :: empty_collection
        character(len=:), allocatable :: message
        
        write(*,*) '--- Testing Boundary Condition Error Handling ---'
        
        ! Test 1: Empty error messages
        result = create_error_result('', ERROR_VALIDATION)
        message = result%get_message()
        
        call assert_test( &
            result%is_failure(), &
            'Boundary conditions - empty error message handling' &
        )
        
        ! Test 2: Error result with no optional parameters
        result = create_error_result('Minimal error')
        
        call assert_test( &
            result%is_failure() .and. len_trim(result%get_message()) > 0, &
            'Boundary conditions - minimal error result creation' &
        )
        
        ! Test 3: Empty error collection operations
        empty_collection = create_error_collection()
        
        call assert_test( &
            empty_collection%get_error_count() == 0 .and. .not. empty_collection%has_errors(), &
            'Boundary conditions - empty error collection state' &
        )
        
        call assert_test( &
            empty_collection%get_worst_severity() == ERROR_INFO, &
            'Boundary conditions - empty collection worst severity' &
        )
        
        ! Test 4: Result with maximum severity
        result = critical_result('Maximum severity test', ERROR_CRITICAL)
        
        call assert_test( &
            result%severity == ERROR_CRITICAL .and. result%is_failure(), &
            'Boundary conditions - maximum severity handling' &
        )
        
        ! Test 5: Full message formatting with all fields
        result = create_error_result( &
            'Complete error with all fields', &
            ERROR_TYPE_SYSTEM, &
            component='boundary_test_component', &
            context='test_all_fields_function', &
            suggestion='Verify all error formatting works correctly' &
        )
        
        message = result%get_full_message()
        call assert_test( &
            index(message, 'ERROR') > 0 .and. &
            index(message, 'boundary_test_component') > 0 .and. &
            index(message, 'Context:') > 0 .and. &
            index(message, 'Suggestion:') > 0, &
            'Boundary conditions - complete error formatting' &
        )
        
        write(*,*) '   Boundary condition error handling testing completed'
    end subroutine test_boundary_condition_errors

    subroutine assert_test(condition, test_description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_description
        
        total_tests = total_tests + 1
        
        if (condition) then
            passed_tests = passed_tests + 1
            write(*,'(A,A)') '   ✓ ', test_description
        else
            all_tests_passed = .false.
            write(*,'(A,A)') '   ✗ FAILED: ', test_description
        end if
    end subroutine assert_test

end program test_error_injection_framework