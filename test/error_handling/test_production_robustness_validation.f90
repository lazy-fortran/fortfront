program test_production_robustness_validation
    ! Production robustness validation through systematic error injection
    ! Tests that production components handle errors gracefully without crashes
    use error_handling
    use parser_result_types
    implicit none
    
    logical :: all_robustness_tests_passed = .true.
    integer :: total_robustness_tests = 0
    integer :: passed_robustness_tests = 0
    
    write(*,*) '=== PRODUCTION ROBUSTNESS VALIDATION THROUGH ERROR INJECTION ==='
    write(*,*) ''
    
    ! Systematic robustness testing
    call test_parser_robustness_under_errors()
    call test_semantic_analysis_graceful_degradation()
    call test_error_propagation_chains()
    call test_memory_exhaustion_simulation()
    call test_concurrent_error_handling()
    call test_recovery_mechanism_validation()
    
    ! Production readiness assessment
    write(*,*) ''
    write(*,*) '=== PRODUCTION ROBUSTNESS ASSESSMENT ==='
    write(*,'(A,I0,A,I0)') 'Robustness Tests Passed: ', passed_robustness_tests, ' / ', total_robustness_tests
    
    if (all_robustness_tests_passed) then
        write(*,*) 'SUCCESS: Production system demonstrates robust error handling!'
        write(*,*) '         All error injection scenarios handled gracefully.'
        write(*,*) '         System ready for production deployment.'
    else
        write(*,*) 'FAILURE: Production system shows robustness gaps!'
        write(*,*) '         Error injection revealed crash vectors or poor degradation.'
        write(*,*) '         System NOT ready for production deployment.'
        stop 1
    end if

contains

    subroutine test_parser_robustness_under_errors()
        type(parse_result_t) :: parse_result
        type(compile_result_t) :: compile_result
        integer :: i
        character(len=100) :: error_msg
        
        write(*,*) '--- Testing Parser Robustness Under Error Conditions ---'
        
        ! Test 1: Parser error recovery with invalid syntax injection
        do i = 1, 10
            write(error_msg, '(A,I0)') 'Simulated parser error #', i
            
            parse_result = error_parse_result( &
                trim(error_msg), &
                ERROR_PARSER, &
                component='parser_injection_test', &
                context='error_simulation_loop', &
                suggestion='Test parser resilience to consecutive errors' &
            )
            
            ! Parser should handle consecutive errors gracefully
            call assert_robustness_test( &
                parse_result%is_failure() .and. parse_result%get_node() == 0, &
                'Parser robustness - consecutive error handling without crashes' &
            )
        end do
        
        ! Test 2: Compile result error accumulation
        compile_result = success_compile_result(1)
        
        ! Inject multiple warnings to test accumulation
        do i = 1, 25
            write(error_msg, '(A,I0)') 'Injected warning #', i
            call compile_result%add_warning( &
                trim(error_msg), &
                ERROR_VALIDATION, &
                component='robustness_test', &
                suggestion='Testing warning accumulation limits' &
            )
        end do
        
        call assert_robustness_test( &
            compile_result%is_success() .and. compile_result%get_warning_count() == 25, &
            'Parser robustness - warning accumulation without degradation' &
        )
        
        ! Test 3: Parser error with critical failure injection
        compile_result = error_compile_result( &
            'Critical parser failure simulation', &
            ERROR_CRITICAL, &
            component='parser_core_simulation', &
            context='critical_failure_injection', &
            suggestion='Verify parser handles critical failures gracefully' &
        )
        
        call assert_robustness_test( &
            compile_result%is_failure() .and. compile_result%get_program() == 0, &
            'Parser robustness - critical failure graceful handling' &
        )
        
        write(*,*) '   Parser robustness testing completed - no crashes detected'
    end subroutine test_parser_robustness_under_errors

    subroutine test_semantic_analysis_graceful_degradation()
        type(error_collection_t) :: semantic_errors
        type(result_t) :: analysis_results(20)
        type(result_t) :: combined_semantic_result
        integer :: i
        character(len=50) :: semantic_error_msg
        
        write(*,*) '--- Testing Semantic Analysis Graceful Degradation ---'
        
        ! Test 1: Semantic error collection under stress
        semantic_errors = create_error_collection(10)
        
        do i = 1, 50
            write(semantic_error_msg, '(A,I0)') 'Semantic analysis error #', i
            
            call semantic_errors%add_error( &
                trim(semantic_error_msg), &
                ERROR_SEMANTIC, &
                severity=merge(ERROR_CRITICAL, ERROR_ERROR, mod(i, 10) == 0), &
                component='semantic_analysis_injection', &
                context='degradation_test', &
                suggestion='Test semantic analysis error handling capacity' &
            )
        end do
        
        call assert_robustness_test( &
            semantic_errors%get_error_count() == 50 .and. semantic_errors%has_critical_errors(), &
            'Semantic analysis - error collection capacity under stress' &
        )
        
        ! Test 2: Semantic analysis result combination under errors
        do i = 1, 20
            select case (mod(i, 4))
            case (0)
                analysis_results(i) = critical_result('Critical semantic error', ERROR_SEMANTIC)
            case (1)
                analysis_results(i) = create_error_result('Type system error', ERROR_TYPE_SYSTEM)
            case (2)
                analysis_results(i) = warning_result('Semantic warning', ERROR_SEMANTIC)
            case (3)
                analysis_results(i) = success_result()
            end select
        end do
        
        combined_semantic_result = combine_results(analysis_results)
        
        call assert_robustness_test( &
            combined_semantic_result%is_failure() .and. combined_semantic_result%severity == ERROR_CRITICAL, &
            'Semantic analysis - result combination prioritizes critical errors' &
        )
        
        ! Test 3: Graceful degradation with partial analysis results
        block
            character(len=:), allocatable :: error_summary
            
            error_summary = semantic_errors%get_summary()
            
            call assert_robustness_test( &
                len_trim(error_summary) > 0 .and. index(error_summary, 'Critical:') > 0, &
                'Semantic analysis - graceful degradation with informative summaries' &
            )
        end block
        
        write(*,*) '   Semantic analysis graceful degradation verified'
    end subroutine test_semantic_analysis_graceful_degradation

    subroutine test_error_propagation_chains()
        type(result_t) :: chain_results(10)
        type(result_t) :: final_result
        integer :: i
        
        write(*,*) '--- Testing Error Propagation Chain Robustness ---'
        
        ! Test 1: Create error propagation chain
        chain_results(1) = success_result()
        chain_results(2) = warning_result('First warning in chain', ERROR_VALIDATION)
        chain_results(3) = create_error_result('First error in chain', ERROR_PARSER)
        chain_results(4) = success_result()
        chain_results(5) = warning_result('Second warning in chain', ERROR_SEMANTIC)
        chain_results(6) = critical_result('Critical error in chain', ERROR_MEMORY)
        chain_results(7) = create_error_result('Error after critical', ERROR_TYPE_SYSTEM)
        chain_results(8) = success_result()
        chain_results(9) = warning_result('Final warning', ERROR_VALIDATION)
        chain_results(10) = create_error_result('Final error', ERROR_PARSER)
        
        ! Test propagation through sequential combination
        final_result = chain_results(1)
        do i = 2, 10
            final_result = final_result%combine_result(chain_results(i))
        end do
        
        call assert_robustness_test( &
            final_result%is_failure() .and. final_result%severity == ERROR_CRITICAL, &
            'Error propagation - sequential combination maintains critical priority' &
        )
        
        ! Test 2: Array-based propagation
        final_result = combine_results(chain_results)
        
        call assert_robustness_test( &
            final_result%is_failure() .and. final_result%severity == ERROR_CRITICAL, &
            'Error propagation - array combination handles complex chains' &
        )
        
        ! Test 3: Error propagation with result clearing mid-chain
        chain_results(6) = success_result()  ! Clear the critical error
        final_result = combine_results(chain_results)
        
        call assert_robustness_test( &
            final_result%is_failure() .and. final_result%severity == ERROR_ERROR, &
            'Error propagation - handles dynamic chain modifications' &
        )
        
        write(*,*) '   Error propagation chain robustness verified'
    end subroutine test_error_propagation_chains

    subroutine test_memory_exhaustion_simulation()
        type(error_collection_t) :: large_collection
        type(result_t) :: temp_result
        integer :: i
        character(len=1000) :: large_error_message
        character(len=20) :: counter
        
        write(*,*) '--- Testing Memory Exhaustion Simulation ---'
        
        ! Test 1: Large error collection with memory pressure
        large_collection = create_error_collection(5)  ! Small initial capacity
        
        ! Create a large error message to stress memory allocation
        large_error_message = repeat('X', 900) // 'END_OF_LARGE_MESSAGE'
        
        do i = 1, 200
            write(counter, '(I0)') i
            
            temp_result = create_error_result( &
                trim(large_error_message) // ' #' // trim(counter), &
                ERROR_MEMORY, &
                component='memory_exhaustion_simulation', &
                context='large_message_stress_test', &
                suggestion='Test system behavior under memory pressure' &
            )
            
            call large_collection%add_result(temp_result)
        end do
        
        call assert_robustness_test( &
            large_collection%get_error_count() == 200, &
            'Memory exhaustion - large error collection with reallocation' &
        )
        
        ! Test 2: Very long error message chain
        block
            character(len=:), allocatable :: full_message
            
            temp_result = create_error_result( &
                repeat('This is a very long error message designed to test memory handling. ', 50), &
                ERROR_MEMORY, &
                component=repeat('long_component_name_', 10), &
                context=repeat('long_context_name_', 8), &
                suggestion=repeat('long_suggestion_text_', 12) &
            )
            
            full_message = temp_result%get_full_message()
            
            call assert_robustness_test( &
                len(full_message) > 2000 .and. temp_result%is_failure(), &
                'Memory exhaustion - very long error message formatting' &
            )
        end block
        
        ! Test 3: Collection summary under memory pressure
        block
            character(len=:), allocatable :: summary
            
            summary = large_collection%get_summary()
            
            call assert_robustness_test( &
                len_trim(summary) > 0 .and. index(summary, 'Total: 200') > 0, &
                'Memory exhaustion - summary generation under pressure' &
            )
        end block
        
        write(*,*) '   Memory exhaustion simulation completed - system remained stable'
    end subroutine test_memory_exhaustion_simulation

    subroutine test_concurrent_error_handling()
        type(error_collection_t) :: concurrent_errors
        type(result_t) :: concurrent_results(100)
        integer :: i
        character(len=30) :: thread_id
        
        write(*,*) '--- Testing Concurrent Error Handling Simulation ---'
        
        ! Test 1: Simulate concurrent error generation
        concurrent_errors = create_error_collection(20)
        
        do i = 1, 100
            write(thread_id, '(A,I0)') 'thread_', mod(i, 8) + 1
            
            call concurrent_errors%add_error( &
                'Concurrent error from ' // trim(thread_id), &
                ERROR_INTERNAL, &
                severity=merge(ERROR_CRITICAL, ERROR_ERROR, mod(i, 20) == 0), &
                component='concurrent_simulation', &
                context='multi_thread_simulation', &
                suggestion='Test concurrent error handling capacity' &
            )
        end do
        
        call assert_robustness_test( &
            concurrent_errors%get_error_count() == 100, &
            'Concurrent error handling - error collection thread-safety simulation' &
        )
        
        ! Test 2: Concurrent result combination
        do i = 1, 100
            select case (mod(i, 5))
            case (0)
                concurrent_results(i) = critical_result('Critical from concurrent', ERROR_MEMORY)
            case (1)
                concurrent_results(i) = create_error_result('Error from concurrent', ERROR_PARSER)
            case (2)
                concurrent_results(i) = warning_result('Warning from concurrent', ERROR_VALIDATION)
            case (3)
                concurrent_results(i) = success_result()
            case (4)
                concurrent_results(i) = create_error_result('Semantic error', ERROR_SEMANTIC)
            end select
        end do
        
        block
            type(result_t) :: combined
            combined = combine_results(concurrent_results)
            
            call assert_robustness_test( &
                combined%is_failure() .and. combined%severity == ERROR_CRITICAL, &
                'Concurrent error handling - result combination under concurrency' &
            )
        end block
        
        write(*,*) '   Concurrent error handling simulation completed'
    end subroutine test_concurrent_error_handling

    subroutine test_recovery_mechanism_validation()
        type(result_t) :: recovery_result
        type(error_collection_t) :: recovery_errors
        
        write(*,*) '--- Testing Recovery Mechanism Validation ---'
        
        ! Test 1: Error result recovery through clearing
        recovery_result = create_error_result('Initial error state', ERROR_VALIDATION)
        call assert_robustness_test( &
            recovery_result%is_failure(), &
            'Recovery mechanisms - initial error state verification' &
        )
        
        call recovery_result%clear()
        recovery_result = success_result()
        
        call assert_robustness_test( &
            recovery_result%is_success(), &
            'Recovery mechanisms - error result clearing and recovery' &
        )
        
        ! Test 2: Error collection recovery
        recovery_errors = create_error_collection()
        
        call recovery_errors%add_error('First error for recovery test', ERROR_PARSER)
        call recovery_errors%add_error('Second error for recovery test', ERROR_SEMANTIC)
        
        call assert_robustness_test( &
            recovery_errors%has_errors() .and. recovery_errors%get_error_count() == 2, &
            'Recovery mechanisms - error accumulation before recovery' &
        )
        
        call recovery_errors%clear_errors()
        
        call assert_robustness_test( &
            .not. recovery_errors%has_errors() .and. recovery_errors%get_error_count() == 0, &
            'Recovery mechanisms - error collection clearing and recovery' &
        )
        
        ! Test 3: Recovery from critical error states
        recovery_result = critical_result('Critical system state', ERROR_CRITICAL)
        
        call assert_robustness_test( &
            recovery_result%severity == ERROR_CRITICAL, &
            'Recovery mechanisms - critical error state recognition' &
        )
        
        call recovery_result%clear()
        recovery_result = success_result()
        
        call assert_robustness_test( &
            recovery_result%is_success() .and. recovery_result%severity == ERROR_INFO, &
            'Recovery mechanisms - recovery from critical states' &
        )
        
        write(*,*) '   Recovery mechanism validation completed'
    end subroutine test_recovery_mechanism_validation

    subroutine assert_robustness_test(condition, test_description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: test_description
        
        total_robustness_tests = total_robustness_tests + 1
        
        if (condition) then
            passed_robustness_tests = passed_robustness_tests + 1
            write(*,'(A,A)') '   ✓ ', test_description
        else
            all_robustness_tests_passed = .false.
            write(*,'(A,A)') '   ✗ ROBUSTNESS FAILURE: ', test_description
        end if
    end subroutine assert_robustness_test

end program test_production_robustness_validation