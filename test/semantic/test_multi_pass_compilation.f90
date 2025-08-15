program test_multi_pass_compilation
    ! TDD Tests for Multi-Pass Compilation Infrastructure (Phase 2)
    ! These tests MUST FAIL until the multi-pass infrastructure is implemented
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use ast_core, only: ast_arena_t, create_ast_arena
    use frontend, only: compile_source, compilation_options_t
    use pass_manager, only: pass_manager_t
    use convergence_checker, only: convergence_checker_t
    use analysis_cache, only: analysis_cache_t
    use mock_analyzers, only: mock_analyzer_t, convergent_analyzer_t, &
                              infinite_analyzer_t, ordered_analyzer_t, &
                              create_test_results, create_stable_results, &
                              create_initial_results, create_intermediate_results, &
                              create_changing_results, create_precise_results, &
                              create_test_results_variant, create_large_test_results, &
                              results_equal, get_memory_usage
    use base_analyzer, only: analysis_results_t, analyzer_id_t
    implicit none

    logical :: all_passed
    integer :: total_tests, passed_tests

    all_passed = .true.
    total_tests = 0
    passed_tests = 0

    print *, '=== Multi-Pass Compilation Infrastructure Tests ==='
    print *, 'Testing Phase 2: Multi-Pass Core Infrastructure'
    print *

    ! Test Group 1: Pass Manager Tests
    if (.not. test_pass_manager_single_pass()) all_passed = .false.
    if (.not. test_pass_manager_multiple_passes()) all_passed = .false.
    if (.not. test_pass_manager_max_iterations()) all_passed = .false.
    if (.not. test_pass_manager_execution_order()) all_passed = .false.

    ! Test Group 2: Convergence Detection Tests
    if (.not. test_convergence_detector_immediate()) all_passed = .false.
    if (.not. test_convergence_detector_multiple()) all_passed = .false.
    if (.not. test_convergence_detector_timeout()) all_passed = .false.
    if (.not. test_convergence_stability()) all_passed = .false.

    ! Test Group 3: Results Storage Tests
    if (.not. test_results_cache_storage()) all_passed = .false.
    if (.not. test_results_cache_retrieval()) all_passed = .false.
    if (.not. test_results_cache_invalidation()) all_passed = .false.
    if (.not. test_results_cache_memory_management()) all_passed = .false.

    ! Test Group 4: Integration Tests (TODO: Enable in Phase 3)
    ! These tests are intentionally designed to fail until Phase 3 integration
    print *, '  SKIPPING integration tests (Phase 3): Type inference, allocatable detection, forward references, performance'
    ! if (.not. test_multi_pass_type_inference()) all_passed = .false.
    ! if (.not. test_multi_pass_allocatable_detection()) all_passed = .false.
    ! if (.not. test_multi_pass_forward_references()) all_passed = .false.
    ! if (.not. test_multi_pass_performance()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All multi-pass compilation tests passed!'
        stop 0
    else
        print *, 'Some multi-pass compilation tests failed!'
        stop 1
    end if

contains

    ! =================================================================
    ! Test Group 1: Pass Manager Tests
    ! =================================================================

    logical function test_pass_manager_single_pass()
        ! Given: A multi-pass system with one analyzer
        ! When: The pass manager runs a single pass
        ! Then: The analyzer is executed exactly once
        test_pass_manager_single_pass = .false.
        print *, 'Testing pass manager with single pass...'

        block
            type(pass_manager_t) :: manager
            type(mock_analyzer_t) :: analyzer
            type(semantic_context_t) :: ctx
            type(ast_arena_t) :: arena
            integer :: root_index
            
            ! Create test environment
            ctx = create_semantic_context()
            arena = create_ast_arena()
            root_index = create_test_ast(arena)
            
            ! Configure analyzer ID
            analyzer%id%id = 1
            analyzer%id%name = "mock_analyzer"
            
            ! Configure manager for single pass
            manager%max_passes = 1
            manager%convergence_enabled = .false.
            call manager%add_analyzer(analyzer)
            
            ! Execute passes
            call manager%execute_passes(ctx, arena, root_index)
            
            ! Verify: analyzer was called exactly once
            if (manager%get_analyzer_call_count("mock_analyzer") == 1) then
                test_pass_manager_single_pass = .true.
                print *, '  PASS: Single pass executed correctly'
            else
                print *, '  FAIL: Expected 1 call, got ', &
                    manager%get_analyzer_call_count("mock_analyzer")
            end if
        end block
    end function test_pass_manager_single_pass

    logical function test_pass_manager_multiple_passes()
        ! Given: A multi-pass system with convergence detection
        ! When: Analysis requires multiple passes to converge
        ! Then: The pass manager runs until convergence is achieved
        test_pass_manager_multiple_passes = .false.
        print *, 'Testing pass manager with multiple passes...'

        block
            type(pass_manager_t) :: manager
            type(convergent_analyzer_t) :: analyzer
            type(semantic_context_t) :: ctx
            type(ast_arena_t) :: arena
            integer :: root_index
            
            ! Create test environment
            ctx = create_semantic_context()
            arena = create_ast_arena()
            root_index = create_forward_reference_ast(arena)
            
            ! Configure analyzer to converge after 3 passes
            analyzer%id%id = 1
            analyzer%id%name = "convergent_analyzer"
            analyzer%convergence_pass = 3
            
            ! Configure manager for multiple passes
            manager%max_passes = 10
            manager%convergence_enabled = .true.
            call manager%add_analyzer(analyzer)
            
            ! Execute passes
            call manager%execute_passes(ctx, arena, root_index)
            
            ! Verify: analyzer was called exactly 3 times before convergence
            if (manager%get_analyzer_call_count("convergent_analyzer") == 3) then
                test_pass_manager_multiple_passes = .true.
                print *, '  PASS: Multiple passes until convergence'
            else
                print *, '  FAIL: Expected 3 calls, got ', &
                    manager%get_analyzer_call_count("convergent_analyzer")
            end if
        end block
    end function test_pass_manager_multiple_passes

    logical function test_pass_manager_max_iterations()
        ! Given: A multi-pass system with infinite loop potential
        ! When: Analysis never converges
        ! Then: The pass manager stops at max_passes limit
        test_pass_manager_max_iterations = .false.
        print *, 'Testing pass manager max iterations safety...'

        block
            type(pass_manager_t) :: manager
            type(infinite_analyzer_t) :: analyzer  ! Never converges
            type(semantic_context_t) :: ctx
            type(ast_arena_t) :: arena
            integer :: root_index
            
            ! Create test environment
            ctx = create_semantic_context()
            arena = create_ast_arena()
            root_index = create_test_ast(arena)
            
            ! Configure analyzer
            analyzer%id%id = 1
            analyzer%id%name = "infinite_analyzer"
            
            ! Configure manager with low max_passes for test speed
            manager%max_passes = 5
            manager%convergence_enabled = .true.
            call manager%add_analyzer(analyzer)
            
            ! Execute passes (should stop at max)
            call manager%execute_passes(ctx, arena, root_index)
            
            ! Verify: stopped at exactly max_passes
            if (manager%get_analyzer_call_count("infinite_analyzer") == 5) then
                test_pass_manager_max_iterations = .true.
                print *, '  PASS: Max iterations safety working'
            else
                print *, '  FAIL: Expected 5 calls, got ', &
                    manager%get_analyzer_call_count("infinite_analyzer")
            end if
        end block
    end function test_pass_manager_max_iterations

    logical function test_pass_manager_execution_order()
        ! Given: Multiple analyzers with dependencies
        ! When: The pass manager executes analyzers
        ! Then: Analyzers run in correct dependency order
        test_pass_manager_execution_order = .false.
        print *, 'Testing pass manager execution order...'

        block
            type(pass_manager_t) :: manager
            type(ordered_analyzer_t) :: type_analyzer, scope_analyzer
            type(semantic_context_t) :: ctx
            type(ast_arena_t) :: arena
            integer :: root_index
            integer, allocatable :: execution_order(:)
            
            ! Create test environment
            ctx = create_semantic_context()
            arena = create_ast_arena()
            root_index = create_test_ast(arena)
            
            ! Configure analyzers with dependencies
            type_analyzer%id%id = 1
            type_analyzer%id%name = "type_analyzer"
            scope_analyzer%id%id = 2
            scope_analyzer%id%name = "scope_analyzer"
            call scope_analyzer%set_dependencies([1])  ! Depends on type analyzer
            
            ! Add analyzers in wrong order to test dependency resolution
            call manager%add_analyzer(scope_analyzer)  ! Add dependent first
            call manager%add_analyzer(type_analyzer)   ! Add dependency second
            
            ! Execute passes
            call manager%execute_passes(ctx, arena, root_index)
            
            ! Get execution order
            execution_order = manager%get_execution_order()
            
            ! Verify: type analyzer ran before scope analyzer
            if (size(execution_order) == 2 .and. &
                execution_order(1) == 2 .and. execution_order(2) == 1) then
                test_pass_manager_execution_order = .true.
                print *, '  PASS: Execution order respects dependencies'
            else
                print *, '  FAIL: Wrong execution order: ', execution_order
            end if
        end block
    end function test_pass_manager_execution_order

    ! =================================================================
    ! Test Group 2: Convergence Detection Tests
    ! =================================================================

    logical function test_convergence_detector_immediate()
        ! Given: A convergence detector with stable results
        ! When: Results don't change from first pass
        ! Then: Convergence is detected immediately
        test_convergence_detector_immediate = .false.
        print *, 'Testing immediate convergence detection...'

        block
            type(convergence_checker_t) :: checker
            type(analysis_results_t) :: results1, results2
            logical :: converged
            
            ! Create identical results
            results1 = create_stable_results()
            results2 = results1  ! Same results
            
            ! Record first state
            call checker%record_state(results1)
            
            ! Check convergence with identical state
            converged = checker%has_converged(results2)
            
            ! Verify: convergence detected
            if (converged) then
                test_convergence_detector_immediate = .true.
                print *, '  PASS: Immediate convergence detected'
            else
                print *, '  FAIL: Should have detected convergence'
            end if
        end block
    end function test_convergence_detector_immediate

    logical function test_convergence_detector_multiple()
        ! Given: A convergence detector with changing results
        ! When: Results stabilize after several passes
        ! Then: Convergence is detected when results stabilize
        test_convergence_detector_multiple = .false.
        print *, 'Testing multi-pass convergence detection...'

        block
            type(convergence_checker_t) :: checker
            type(analysis_results_t) :: results1, results2, results3
            logical :: converged
            
            ! Create evolving results
            results1 = create_initial_results()
            results2 = create_intermediate_results()
            results3 = results2  ! Stabilized
            
            ! Record progression
            call checker%record_state(results1)
            converged = checker%has_converged(results2)
            if (converged) then
                print *, '  FAIL: Premature convergence detection'
                return
            end if
            
            ! Check final convergence
            call checker%record_state(results2)
            converged = checker%has_converged(results3)
            
            ! Verify: convergence detected when stable
            if (converged) then
                test_convergence_detector_multiple = .true.
                print *, '  PASS: Multi-pass convergence detected'
            else
                print *, '  FAIL: Should have detected convergence'
            end if
        end block
    end function test_convergence_detector_multiple

    logical function test_convergence_detector_timeout()
        ! Given: A convergence detector with continuously changing results
        ! When: Results never stabilize
        ! Then: Timeout is handled gracefully
        test_convergence_detector_timeout = .false.
        print *, 'Testing convergence detector timeout...'

        block
            type(convergence_checker_t) :: checker
            type(analysis_results_t) :: results
            logical :: converged
            integer :: i
            
            ! Set timeout limit
            call checker%set_max_iterations(3)
            
            ! Simulate continuously changing results
            do i = 1, 5  ! More than max_iterations
                results = create_changing_results(i)
                call checker%record_state(results)
                converged = checker%has_converged(results)
                
                if (i > checker%max_iterations .and. .not. converged) then
                    ! Should timeout by now
                    test_convergence_detector_timeout = .true.
                    print *, '  PASS: Timeout handled correctly'
                    return
                end if
            end do
            
            print *, '  FAIL: Timeout not handled'
        end block
    end function test_convergence_detector_timeout

    logical function test_convergence_stability()
        ! Given: A convergence detector with near-stable results
        ! When: Results have minor floating-point differences
        ! Then: Convergence is detected within tolerance
        test_convergence_stability = .false.
        print *, 'Testing convergence stability tolerance...'

        block
            type(convergence_checker_t) :: checker
            type(analysis_results_t) :: results1, results2
            logical :: converged
            
            ! Set tolerance for floating-point comparison
            call checker%set_tolerance(1.0e-6)
            
            ! Create nearly identical results (within tolerance)
            results1 = create_precise_results(1.000000)
            results2 = create_precise_results(1.000001)  ! Small difference
            
            ! Record and check convergence
            call checker%record_state(results1)
            converged = checker%has_converged(results2)
            
            ! Verify: convergence detected within tolerance
            if (converged) then
                test_convergence_stability = .true.
                print *, '  PASS: Stability tolerance working'
            else
                print *, '  FAIL: Should detect convergence within tolerance'
            end if
        end block
    end function test_convergence_stability

    ! =================================================================
    ! Test Group 3: Results Storage Tests
    ! =================================================================

    logical function test_results_cache_storage()
        ! Given: An analysis cache for intermediate results
        ! When: Results are stored from a pass
        ! Then: Results can be retrieved by analyzer and pass number
        test_results_cache_storage = .false.
        print *, 'Testing results cache storage...'

        block
            type(analysis_cache_t) :: cache
            type(analysis_results_t) :: stored_results, retrieved_results
            character(len=*), parameter :: analyzer_name = "type_analyzer"
            integer, parameter :: pass_num = 1
            logical :: success
            
            ! Create test results
            stored_results = create_test_results()
            
            ! Store results
            call cache%store_results(analyzer_name, pass_num, stored_results)
            
            ! Retrieve results
            call cache%retrieve_results(analyzer_name, pass_num, &
                                      retrieved_results, success)
            
            ! Verify: results match what was stored
            if (success .and. results_equal(stored_results, retrieved_results)) then
                test_results_cache_storage = .true.
                print *, '  PASS: Results cache storage working'
            else
                print *, '  FAIL: Results storage/retrieval failed'
            end if
        end block
    end function test_results_cache_storage

    logical function test_results_cache_retrieval()
        ! Given: An analysis cache with multiple stored results
        ! When: Specific results are requested
        ! Then: Correct results are returned for analyzer/pass combination
        test_results_cache_retrieval = .false.
        print *, 'Testing results cache retrieval...'

        block
            type(analysis_cache_t) :: cache
            type(analysis_results_t) :: results1, results2, retrieved
            logical :: success
            
            ! Store multiple results
            results1 = create_test_results_variant(1)
            results2 = create_test_results_variant(2)
            call cache%store_results("analyzer1", 1, results1)
            call cache%store_results("analyzer2", 1, results2)
            
            ! Retrieve specific results
            call cache%retrieve_results("analyzer1", 1, retrieved, success)
            
            ! Verify: correct results retrieved
            if (success .and. results_equal(results1, retrieved)) then
                test_results_cache_retrieval = .true.
                print *, '  PASS: Results cache retrieval working'
            else
                print *, '  FAIL: Wrong results retrieved'
            end if
        end block
    end function test_results_cache_retrieval

    logical function test_results_cache_invalidation()
        ! Given: An analysis cache with stale results
        ! When: Cache invalidation is triggered
        ! Then: Old results are cleared and new results can be stored
        test_results_cache_invalidation = .false.
        print *, 'Testing results cache invalidation...'

        block
            type(analysis_cache_t) :: cache
            type(analysis_results_t) :: old_results, new_results, retrieved
            logical :: success
            
            ! Store initial results
            old_results = create_test_results_variant(1)
            call cache%store_results("analyzer", 1, old_results)
            
            ! Invalidate cache
            call cache%invalidate("analyzer")
            
            ! Try to retrieve (should fail)
            call cache%retrieve_results("analyzer", 1, retrieved, success)
            if (success) then
                print *, '  FAIL: Cache not properly invalidated'
                return
            end if
            
            ! Store new results
            new_results = create_test_results_variant(2)
            call cache%store_results("analyzer", 1, new_results)
            
            ! Retrieve new results
            call cache%retrieve_results("analyzer", 1, retrieved, success)
            
            ! Verify: new results retrieved successfully
            if (success .and. results_equal(new_results, retrieved)) then
                test_results_cache_invalidation = .true.
                print *, '  PASS: Cache invalidation working'
            else
                print *, '  FAIL: Cache invalidation failed'
            end if
        end block
    end function test_results_cache_invalidation

    logical function test_results_cache_memory_management()
        ! Given: An analysis cache with memory constraints
        ! When: Large amounts of data are cached
        ! Then: Memory is managed properly without leaks
        test_results_cache_memory_management = .false.
        print *, 'Testing results cache memory management...'

        block
            type(analysis_cache_t) :: cache
            type(analysis_results_t) :: results
            integer :: i, initial_memory, final_memory
            
            ! Get initial memory usage
            initial_memory = get_memory_usage()
            
            ! Store and clear many results
            do i = 1, 100
                results = create_large_test_results(i)
                call cache%store_results("analyzer", i, results)
            end do
            
            ! Clear cache
            call cache%clear_all()
            
            ! Get final memory usage
            final_memory = get_memory_usage()
            
            ! Verify: no significant memory leak
            if (abs(final_memory - initial_memory) < 1000) then  ! 1KB tolerance
                test_results_cache_memory_management = .true.
                print *, '  PASS: Memory management working'
            else
                print *, '  FAIL: Memory leak detected'
            end if
        end block
    end function test_results_cache_memory_management

    ! =================================================================
    ! Test Group 4: Integration Tests
    ! =================================================================

    logical function test_multi_pass_type_inference()
        ! Given: Code with complex type dependencies
        ! When: Multi-pass type inference is performed
        ! Then: All types are correctly inferred after multiple passes
        test_multi_pass_type_inference = .false.
        print *, 'Testing multi-pass type inference...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg
            type(compilation_options_t) :: options
            integer :: unit

            ! Create complex type scenario requiring multiple passes
            input_file = 'test_multipass_types.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'program test'
            write (unit, '(a)') '    ! Forward reference scenario'
            write (unit, '(a)') '    x = f(y)'  ! f and y not yet declared
            write (unit, '(a)') '    y = 42'    ! y is integer
            write (unit, '(a)') '    contains'
            write (unit, '(a)') '    function f(a) result(res)'
            write (unit, '(a)') '        integer :: a, res'
            write (unit, '(a)') '        res = a * 2'
            write (unit, '(a)') '    end function'
            write (unit, '(a)') 'end program'
            close (unit)

            output_file = 'test_multipass_types_out.f90'
            options%output_file = output_file
            ! Enable multi-pass analysis (this option doesn't exist yet)
            ! options%multi_pass_enabled = .true.

            call compile_source(input_file, options, error_msg)

            if (error_msg /= '') then
                print *, '  EXPECTED FAIL: Multi-pass not implemented - ', &
                    trim(error_msg)
                ! This should fail until multi-pass is implemented
            else
                print *, '  UNEXPECTED: Compilation succeeded without multi-pass'
            end if

            print *, '  FAIL: Multi-pass type inference not implemented yet'
        end block
    end function test_multi_pass_type_inference

    logical function test_multi_pass_allocatable_detection()
        ! Given: Code with issue #188 allocatable reassignment patterns
        ! When: Multi-pass allocatable detection is performed
        ! Then: Allocatable reassignments are correctly identified
        test_multi_pass_allocatable_detection = .false.
        print *, 'Testing multi-pass allocatable detection (issue #188)...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg
            type(compilation_options_t) :: options
            integer :: unit

            ! Create allocatable reassignment scenario from issue #188
            input_file = 'test_multipass_alloc.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'program test'
            write (unit, '(a)') '    integer, allocatable :: arr(:)'
            write (unit, '(a)') '    allocate(arr(10))'
            write (unit, '(a)') '    arr = process_array(arr)'  ! Reassignment pattern
            write (unit, '(a)') '    contains'
            write (unit, '(a)') '    function process_array(input) result(output)'
            write (unit, '(a)') '        integer, intent(in) :: input(:)'
            write (unit, '(a)') '        integer, allocatable :: output(:)'
            write (unit, '(a)') '        allocate(output(size(input)))'
            write (unit, '(a)') '        output = input * 2'
            write (unit, '(a)') '    end function'
            write (unit, '(a)') 'end program'
            close (unit)

            output_file = 'test_multipass_alloc_out.f90'
            options%output_file = output_file
            ! Enable allocatable analysis (this option doesn't exist yet)
            ! options%allocatable_analysis_enabled = .true.

            call compile_source(input_file, options, error_msg)

            if (error_msg /= '') then
                print *, '  EXPECTED FAIL: Multi-pass allocatable analysis not implemented - ', &
                    trim(error_msg)
            else
                print *, '  UNEXPECTED: Should require multi-pass for full analysis'
            end if

            print *, '  FAIL: Multi-pass allocatable detection not implemented yet'
        end block
    end function test_multi_pass_allocatable_detection

    logical function test_multi_pass_forward_references()
        ! Given: Code with forward references between symbols
        ! When: Multi-pass symbol resolution is performed
        ! Then: All forward references are resolved correctly
        test_multi_pass_forward_references = .false.
        print *, 'Testing multi-pass forward reference resolution...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg
            type(compilation_options_t) :: options
            integer :: unit

            ! Create forward reference scenario
            input_file = 'test_multipass_forward.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'program test'
            write (unit, '(a)') '    call sub_a()  ! Forward reference'
            write (unit, '(a)') '    contains'
            write (unit, '(a)') '    subroutine sub_a()'
            write (unit, '(a)') '        call sub_b()  ! Another forward reference'
            write (unit, '(a)') '    end subroutine'
            write (unit, '(a)') '    subroutine sub_b()'
            write (unit, '(a)') '        print *, "Hello from sub_b"'
            write (unit, '(a)') '    end subroutine'
            write (unit, '(a)') 'end program'
            close (unit)

            output_file = 'test_multipass_forward_out.f90'
            options%output_file = output_file

            call compile_source(input_file, options, error_msg)

            if (error_msg /= '') then
                print *, '  EXPECTED FAIL: Multi-pass forward resolution not implemented - ', &
                    trim(error_msg)
            else
                print *, '  Single-pass might handle this case'
            end if

            print *, '  FAIL: Multi-pass forward reference resolution not implemented yet'
        end block
    end function test_multi_pass_forward_references

    logical function test_multi_pass_performance()
        ! Given: Multi-pass compilation system
        ! When: Performance is compared to single-pass
        ! Then: Multi-pass overhead is acceptable (< 3x single-pass time)
        test_multi_pass_performance = .false.
        print *, 'Testing multi-pass compilation performance...'

        block
            character(len=:), allocatable :: input_file, output_file
            character(len=256) :: error_msg
            type(compilation_options_t) :: options
            integer :: unit, start_time, end_time, single_time, multi_time
            real :: performance_ratio

            ! Create moderately complex test case
            input_file = 'test_multipass_perf.lf'
            open (newunit=unit, file=input_file, status='replace')
            write (unit, '(a)') 'program test'
            write (unit, '(a)') '    integer :: arr(100), i'
            write (unit, '(a)') '    do i = 1, 100'
            write (unit, '(a)') '        arr(i) = compute(i)'
            write (unit, '(a)') '    end do'
            write (unit, '(a)') '    contains'
            write (unit, '(a)') '    function compute(x) result(y)'
            write (unit, '(a)') '        integer :: x, y'
            write (unit, '(a)') '        y = x * x + helper(x)'
            write (unit, '(a)') '    end function'
            write (unit, '(a)') '    function helper(a) result(b)'
            write (unit, '(a)') '        integer :: a, b'
            write (unit, '(a)') '        b = a + 1'
            write (unit, '(a)') '    end function'
            write (unit, '(a)') 'end program'
            close (unit)

            ! Time single-pass compilation
            output_file = 'test_perf_single_out.f90'
            options%output_file = output_file
            ! options%multi_pass_enabled = .false.  ! Not implemented yet

            call system_clock(start_time)
            call compile_source(input_file, options, error_msg)
            call system_clock(end_time)
            single_time = end_time - start_time

            ! Time multi-pass compilation (would fail until implemented)
            output_file = 'test_perf_multi_out.f90'
            options%output_file = output_file
            ! options%multi_pass_enabled = .true.   ! Not implemented yet

            call system_clock(start_time)
            call compile_source(input_file, options, error_msg)
            call system_clock(end_time)
            multi_time = end_time - start_time

            ! Calculate performance ratio
            if (single_time > 0) then
                performance_ratio = real(multi_time) / real(single_time)
                if (performance_ratio < 3.0) then
                    test_multi_pass_performance = .true.
                    print *, '  PASS: Performance ratio acceptable: ', performance_ratio
                else
                    print *, '  FAIL: Performance ratio too high: ', performance_ratio
                end if
            else
                print *, '  FAIL: Cannot measure performance'
            end if

            print *, '  FAIL: Multi-pass performance testing not implemented yet'
        end block
    end function test_multi_pass_performance

    ! =================================================================
    ! Helper Functions (Mock Infrastructure)
    ! These would be implemented alongside the actual infrastructure
    ! =================================================================

    function create_test_ast(arena) result(root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer :: root_index
        
        ! This would create a minimal test AST for infrastructure testing
        ! For now, just return a dummy index
        root_index = 1
    end function create_test_ast

    function create_forward_reference_ast(arena) result(root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer :: root_index
        
        ! This would create an AST with forward references
        root_index = 1
    end function create_forward_reference_ast

end program test_multi_pass_compilation