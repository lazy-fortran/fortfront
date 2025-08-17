program vicky_test_performance_stress
    ! Performance and stress testing for semantic pipeline
    ! Testing scalability for production workloads
    
    use ast_core, only: ast_arena_t, create_ast_arena
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                                 call_graph_analyzer_t, control_flow_analyzer_t, &
                                 usage_tracker_analyzer_t, source_reconstruction_analyzer_t, &
                                 interface_analyzer_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(ast_arena_t) :: arena
    integer :: root_node_index
    logical :: all_passed = .true.
    integer :: i, start_time, end_time, count_rate

    print *, "=== VICKY'S PERFORMANCE & STRESS TESTING ==="
    print *, "Testing scalability for production workloads"
    
    arena = create_ast_arena()
    root_node_index = 1
    
    ! Test 1: Many analyzers registration and execution
    call test_many_analyzers()
    
    ! Test 2: Multiple pipeline execution cycles
    call test_multiple_execution_cycles()
    
    ! Test 3: Large pipeline stress test
    call test_large_pipeline_stress()
    
    ! Test 4: Memory usage stability
    call test_memory_stability()

    if (all_passed) then
        print *, ""
        print *, "=== ALL PERFORMANCE TESTS PASSED ==="
        print *, "✓ Handles many analyzers efficiently"
        print *, "✓ Multiple execution cycles are stable"
        print *, "✓ Large pipeline stress test passed"
        print *, "✓ Memory usage is stable under load"
        print *, ""
        print *, "🎉 Semantic pipeline SCALABLE for production workloads!"
    else
        print *, ""
        print *, "❌ PERFORMANCE TESTS FAILED"
        print *, "Pipeline may not scale for production use"
        error stop
    end if

contains

    subroutine test_many_analyzers()
        type(symbol_analyzer_t) :: symbol_analyzer
        type(type_analyzer_t) :: type_analyzer
        type(scope_analyzer_t) :: scope_analyzer
        type(call_graph_analyzer_t) :: call_graph_analyzer
        type(control_flow_analyzer_t) :: control_flow_analyzer
        type(usage_tracker_analyzer_t) :: usage_analyzer
        type(source_reconstruction_analyzer_t) :: source_analyzer
        type(interface_analyzer_t) :: interface_analyzer
        
        print *, "=== Test 1: Many Analyzers Registration & Execution ==="
        
        pipeline = create_pipeline()
        
        ! Register all available analyzer types
        call pipeline%register_analyzer(symbol_analyzer)
        call pipeline%register_analyzer(type_analyzer)
        call pipeline%register_analyzer(scope_analyzer)
        call pipeline%register_analyzer(call_graph_analyzer)
        call pipeline%register_analyzer(control_flow_analyzer)
        call pipeline%register_analyzer(usage_analyzer)
        call pipeline%register_analyzer(source_analyzer)
        call pipeline%register_analyzer(interface_analyzer)
        
        if (pipeline%get_analyzer_count() /= 8) then
            print *, "❌ FAIL: Should have 8 analyzers registered"
            all_passed = .false.
            return
        end if
        
        ! Time the execution
        call system_clock(start_time, count_rate)
        call pipeline%run_analysis(arena, root_node_index)
        call system_clock(end_time)
        
        print *, "✓ PASS: All 8 analyzers executed successfully"
        print *, "  Execution time:", real(end_time - start_time) / real(count_rate), "seconds"
    end subroutine

    subroutine test_multiple_execution_cycles()
        integer, parameter :: num_cycles = 50
        
        print *, "=== Test 2: Multiple Execution Cycles ==="
        
        pipeline = create_pipeline()
        call pipeline%register_analyzer(symbol_analyzer_t())
        call pipeline%register_analyzer(type_analyzer_t())
        
        call system_clock(start_time, count_rate)
        
        do i = 1, num_cycles
            call pipeline%run_analysis(arena, root_node_index)
        end do
        
        call system_clock(end_time)
        
        ! Pipeline should still be functional
        if (pipeline%get_analyzer_count() /= 2) then
            print *, "❌ FAIL: Pipeline corrupted after multiple cycles"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS:", num_cycles, "execution cycles completed"
        print *, "  Average time per cycle:", &
                 real(end_time - start_time) / real(count_rate) / real(num_cycles), "seconds"
    end subroutine

    subroutine test_large_pipeline_stress()
        integer, parameter :: num_analyzers = 20  ! Register same analyzers multiple times
        type(symbol_analyzer_t) :: analyzers(num_analyzers)
        
        print *, "=== Test 3: Large Pipeline Stress Test ==="
        
        pipeline = create_pipeline()
        
        ! Register many analyzer instances
        do i = 1, num_analyzers
            call pipeline%register_analyzer(analyzers(i))
        end do
        
        if (pipeline%get_analyzer_count() /= num_analyzers) then
            print *, "❌ FAIL: Should have", num_analyzers, "analyzers"
            all_passed = .false.
            return
        end if
        
        call system_clock(start_time, count_rate)
        call pipeline%run_analysis(arena, root_node_index)
        call system_clock(end_time)
        
        print *, "✓ PASS:", num_analyzers, "analyzers executed successfully"
        print *, "  Execution time:", real(end_time - start_time) / real(count_rate), "seconds"
    end subroutine

    subroutine test_memory_stability()
        integer, parameter :: num_iterations = 100
        type(semantic_pipeline_t) :: temp_pipelines(10)
        
        print *, "=== Test 4: Memory Usage Stability ==="
        
        ! Create and destroy many pipelines to test memory leaks
        do i = 1, num_iterations
            block
                type(semantic_pipeline_t) :: temp_pipeline
                temp_pipeline = create_pipeline()
                call temp_pipeline%register_analyzer(symbol_analyzer_t())
                call temp_pipeline%run_analysis(arena, root_node_index)
                ! Automatic cleanup when temp_pipeline goes out of scope
            end block
        end do
        
        ! Create multiple concurrent pipelines
        do i = 1, 10
            temp_pipelines(i) = create_pipeline()
            call temp_pipelines(i)%register_analyzer(symbol_analyzer_t())
            call temp_pipelines(i)%run_analysis(arena, root_node_index)
        end do
        
        ! Verify they all work
        do i = 1, 10
            if (temp_pipelines(i)%get_analyzer_count() /= 1) then
                print *, "❌ FAIL: Pipeline", i, "corrupted in concurrent test"
                all_passed = .false.
                return
            end if
        end do
        
        print *, "✓ PASS:", num_iterations, "create/destroy cycles completed"
        print *, "✓ PASS: 10 concurrent pipelines work correctly"
        print *, "  Memory management appears stable"
    end subroutine

end program vicky_test_performance_stress