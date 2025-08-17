program vicky_test_error_handling
    ! Test error handling and edge cases for semantic pipeline
    ! Critical for production readiness and external tool robustness
    
    use ast_core, only: ast_arena_t, create_ast_arena
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(type_analyzer_t) :: type_analyzer
    type(ast_arena_t) :: arena, empty_arena
    integer :: valid_node_index, invalid_node_index
    logical :: all_passed = .true.

    print *, "=== VICKY'S ERROR HANDLING & EDGE CASES TEST ==="
    print *, "Testing robustness for production use"
    
    ! Setup
    arena = create_ast_arena()
    empty_arena = create_ast_arena()
    valid_node_index = 1
    invalid_node_index = -1
    
    ! Test 1: Empty pipeline execution
    call test_empty_pipeline_execution()
    
    ! Test 2: Invalid node indices
    call test_invalid_node_indices()
    
    ! Test 3: Empty AST arena
    call test_empty_ast_arena()
    
    ! Test 4: Multiple registrations of same analyzer
    call test_multiple_registrations()
    
    ! Test 5: Pipeline reuse
    call test_pipeline_reuse()
    
    ! Test 6: Memory safety
    call test_memory_safety()

    if (all_passed) then
        print *, ""
        print *, "=== ALL ERROR HANDLING TESTS PASSED ==="
        print *, "✓ Empty pipeline execution is safe"
        print *, "✓ Invalid inputs handled gracefully"
        print *, "✓ Edge cases don't crash the system"
        print *, "✓ Multiple registrations work correctly"
        print *, "✓ Pipeline reuse is safe"
        print *, "✓ Memory management is robust"
        print *, ""
        print *, "🎉 Semantic pipeline ROBUST for production use!"
    else
        print *, ""
        print *, "❌ ERROR HANDLING TESTS FAILED"
        print *, "Pipeline not robust enough for production"
        error stop
    end if

contains

    subroutine test_empty_pipeline_execution()
        type(semantic_pipeline_t) :: empty_pipeline
        
        print *, "=== Test 1: Empty Pipeline Execution ==="
        
        empty_pipeline = create_pipeline()
        
        if (empty_pipeline%get_analyzer_count() /= 0) then
            print *, "❌ FAIL: Empty pipeline should have 0 analyzers"
            all_passed = .false.
            return
        end if
        
        ! This should not crash - empty pipeline execution
        call empty_pipeline%run_analysis(arena, valid_node_index)
        
        print *, "✓ PASS: Empty pipeline execution is safe"
    end subroutine

    subroutine test_invalid_node_indices()
        print *, "=== Test 2: Invalid Node Indices ==="
        
        pipeline = create_pipeline()
        call pipeline%register_analyzer(symbol_analyzer)
        
        ! Test with invalid node index - should not crash
        call pipeline%run_analysis(arena, invalid_node_index)
        
        ! Test with zero node index - should not crash
        call pipeline%run_analysis(arena, 0)
        
        ! Test with very large node index - should not crash
        call pipeline%run_analysis(arena, 999999)
        
        print *, "✓ PASS: Invalid node indices handled gracefully"
    end subroutine

    subroutine test_empty_ast_arena()
        print *, "=== Test 3: Empty AST Arena ==="
        
        pipeline = create_pipeline()
        call pipeline%register_analyzer(symbol_analyzer)
        
        ! Analysis on empty arena should not crash
        call pipeline%run_analysis(empty_arena, valid_node_index)
        
        print *, "✓ PASS: Empty AST arena handled gracefully"
    end subroutine

    subroutine test_multiple_registrations()
        type(semantic_pipeline_t) :: multi_pipeline
        type(symbol_analyzer_t) :: analyzer1, analyzer2
        
        print *, "=== Test 4: Multiple Analyzer Registrations ==="
        
        multi_pipeline = create_pipeline()
        
        ! Register same type multiple times
        call multi_pipeline%register_analyzer(analyzer1)
        call multi_pipeline%register_analyzer(analyzer2)
        call multi_pipeline%register_analyzer(symbol_analyzer)
        
        if (multi_pipeline%get_analyzer_count() /= 3) then
            print *, "❌ FAIL: Should have 3 analyzers after multiple registrations"
            all_passed = .false.
            return
        end if
        
        ! Should execute all instances
        call multi_pipeline%run_analysis(arena, valid_node_index)
        
        print *, "✓ PASS: Multiple registrations work correctly"
    end subroutine

    subroutine test_pipeline_reuse()
        print *, "=== Test 5: Pipeline Reuse ==="
        
        pipeline = create_pipeline()
        call pipeline%register_analyzer(symbol_analyzer)
        call pipeline%register_analyzer(type_analyzer)
        
        ! Run analysis multiple times on same pipeline
        call pipeline%run_analysis(arena, valid_node_index)
        call pipeline%run_analysis(arena, valid_node_index)
        call pipeline%run_analysis(arena, valid_node_index)
        
        ! Pipeline should still be functional
        if (pipeline%get_analyzer_count() /= 2) then
            print *, "❌ FAIL: Pipeline corrupted after reuse"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: Pipeline reuse is safe"
    end subroutine

    subroutine test_memory_safety()
        print *, "=== Test 6: Memory Safety ==="
        
        ! Test multiple pipeline creation/destruction cycles
        block
            type(semantic_pipeline_t) :: temp_pipeline
            integer :: i
            
            do i = 1, 10
                temp_pipeline = create_pipeline()
                call temp_pipeline%register_analyzer(symbol_analyzer)
                call temp_pipeline%run_analysis(arena, valid_node_index)
                ! Let Fortran handle cleanup automatically
            end do
        end block
        
        ! Test analyzer access after multiple operations
        pipeline = create_pipeline()
        call pipeline%register_analyzer(symbol_analyzer)
        call pipeline%run_analysis(arena, valid_node_index)
        
        if (.not. allocated(pipeline%analyzers(1)%analyzer)) then
            print *, "❌ FAIL: Analyzer not accessible after operations"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: Memory management is robust"
    end subroutine

end program vicky_test_error_handling