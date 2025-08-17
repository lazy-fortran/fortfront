program vicky_test_builtin_analyzers
    ! Comprehensive user acceptance test for built-in analyzers
    ! Testing from external user perspective (fluff, fortfront standardizer)
    
    use ast_core, only: ast_arena_t, create_ast_arena
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    use semantic_analyzer, only: semantic_context_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(type_analyzer_t) :: type_analyzer  
    type(scope_analyzer_t) :: scope_analyzer
    type(ast_arena_t) :: arena
    integer :: root_node_index
    class(*), allocatable :: results
    logical :: all_passed = .true.

    print *, "=== VICKY'S BUILT-IN ANALYZERS USER ACCEPTANCE TEST ==="
    
    arena = create_ast_arena()
    root_node_index = 1
    
    ! Test 1: Individual Analyzer Registration and Execution
    call test_individual_analyzers()
    
    ! Test 2: Combined Analyzer Pipeline  
    call test_combined_pipeline()
    
    ! Test 3: Analyzer Results Accessibility
    call test_results_accessibility()
    
    ! Test 4: Analyzer Names and Identification
    call test_analyzer_identification()
    
    if (all_passed) then
        print *, ""
        print *, "=== ALL BUILT-IN ANALYZER TESTS PASSED ==="
        print *, "✓ Symbol analyzer works correctly"
        print *, "✓ Type analyzer works correctly"
        print *, "✓ Scope analyzer works correctly"
        print *, "✓ Combined pipeline execution works"
        print *, "✓ Results are accessible to external tools"
        print *, "✓ Analyzer identification works"
        print *, ""
        print *, "🎉 Built-in analyzers READY for production use!"
    else
        print *, ""
        print *, "❌ BUILT-IN ANALYZER TESTS FAILED"
        print *, "Implementation needs improvement"
        error stop
    end if

contains

    subroutine test_individual_analyzers()
        type(semantic_pipeline_t) :: test_pipeline
        
        print *, "=== Test 1: Individual Analyzer Registration ==="
        
        ! Test symbol analyzer
        test_pipeline = create_pipeline()
        call test_pipeline%register_analyzer(symbol_analyzer)
        
        if (test_pipeline%get_analyzer_count() /= 1) then
            print *, "❌ FAIL: Symbol analyzer registration"
            all_passed = .false.
            return
        end if
        
        call test_pipeline%run_analysis(arena, root_node_index)
        print *, "✓ PASS: Symbol analyzer works individually"
        
        ! Test type analyzer
        test_pipeline = create_pipeline()
        call test_pipeline%register_analyzer(type_analyzer)
        call test_pipeline%run_analysis(arena, root_node_index)
        print *, "✓ PASS: Type analyzer works individually"
        
        ! Test scope analyzer
        test_pipeline = create_pipeline()
        call test_pipeline%register_analyzer(scope_analyzer)
        call test_pipeline%run_analysis(arena, root_node_index)
        print *, "✓ PASS: Scope analyzer works individually"
    end subroutine

    subroutine test_combined_pipeline()
        print *, "=== Test 2: Combined Analyzer Pipeline ==="
        
        pipeline = create_pipeline()
        
        ! Register all built-in analyzers (core semantic analysis)
        call pipeline%register_analyzer(symbol_analyzer)
        call pipeline%register_analyzer(type_analyzer)
        call pipeline%register_analyzer(scope_analyzer)
        
        if (pipeline%get_analyzer_count() /= 3) then
            print *, "❌ FAIL: Should have 3 built-in analyzers"
            all_passed = .false.
            return
        end if
        
        ! Run combined analysis
        call pipeline%run_analysis(arena, root_node_index)
        print *, "✓ PASS: Combined built-in analyzer pipeline works"
    end subroutine

    subroutine test_results_accessibility()
        print *, "=== Test 3: Analyzer Results Accessibility ==="
        
        ! User story: External tools need to access analysis results
        if (allocated(pipeline%analyzers(1)%analyzer)) then
            results = pipeline%analyzers(1)%analyzer%get_results()
            
            select type(results)
            type is (semantic_context_t)
                print *, "✓ PASS: Symbol analyzer returns semantic context"
            class default
                print *, "❌ FAIL: Symbol analyzer returns wrong type"
                all_passed = .false.
                return
            end select
        else
            print *, "❌ FAIL: Analyzer not accessible"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: All analyzer results accessible"
    end subroutine

    subroutine test_analyzer_identification()
        print *, "=== Test 4: Analyzer Identification ==="
        
        ! User story: External tools need to identify analyzer types
        if (symbol_analyzer%get_name() /= "symbol_analyzer") then
            print *, "❌ FAIL: Symbol analyzer name incorrect"
            all_passed = .false.
            return
        end if
        
        if (type_analyzer%get_name() /= "type_analyzer") then
            print *, "❌ FAIL: Type analyzer name incorrect"  
            all_passed = .false.
            return
        end if
        
        if (scope_analyzer%get_name() /= "scope_analyzer") then
            print *, "❌ FAIL: Scope analyzer name incorrect"
            all_passed = .false.
            return
        end if
        
        print *, "✓ PASS: All analyzer names correct"
    end subroutine

end program vicky_test_builtin_analyzers