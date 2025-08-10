program test_builtin_analyzers
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

    print *, "=== Built-in Analyzers Test ==="

    ! Initialize test environment
    arena = create_ast_arena()
    root_node_index = 1  ! Dummy root node index

    ! Create pipeline
    pipeline = create_pipeline()

    ! Test 1: Register built-in analyzers
    call pipeline%register_analyzer(symbol_analyzer)
    call pipeline%register_analyzer(type_analyzer)
    call pipeline%register_analyzer(scope_analyzer)
    
    if (pipeline%get_analyzer_count() /= 3) then
        print *, "FAIL: Pipeline should have 3 built-in analyzers"
        error stop
    end if
    print *, "PASS: Built-in analyzers registered successfully"

    ! Test 2: Run analysis pipeline
    call pipeline%run_analysis(arena, root_node_index)
    print *, "PASS: Built-in analyzer pipeline executed"

    ! Test 3: Verify analyzer names
    if (symbol_analyzer%get_name() /= "symbol_analyzer") then
        print *, "FAIL: Symbol analyzer name incorrect"
        error stop
    end if
    
    if (type_analyzer%get_name() /= "type_analyzer") then
        print *, "FAIL: Type analyzer name incorrect"
        error stop
    end if
    
    if (scope_analyzer%get_name() /= "scope_analyzer") then
        print *, "FAIL: Scope analyzer name incorrect"
        error stop
    end if
    print *, "PASS: All analyzer names correct"

    ! Test 4: Check analyzer results
    results = symbol_analyzer%get_results()
    select type(results)
    type is (semantic_context_t)
        print *, "PASS: Symbol analyzer returns semantic context"
    class default
        print *, "FAIL: Symbol analyzer results incorrect type"
        error stop
    end select

    results = type_analyzer%get_results()
    select type(results) 
    type is (semantic_context_t)
        print *, "PASS: Type analyzer returns semantic context"
    class default
        print *, "FAIL: Type analyzer results incorrect type"
        error stop
    end select

    results = scope_analyzer%get_results()
    select type(results)
    type is (semantic_context_t) 
        print *, "PASS: Scope analyzer returns semantic context"
    class default
        print *, "FAIL: Scope analyzer results incorrect type"
        error stop
    end select

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Built-in analyzers working correctly!"

end program test_builtin_analyzers