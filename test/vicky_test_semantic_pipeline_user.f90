program vicky_test_semantic_pipeline_user
    ! User acceptance test for semantic pipeline - CRITICAL ISSUE INVESTIGATION
    ! Testing exactly as the working tests do
    
    use ast_core, only: ast_arena_t, create_ast_arena
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(ast_arena_t) :: arena
    integer :: root_node_index

    print *, "=== VICKY'S USER ACCEPTANCE TEST (MINIMAL) ==="
    
    ! Following exact pattern from test_semantic_pipeline.f90
    arena = create_ast_arena()
    root_node_index = 1
    
    pipeline = create_pipeline()
    
    if (pipeline%get_analyzer_count() /= 0) then
        print *, "FAIL: Pipeline should start with 0 analyzers"
        error stop
    end if
    print *, "PASS: Pipeline starts with 0 analyzers"
    
    call pipeline%register_analyzer(symbol_analyzer)
    
    if (pipeline%get_analyzer_count() /= 1) then
        print *, "FAIL: Pipeline should have 1 analyzer after registration"
        error stop
    end if
    print *, "PASS: Analyzer registration works"
    
    call pipeline%run_analysis(arena, root_node_index)
    print *, "PASS: Analysis completed"
    
    print *, ""
    print *, "=== USER ACCEPTANCE TEST PASSED ==="
    print *, "✓ Basic semantic pipeline API works from user perspective"

end program vicky_test_semantic_pipeline_user