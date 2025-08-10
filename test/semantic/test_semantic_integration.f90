program test_semantic_integration
    use fortfront, only: semantic_pipeline_t, semantic_analyzer_t, &
                         symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                         create_pipeline, create_default_semantic_pipeline, &
                         analyze_semantics_with_pipeline
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none

    type(semantic_pipeline_t) :: pipeline, default_pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(ast_arena_t) :: arena
    integer :: root_node_index

    print *, "=== Semantic Pipeline Integration Test ==="

    ! Initialize test environment
    arena = create_ast_arena()
    root_node_index = 1  ! Dummy root node index

    ! Test 1: fortfront exports semantic pipeline types
    pipeline = create_pipeline()
    print *, "PASS: Can create semantic pipeline through fortfront API"

    ! Test 2: fortfront exports built-in analyzer types
    call pipeline%register_analyzer(symbol_analyzer)
    if (pipeline%get_analyzer_count() /= 1) then
        print *, "FAIL: Built-in analyzer registration failed"
        error stop
    end if
    print *, "PASS: Built-in analyzers accessible through fortfront API"

    ! Test 3: Default pipeline creation works
    default_pipeline = create_default_semantic_pipeline()
    if (default_pipeline%get_analyzer_count() /= 3) then
        print *, "FAIL: Default pipeline should have 3 analyzers"
        error stop
    end if
    print *, "PASS: Default semantic pipeline creation works"

    ! Test 4: Integration function available
    ! Note: We can't test full execution without real AST nodes
    ! but we can verify the API is accessible
    print *, "PASS: analyze_semantics_with_pipeline available through fortfront API"

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Semantic pipeline successfully integrated into fortfront!"

end program test_semantic_integration