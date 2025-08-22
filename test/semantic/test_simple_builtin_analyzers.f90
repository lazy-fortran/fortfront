program test_simple_builtin_analyzers
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(type_analyzer_t) :: type_analyzer  
    type(scope_analyzer_t) :: scope_analyzer

    print *, "=== Simple Built-in Analyzers Test ==="

    ! Create pipeline
    pipeline = create_pipeline()

    ! Test 1: Register built-in analyzers
    call pipeline%register_analyzer(symbol_analyzer)
    call pipeline%register_analyzer(type_analyzer)
    call pipeline%register_analyzer(scope_analyzer)
    
    if (pipeline%get_analyzer_count() /= 3) then
        print *, "FAIL: Pipeline should have 3 built-in analyzers"
        stop 1
    end if
    print *, "PASS: Built-in analyzers registered successfully"

    ! Test 2: Verify analyzer names
    if (symbol_analyzer%get_name() /= "symbol_analyzer") then
        print *, "FAIL: Symbol analyzer name incorrect"
        stop 1
    end if
    
    if (type_analyzer%get_name() /= "type_analyzer") then
        print *, "FAIL: Type analyzer name incorrect"
        stop 1
    end if
    
    if (scope_analyzer%get_name() /= "scope_analyzer") then
        print *, "FAIL: Scope analyzer name incorrect"
        stop 1
    end if
    print *, "PASS: All analyzer names correct"

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Built-in analyzer registration working!"

end program test_simple_builtin_analyzers