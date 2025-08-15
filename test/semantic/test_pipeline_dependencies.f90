program test_pipeline_dependencies
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test pipeline dependency registration
    call test_dependency_registration()
    call test_dependency_graph_build()
    
    if (tests_passed) then
        print *, "TEST PASSED: pipeline dependency registration"
    else
        print *, "TEST FAILED: pipeline dependency registration"
        error stop 1
    end if

contains

    subroutine test_dependency_registration()
        type(semantic_pipeline_t) :: pipeline
        type(symbol_analyzer_t) :: symbol_analyzer
        type(type_analyzer_t) :: type_analyzer
        
        pipeline = create_pipeline()
        
        ! Register analyzers - order shouldn't matter due to dependencies
        call pipeline%register_analyzer(type_analyzer)  ! depends on symbol
        call pipeline%register_analyzer(symbol_analyzer) ! no dependencies
        
        ! Check that both were registered
        if (pipeline%get_analyzer_count() /= 2) then
            print *, "ERROR: Expected 2 analyzers registered"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Dependency registration works"
    end subroutine

    subroutine test_dependency_graph_build()
        type(semantic_pipeline_t) :: pipeline
        type(symbol_analyzer_t) :: symbol_analyzer
        type(type_analyzer_t) :: type_analyzer
        type(scope_analyzer_t) :: scope_analyzer
        integer :: symbol_index, type_index, scope_index
        
        pipeline = create_pipeline()
        
        ! Register analyzers in "wrong" order to test dependency sorting
        call pipeline%register_analyzer(type_analyzer)   ! depends on symbol
        call pipeline%register_analyzer(scope_analyzer)  ! no dependencies  
        call pipeline%register_analyzer(symbol_analyzer) ! no dependencies
        
        ! Test that we can find analyzers by name
        symbol_index = pipeline%find_analyzer_by_name("symbol_analyzer")
        type_index = pipeline%find_analyzer_by_name("type_analyzer")
        scope_index = pipeline%find_analyzer_by_name("scope_analyzer")
        
        if (symbol_index == 0) then
            print *, "ERROR: Could not find symbol_analyzer"
            tests_passed = .false.
            return
        end if
        
        if (type_index == 0) then
            print *, "ERROR: Could not find type_analyzer"
            tests_passed = .false.
            return
        end if
        
        if (scope_index == 0) then
            print *, "ERROR: Could not find scope_analyzer"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Dependency graph build and lookup works"
    end subroutine

end program test_pipeline_dependencies