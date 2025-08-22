program test_semantic_pipeline
    use ast_core, only: ast_arena_t, create_ast_arena
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline, &
                                 destroy_pipeline
    use semantic_analyzer_base, only: semantic_analyzer_t
    use test_analyzer, only: simple_test_analyzer_t
    implicit none

    type(semantic_pipeline_t) :: pipeline
    type(simple_test_analyzer_t) :: test_analyzer
    type(ast_arena_t) :: arena
    integer :: root_node_index
    logical :: test_passed
    class(*), allocatable :: results

    print *, "=== Semantic Pipeline Basic Test ==="

    ! Initialize test environment
    arena = create_ast_arena()
    root_node_index = 1  ! Dummy root node index

    ! Create pipeline
    pipeline = create_pipeline()

    ! Test 1: Pipeline starts with zero analyzers
    if (pipeline%get_analyzer_count() /= 0) then
        print *, "FAIL: Pipeline should start with 0 analyzers"
        stop 1
    end if
    print *, "PASS: Pipeline starts with 0 analyzers"

    ! Test 2: Register an analyzer
    call pipeline%register_analyzer(test_analyzer)
    
    if (pipeline%get_analyzer_count() /= 1) then
        print *, "FAIL: Pipeline should have 1 analyzer after registration"
        stop 1
    end if
    print *, "PASS: Analyzer registration works"

    ! Test 3: Run analysis
    print *, "DEBUG: About to run analysis"
    call pipeline%run_analysis(arena, root_node_index)
    print *, "DEBUG: Analysis completed"

    ! Test 4: Verify analyzer was executed
    ! Check the analyzer directly from the pipeline (no copy)
    print *, "DEBUG: About to check analyzer execution"
    
    if (allocated(pipeline%analyzers(1)%analyzer)) then
        select type(a => pipeline%analyzers(1)%analyzer)
        type is (simple_test_analyzer_t)
            if (.not. a%was_executed()) then
                print *, "FAIL: Test analyzer was not executed"
                stop 1
            end if
            print *, "PASS: Analyzer execution works"
            
            ! Test 5: Check analyzer results
            results = a%get_results()
        class default
            print *, "FAIL: Wrong analyzer type"
            stop 1
        end select
    else
        print *, "FAIL: Analyzer not allocated"
        stop 1
    end if
    
    select type(results)
    type is (logical)
        test_passed = results
    class default
        test_passed = .false.
    end select

    if (.not. test_passed) then
        print *, "FAIL: Analyzer results incorrect"
        stop 1
    end if
    print *, "PASS: Analyzer results accessible"

    ! Test 6: Verify analyzer name
    if (test_analyzer%get_name() /= "simple_test_analyzer") then
        print *, "FAIL: Analyzer name incorrect"
        stop 1
    end if
    print *, "PASS: Analyzer name correct"

    ! Cleanup - let Fortran handle finalization automatically
    print *, "DEBUG: Tests completed, entering cleanup phase"

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Basic semantic pipeline infrastructure is working!"
    print *, "DEBUG: About to exit program"

end program test_semantic_pipeline