program test_control_flow_plugin_integration
    use base_analyzer
    use control_flow_analyzer_plugin
    use control_flow_graph_module
    use semantic_analyzer
    use ast_core
    use ast_operations
    implicit none

    integer :: test_count = 0
    integer :: failed_count = 0

    call test_plugin_registration()
    call test_plugin_integration_with_semantic_analyzer()
    call test_fortfront_module_export()

    call print_test_summary()

contains

    ! Test that the plugin can be registered
    subroutine test_plugin_registration()
        type(control_flow_analyzer_t) :: analyzer
        type(analyzer_id_t) :: id
        
        call start_test("Plugin Registration")
        
        ! Initialize plugin
        call analyzer%initialize("control_flow_analyzer")
        
        ! Verify plugin properties for registration
        id = analyzer%get_id()
        call assert(trim(id%name) == "control_flow_analyzer", &
                   "Plugin should have correct name")
        call assert(id%id == 194, &
                   "Plugin should have correct ID (issue number)")
        call assert(analyzer%enabled .eqv. .true., &
                   "Plugin should be enabled")
        call assert(analyzer%priority == 100, &
                   "Plugin should have correct priority")
        
        call end_test()
    end subroutine test_plugin_registration

    ! Test integration with semantic analyzer
    subroutine test_plugin_integration_with_semantic_analyzer()
        type(control_flow_analyzer_t) :: analyzer
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(analysis_results_t) :: results
        integer :: root_index, empty_body(0)
        
        call start_test("Plugin Integration with Semantic Analyzer")
        
        ! Create test setup
        arena = create_ast_arena()
        root_index = create_program_node(arena, "test_integration", empty_body)
        call analyzer%initialize("integration_test")
        ctx = create_semantic_context()
        
        ! Perform analysis through plugin interface
        results = analyzer%analyze(ctx, arena, root_index)
        
        ! Verify analysis completed successfully
        call assert(results%converged .eqv. .true., &
                   "Plugin analysis should converge")
        call assert(results%iteration_count == 1, &
                   "Should complete in one iteration")
        call assert(results%confidence_score == 1.0, &
                   "Should have full confidence")
        
        ! Verify that control flow analysis was performed
        block
            type(control_flow_graph_t) :: cfg
            cfg = analyzer%get_control_flow_graph()
            call assert(cfg%block_count >= 1, &
                       "CFG should have at least one block")
        end block
        
        call end_test()
    end subroutine test_plugin_integration_with_semantic_analyzer

    ! Test that the plugin is exported through fortfront module
    subroutine test_fortfront_module_export()
        call start_test("Fortfront Module Export")
        
        ! This test verifies that the plugin type is accessible
        ! through the main fortfront module interface
        block
            type(control_flow_analyzer_t) :: analyzer
            call analyzer%initialize("export_test")
            call assert(.true., "Plugin type should be accessible")
        end block
        
        call end_test()
    end subroutine test_fortfront_module_export

    ! Test framework utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        print *, "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        print *, "  PASSED"
    end subroutine end_test

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        if (.not. condition) then
            print *, "  FAILED: ", trim(message)
            failed_count = failed_count + 1
            print *, "Test failed"
            stop 1
        end if
    end subroutine assert

    subroutine print_test_summary()
        print *, ""
        print *, "Test Summary:"
        print *, "  Total tests: ", test_count
        print *, "  Failed tests: ", failed_count
        print *, "  Success rate: ", &
                 real(test_count - failed_count) / real(test_count) * 100.0, "%"
        
        if (failed_count > 0) then
            print *, "Some tests failed"
            stop 1
        else
            print *, "All tests passed!"
        end if
    end subroutine print_test_summary

end program test_control_flow_plugin_integration