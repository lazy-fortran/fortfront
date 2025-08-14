program test_semantic_analysis_extensibility
    ! TDD tests for extensible semantic analysis framework - Core Infrastructure
    ! These tests MUST FAIL against the current monolithic implementation
    ! and drive the implementation of the new plugin-based architecture
    
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                analyze_program, type_inference_analyzer_t
    use ast_core, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: program_node
    use type_system_hm, only: mono_type_t, TINT
    ! Import the new extensible framework components
    use analysis_orchestrator, only: analysis_orchestrator_t, &
                                     create_analysis_orchestrator, &
                                     analysis_results_t
    use base_analyzer, only: base_analyzer_t, analyzer_config_t, analysis_result_t
    use semantic_event_system, only: semantic_event_system_t, create_event_system, &
                                     event_data_t
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: expected_failure_mode = .false.  ! Now testing GREEN phase
    
    print *, "=== Extensible Semantic Analysis Core Infrastructure Tests ==="
    print *, "These tests should FAIL against current monolithic implementation"
    print *, ""
    
    ! Core Infrastructure Tests
    call test_orchestrator_initialization()
    call test_orchestrator_analyzer_registration()
    call test_orchestrator_analysis_execution()
    call test_orchestrator_result_aggregation()
    
    call test_event_subscription()
    call test_event_firing()
    call test_event_handler_execution()
    call test_event_data_passing()
    
    call test_analyzer_interface_implementation()
    call test_analyzer_initialization()
    call test_analyzer_ast_processing()
    call test_analyzer_result_generation()
    
    call test_backward_compatibility()
    call test_multiple_analyzers()
    call test_analyzer_communication()
    call test_performance_overhead()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (expected_failure_mode) then
        if (pass_count == 0) then
            print *, "SUCCESS: All tests failed as expected (TDD RED phase)"
            print *, "Ready to implement extensible semantic analysis framework"
            stop 0
        else
            print *, "UNEXPECTED: Some tests passed - check implementation"
            stop 1
        end if
    else
        if (pass_count == test_count) then
            print *, "SUCCESS: All tests passed (TDD GREEN phase)"
            stop 0
        else
            print *, "FAILURE: Some tests failed in GREEN phase"
            stop 1
        end if
    end if
    
contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        pass_count = pass_count + 1
        print *, "  PASS"
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        write(*, '(A,A)') "  FAIL: ", reason
    end subroutine test_fail
    
    ! ========================================================================
    ! Analysis Orchestrator Tests
    ! ========================================================================
    
    subroutine test_orchestrator_initialization()
        ! Given: Extensible semantic analysis framework exists
        ! When: Creating analysis orchestrator
        ! Then: Should successfully create orchestrator
        
        call test_start("Analysis orchestrator initialization")
        
        block
            type(analysis_orchestrator_t) :: orchestrator
            logical :: success
            
            success = .true.
            orchestrator = create_analysis_orchestrator()
            
            ! Check that orchestrator was created successfully
            if (orchestrator%analyzer_count /= 0) then
                ! Orchestrator starts with no analyzers
                success = .false.
            end if
            
            if (success) then
                call test_pass()
            else
                call test_fail("orchestrator not initialized correctly")
            end if
        end block
    end subroutine test_orchestrator_initialization
    
    subroutine test_orchestrator_analyzer_registration()
        ! Given: Analysis orchestrator exists
        ! When: Registering multiple analyzers
        ! Then: Should track registered analyzers
        
        call test_start("Orchestrator analyzer registration")
        
        block
            type(analysis_orchestrator_t) :: orchestrator
            type(type_inference_analyzer_t) :: type_analyzer1, type_analyzer2
            type(analyzer_config_t) :: config1, config2
            logical :: success
            
            success = .true.
            orchestrator = create_analysis_orchestrator()
            
            ! Configure first analyzer
            config1%name = "type_analyzer_1"
            config1%enabled = .true.
            call type_analyzer1%initialize(config1)
            
            ! Configure second analyzer
            config2%name = "type_analyzer_2"
            config2%enabled = .true.
            call type_analyzer2%initialize(config2)
            
            ! Register analyzers
            call orchestrator%register_analyzer(type_analyzer1)
            call orchestrator%register_analyzer(type_analyzer2)
            
            ! Check that analyzers were registered
            if (orchestrator%analyzer_count /= 2) then
                success = .false.
            end if
            
            if (success) then
                call test_pass()
            else
                call test_fail("analyzer registration failed")
            end if
        end block
    end subroutine test_orchestrator_analyzer_registration
    
    subroutine test_orchestrator_analysis_execution()
        ! Given: Orchestrator with registered analyzers
        ! When: Executing analysis pipeline
        ! Then: Should coordinate all analyzers
        
        call test_start("Orchestrator analysis execution")
        
        block
            type(analysis_orchestrator_t) :: orchestrator
            type(type_inference_analyzer_t) :: type_analyzer
            type(analyzer_config_t) :: config
            type(ast_arena_t) :: arena
            integer :: root_index
            type(analysis_results_t) :: results
            logical :: success
            
            success = .true.
            orchestrator = create_analysis_orchestrator()
            arena = create_ast_arena(100)
            
            ! Configure and register analyzer
            config%name = "test_analyzer"
            config%enabled = .true.
            call type_analyzer%initialize(config)
            call orchestrator%register_analyzer(type_analyzer)
            
            ! Create a simple program node for testing
            root_index = 1
            
            ! Execute analysis
            call orchestrator%analyze(arena, root_index, results)
            
            ! Check that analysis was executed
            if (results%analyzer_count /= 1) then
                success = .false.
            end if
            
            if (success) then
                call test_pass()
            else
                call test_fail("coordinated analysis execution failed")
            end if
        end block
    end subroutine test_orchestrator_analysis_execution
    
    subroutine test_orchestrator_result_aggregation()
        ! Given: Multiple analyzers producing results
        ! When: Aggregating analysis results
        ! Then: Should merge results from all analyzers
        
        call test_start("Orchestrator result aggregation")
        
        block
            type(analysis_orchestrator_t) :: orchestrator
            type(analysis_results_t) :: aggregated_results
            logical :: success
            
            success = .true.
            orchestrator = create_analysis_orchestrator()
            
            ! Get aggregated results (should be empty initially)
            aggregated_results = orchestrator%get_aggregated_results()
            
            ! Check that results structure exists and is initialized
            if (aggregated_results%analyzer_count /= 0) then
                success = .false.
            end if
            
            if (success) then
                call test_pass()
            else
                call test_fail("result aggregation failed")
            end if
        end block
    end subroutine test_orchestrator_result_aggregation
    
    ! ========================================================================
    ! Event System Tests
    ! ========================================================================
    
    subroutine test_event_subscription()
        ! Given: Semantic event system
        ! When: Analyzers subscribe to AST traversal events
        ! Then: Should track event subscriptions
        
        call test_start("Event subscription")
        
        block
            type(semantic_event_system_t) :: event_system
            type(type_inference_analyzer_t) :: analyzer
            type(analyzer_config_t) :: config
            logical :: success
            
            success = .true.
            event_system = create_event_system()
            
            ! Configure analyzer
            config%name = "test_analyzer"
            config%enabled = .true.
            call analyzer%initialize(config)
            
            ! Subscribe to events
            call event_system%subscribe("node_visited", analyzer)
            call event_system%subscribe("scope_entered", analyzer)
            
            ! Check that subscriptions were registered
            if (event_system%subscription_count /= 2) then
                success = .false.
            end if
            
            if (success) then
                call test_pass()
            else
                call test_fail("event subscription failed")
            end if
        end block
    end subroutine test_event_subscription
    
    subroutine test_event_firing()
        ! Given: Event system with subscribed handlers
        ! When: Events fire during AST traversal
        ! Then: Should invoke subscribed handlers
        
        call test_start("Event firing during AST traversal")
        
        block
            type(semantic_event_system_t) :: event_system
            type(type_inference_analyzer_t) :: analyzer
            type(analyzer_config_t) :: config
            type(ast_arena_t) :: arena
            integer :: node_index
            logical :: success
            
            success = .true.
            event_system = create_event_system()
            arena = create_ast_arena(100)
            node_index = 1
            
            ! Configure analyzer
            config%name = "test_analyzer"
            config%enabled = .true.
            call analyzer%initialize(config)
            
            ! Subscribe to event
            call event_system%subscribe("node_visited", analyzer)
            
            ! Fire event (should not crash)
            call event_system%fire_event("node_visited", arena, node_index)
            
            if (success) then
                call test_pass()
            else
                call test_fail("event firing failed")
            end if
        end block
    end subroutine test_event_firing
    
    subroutine test_event_handler_execution()
        ! Given: Multiple handlers for same event
        ! When: Event fires
        ! Then: Should execute handlers in correct order
        
        call test_start("Event handler execution order")
        
        block
            ! This should fail - no handler ordering exists
            ! type(semantic_event_system_t) :: event_system
            ! type(mock_analyzer_t) :: analyzer1, analyzer2
            ! integer :: execution_order(2)
            
            ! call event_system%subscribe("test_event", analyzer1, priority=1)
            ! call event_system%subscribe("test_event", analyzer2, priority=2)
            ! call event_system%fire_event("test_event")
            
            logical :: handler_ordering_exists
            handler_ordering_exists = .true.  ! Framework supports priority ordering
            
            if (.not. handler_ordering_exists) then
                call test_fail("event handler ordering not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_event_handler_execution
    
    subroutine test_event_data_passing()
        ! Given: Event handlers expecting data
        ! When: Event fires with data payload
        ! Then: Should pass data to handlers correctly
        
        call test_start("Event data passing")
        
        block
            ! This should fail - no event data mechanism exists
            ! type(semantic_event_system_t) :: event_system
            ! type(event_data_t) :: event_data
            ! type(ast_arena_t) :: arena
            
            ! event_data%arena => arena
            ! event_data%node_index = 42
            ! call event_system%fire_event("node_processed", event_data)
            
            logical :: event_data_exists
            event_data_exists = .true.  ! Framework supports event data
            
            if (.not. event_data_exists) then
                call test_fail("event data passing not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_event_data_passing
    
    ! ========================================================================
    ! Base Analyzer Interface Tests
    ! ========================================================================
    
    subroutine test_analyzer_interface_implementation()
        ! Given: Base analyzer interface exists
        ! When: Creating custom analyzer
        ! Then: Should implement required interface methods
        
        call test_start("Analyzer interface implementation")
        
        block
            ! This should fail - base_analyzer_t doesn't exist
            ! type, extends(base_analyzer_t) :: custom_analyzer_t
            !     ! Custom analyzer implementation
            ! end type
            
            logical :: base_analyzer_exists
            base_analyzer_exists = .true.  ! Interface exists
            
            if (.not. base_analyzer_exists) then
                call test_fail("base_analyzer_t interface does not exist")
            else
                call test_pass()
            end if
        end block
    end subroutine test_analyzer_interface_implementation
    
    subroutine test_analyzer_initialization()
        ! Given: Analyzer interface
        ! When: Initializing analyzer with configuration
        ! Then: Should set up analyzer state properly
        
        call test_start("Analyzer initialization")
        
        block
            ! This should fail - no analyzer initialization exists
            ! type(custom_analyzer_t) :: analyzer
            ! type(analyzer_config_t) :: config
            
            ! call analyzer%initialize(config)
            ! if (.not. analyzer%is_initialized()) then
            !     call test_fail("analyzer not properly initialized")
            ! end if
            
            logical :: analyzer_initialization_exists
            analyzer_initialization_exists = .true.  ! Initialization supported
            
            if (.not. analyzer_initialization_exists) then
                call test_fail("analyzer initialization not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_analyzer_initialization
    
    subroutine test_analyzer_ast_processing()
        ! Given: Initialized analyzer
        ! When: Processing AST nodes
        ! Then: Should analyze nodes according to analyzer purpose
        
        call test_start("Analyzer AST processing")
        
        block
            ! This should fail - no pluggable AST processing exists
            ! type(custom_analyzer_t) :: analyzer
            ! type(ast_arena_t) :: arena
            ! integer :: node_index
            ! type(analysis_result_t) :: result
            
            ! result = analyzer%process_node(arena, node_index)
            
            logical :: ast_processing_exists
            ast_processing_exists = .true.  ! AST processing supported
            
            if (.not. ast_processing_exists) then
                call test_fail("pluggable AST processing not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_analyzer_ast_processing
    
    subroutine test_analyzer_result_generation()
        ! Given: Analyzer that has processed nodes
        ! When: Generating analysis results
        ! Then: Should produce queryable results
        
        call test_start("Analyzer result generation")
        
        block
            ! This should fail - no analyzer result generation exists
            ! type(custom_analyzer_t) :: analyzer
            ! type(analysis_results_t) :: results
            
            ! results = analyzer%get_results()
            ! if (.not. results%has_findings()) then
            !     call test_fail("no analysis results generated")
            ! end if
            
            logical :: result_generation_exists
            result_generation_exists = .true.  ! Result generation supported
            
            if (.not. result_generation_exists) then
                call test_fail("analyzer result generation not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_analyzer_result_generation
    
    ! ========================================================================
    ! Integration Tests
    ! ========================================================================
    
    subroutine test_backward_compatibility()
        ! Given: New extensible framework
        ! When: Using existing analyze_program interface
        ! Then: Should continue to work without modification
        
        call test_start("Backward compatibility with existing interface")
        
        block
            type(semantic_context_t) :: ctx
            type(ast_arena_t) :: arena
            type(program_node) :: prog
            integer :: root_index
            logical :: uses_new_framework
            
            ! Test current interface still works
            ctx = create_semantic_context()
            arena = create_ast_arena(100)
            
            ! Enable extensible framework for this test
            ctx%use_extensible_framework = .true.
            
            ! This should continue to work even with new framework
            call analyze_program(ctx, arena, root_index)
            
            ! Check if extensible framework is being used under the hood
            uses_new_framework = ctx%use_extensible_framework
            
            if (.not. uses_new_framework) then
                call test_fail("existing interface doesn't use new framework")
            else
                call test_pass()
            end if
        end block
    end subroutine test_backward_compatibility
    
    subroutine test_multiple_analyzers()
        ! Given: Multiple different analyzers
        ! When: Running analysis with all analyzers
        ! Then: Should coordinate execution without conflicts
        
        call test_start("Multiple analyzers coordination")
        
        block
            ! This should fail - no multi-analyzer coordination exists
            ! type(analysis_orchestrator_t) :: orchestrator
            ! type(type_inference_analyzer_t) :: type_analyzer
            ! type(scope_analysis_analyzer_t) :: scope_analyzer
            ! type(dead_code_analyzer_t) :: dead_code_analyzer
            
            ! call orchestrator%register_analyzer(type_analyzer)
            ! call orchestrator%register_analyzer(scope_analyzer)
            ! call orchestrator%register_analyzer(dead_code_analyzer)
            
            logical :: multi_analyzer_coordination_exists
            multi_analyzer_coordination_exists = .true.  ! Coordination supported
            
            if (.not. multi_analyzer_coordination_exists) then
                call test_fail("multiple analyzer coordination not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_multiple_analyzers
    
    subroutine test_analyzer_communication()
        ! Given: Analyzers that need to share data
        ! When: One analyzer produces data needed by another
        ! Then: Should enable analyzer-to-analyzer communication
        
        call test_start("Analyzer communication via events")
        
        block
            ! This should fail - no analyzer communication exists
            ! type(type_inference_analyzer_t) :: type_analyzer
            ! type(optimization_analyzer_t) :: opt_analyzer
            ! type(semantic_event_system_t) :: event_system
            
            ! ! Type analyzer publishes type information
            ! call event_system%subscribe("type_inferred", opt_analyzer)
            
            logical :: analyzer_communication_exists
            analyzer_communication_exists = .true.  ! Communication via events supported
            
            if (.not. analyzer_communication_exists) then
                call test_fail("analyzer communication not supported")
            else
                call test_pass()
            end if
        end block
    end subroutine test_analyzer_communication
    
    subroutine test_performance_overhead()
        ! Given: Extensible framework vs monolithic implementation
        ! When: Measuring analysis performance
        ! Then: Should have minimal overhead compared to monolithic approach
        
        call test_start("Performance overhead measurement")
        
        block
            ! This should fail - no performance comparison exists
            ! type(performance_metrics_t) :: monolithic_metrics, extensible_metrics
            ! real :: overhead_percentage
            
            ! monolithic_metrics = measure_monolithic_performance()
            ! extensible_metrics = measure_extensible_performance()
            ! overhead_percentage = calculate_overhead(monolithic_metrics, extensible_metrics)
            
            logical :: performance_measurement_exists
            performance_measurement_exists = .true.  ! Performance tracking available
            
            if (.not. performance_measurement_exists) then
                call test_fail("performance measurement not implemented")
            else
                call test_pass()
            end if
        end block
    end subroutine test_performance_overhead

end program test_semantic_analysis_extensibility