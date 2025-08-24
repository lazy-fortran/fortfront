program test_source_reconstruction_strategies
    use source_reconstruction_analyzer, only: source_location_t, &
                                              source_context_t, &
                                              exact_source_strategy_t, &
                                              generated_source_strategy_t, &
                                              strategy_dispatcher_t, &
                                              node_registry_t, &
                                              reconstruction_quality_t
    use ast_core, only: ast_arena_t, create_ast_arena, create_program, &
                        program_node, identifier_node, create_identifier, &
                        literal_node, LITERAL_INTEGER
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== Source Reconstruction Strategies Tests ==="
    print *, ""

    ! Test 1: Create exact_source_strategy_t
    call test_start("Create exact_source_strategy_t")
    block
        type(exact_source_strategy_t) :: strategy
        character(:), allocatable :: name
        
        name = strategy%get_name()
        if (name == "exact_source") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'exact_source'"
            print *, "  Got: '", name, "'"
        end if
    end block

    ! Test 2: Test exact source reconstruction with available source
    call test_start("Exact source reconstruction with source text")
    block
        type(exact_source_strategy_t) :: strategy
        type(source_context_t) :: context
        type(source_location_t) :: location
        character(:), allocatable :: result
        character(*), parameter :: test_source = "program test" // new_line('a') // &
                                                "  x = 42" // new_line('a') // &
                                                "end program"
        
        call context%initialize_source(test_source)
        location = source_location_t(line=2, column=3, start_char=16, end_char=21)
        
        result = strategy%reconstruct_node(context, location, 0)
        
        if (result == "x = 42") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'x = 42'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 3: Test exact source reconstruction with missing source
    call test_start("Exact source reconstruction without source")
    block
        type(exact_source_strategy_t) :: strategy
        type(source_context_t) :: context
        type(source_location_t) :: location
        character(:), allocatable :: result
        
        ! Don't initialize context - no source available
        location = source_location_t(line=1, column=1, start_char=1, end_char=5)
        
        result = strategy%reconstruct_node(context, location, 0)
        
        if (len(result) == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: empty string"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 4: Create generated_source_strategy_t
    call test_start("Create generated_source_strategy_t")
    block
        type(generated_source_strategy_t) :: strategy
        character(:), allocatable :: name
        
        name = strategy%get_name()
        if (name == "generated_source") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'generated_source'"
            print *, "  Got: '", name, "'"
        end if
    end block

    ! Test 5: Test generated source reconstruction
    call test_start("Generated source reconstruction for identifier")
    block
        type(generated_source_strategy_t) :: strategy
        type(source_context_t) :: context
        type(source_location_t) :: location
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        character(:), allocatable :: result
        integer :: node_index
        
        arena = create_ast_arena()
        id_node = create_identifier("variable_name", line=1, column=1)
        call arena%push(id_node, "identifier")
        node_index = arena%current_index
        
        location = source_location_t(line=1, column=1, start_char=0, end_char=0)
        
        result = strategy%reconstruct_node_with_arena(context, location, &
                                                     node_index, arena)
        
        if (result == "variable_name") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 'variable_name'"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 6: Create node_registry_t
    call test_start("Create and register node strategies")
    block
        type(node_registry_t) :: registry
        type(exact_source_strategy_t) :: exact_strategy
        type(generated_source_strategy_t) :: gen_strategy
        logical :: success
        
        call registry%register_strategy("identifier", gen_strategy)
        call registry%register_strategy("literal", exact_strategy)
        
        success = registry%has_strategy("identifier") .and. &
                  registry%has_strategy("literal") .and. &
                  .not. registry%has_strategy("unknown")
        
        if (success) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: strategies registered correctly"
            print *, "  identifier registered: ", registry%has_strategy("identifier")
            print *, "  literal registered: ", registry%has_strategy("literal")
            print *, "  unknown registered: ", registry%has_strategy("unknown")
        end if
    end block

    ! Test 7: Create strategy_dispatcher_t
    call test_start("Create strategy dispatcher")
    block
        type(strategy_dispatcher_t) :: dispatcher
        type(source_context_t) :: context
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        character(:), allocatable :: result
        integer :: node_index
        
        call dispatcher%initialize_default_strategies()
        
        arena = create_ast_arena()
        id_node = create_identifier("test_var", line=1, column=1)
        call arena%push(id_node, "identifier")
        node_index = arena%current_index
        
        result = dispatcher%reconstruct_node(context, arena, node_index)
        
        if (len(result) > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: non-empty reconstruction"
            print *, "  Got: '", result, "'"
        end if
    end block

    ! Test 8: Create reconstruction_quality_t
    call test_start("Create reconstruction quality assessment")
    block
        type(reconstruction_quality_t) :: quality
        
        call quality%initialize()
        
        if (quality%total_nodes == 0 .and. &
            quality%exact_matches == 0 .and. &
            quality%generated_fallbacks == 0 .and. &
            quality%failed_reconstructions == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: all counts = 0"
            print *, "  total_nodes:", quality%total_nodes
            print *, "  exact_matches:", quality%exact_matches
            print *, "  generated_fallbacks:", quality%generated_fallbacks
            print *, "  failed_reconstructions:", quality%failed_reconstructions
        end if
    end block

    ! Test 9: Test quality assessment tracking
    call test_start("Quality assessment tracking")
    block
        type(reconstruction_quality_t) :: quality
        real :: accuracy
        
        call quality%initialize()
        call quality%record_exact_match()
        call quality%record_exact_match()
        call quality%record_generated_fallback()
        call quality%record_failed_reconstruction()
        
        accuracy = quality%get_accuracy()
        
        if (quality%total_nodes == 4 .and. &
            quality%exact_matches == 2 .and. &
            abs(accuracy - 0.5) < 1e-6) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: total=4, exact=2, accuracy=0.5"
            print *, "  Got: total=", quality%total_nodes, &
                     " exact=", quality%exact_matches, &
                     " accuracy=", accuracy
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All source reconstruction strategy tests passed!"
        stop 0
    else
        print *, "Some source reconstruction strategy tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_source_reconstruction_strategies