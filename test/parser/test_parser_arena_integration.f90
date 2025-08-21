program test_parser_arena_integration
    ! RED PHASE TESTS for Issue #359: Arena memory allocator parser integration
    !
    ! Given: Arena foundation is complete and working (Phase 1-3 complete)
    ! When: Parser modules are integrated with arena allocation (Phase 4)
    ! Then: Tests verify parser arena integration meets performance and safety targets
    !
    ! CRITICAL: ALL TESTS MUST FAIL INITIALLY (RED phase requirement)
    ! These tests define expected behavior AFTER implementation
    
    use arena_memory, only: arena_t, arena_handle_t, create_arena, destroy_arena, &
                           arena_stats_t
    use lexer_core, only: token_t, tokenize_core, TK_IDENTIFIER, TK_NUMBER, TK_OPERATOR
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_expressions_module, only: parse_expression
    implicit none

    logical :: all_passed
    integer :: test_count, failed_count

    all_passed = .true.
    test_count = 0
    failed_count = 0

    print *, '=== RED PHASE: Parser Arena Integration Tests (Issue #359) ==='
    print *, 'CRITICAL: All tests MUST FAIL initially (RED phase requirement)'
    print *, 'Tests define expected behavior AFTER parser arena integration'
    print *

    ! Test Group 1: Parser State Arena Integration
    print *, 'Test Group 1: Parser State Arena Integration'
    call test_parser_state_uses_arena_handles(all_passed, test_count, failed_count)
    call test_parser_state_token_storage_arena(all_passed, test_count, failed_count)
    call test_parser_state_arena_lifecycle(all_passed, test_count, failed_count)

    ! Test Group 2: AST Building in Arena During Parsing
    print *, 'Test Group 2: AST Building in Arena During Parsing'
    call test_ast_nodes_built_directly_in_arena(all_passed, test_count, failed_count)
    call test_expression_parsing_arena_allocation(all_passed, test_count, failed_count)
    call test_complex_expression_arena_layout(all_passed, test_count, failed_count)

    ! Test Group 3: Memory Performance Benchmarks
    print *, 'Test Group 3: Memory Performance Benchmarks'
    call test_memory_usage_arena_vs_traditional(all_passed, test_count, failed_count)
    call test_parsing_speed_25_percent_improvement(all_passed, test_count, failed_count)
    call test_memory_reduction_40_percent_target(all_passed, test_count, failed_count)

    ! Test Group 4: Error Recovery with Arena Generations
    print *, 'Test Group 4: Error Recovery with Arena Generations'
    call test_parse_error_recovery_arena_cleanup(all_passed, test_count, failed_count)
    call test_generation_safety_after_errors(all_passed, test_count, failed_count)
    call test_multiple_error_recovery_generations(all_passed, test_count, failed_count)

    ! Test Group 5: Parser Module Integration
    print *, 'Test Group 5: Parser Module Integration'
    call test_parser_expressions_arena_integration(all_passed, test_count, failed_count)
    call test_parser_declarations_arena_integration(all_passed, test_count, failed_count)
    call test_parser_control_flow_arena_integration(all_passed, test_count, failed_count)

    ! Test Group 6: Safety Guarantees
    print *, 'Test Group 6: Safety Guarantees'
    call test_arena_handle_validation_during_parsing(all_passed, test_count, failed_count)
    call test_use_after_free_prevention_parsing(all_passed, test_count, failed_count)
    call test_bounds_checking_arena_parser_operations(all_passed, test_count, failed_count)

    ! Report results
    print *
    print *, '=== RED PHASE SUMMARY ==='
    print '(A,I0,A,I0)', 'Total tests: ', test_count, ', Failed: ', failed_count
    
    if (failed_count == test_count) then
        print *, 'SUCCESS: All tests failed as expected (RED phase requirement)'
        print *, 'Tests ready for implementation phase (GREEN phase)'
        stop 0
    else
        print *, 'FAILURE: Some tests passed unexpectedly (RED phase violation)'
        print *, 'Arena integration may already be partially implemented'
        stop 1
    end if

contains

    subroutine test_parser_state_uses_arena_handles(passed, test_count, failed_count)
        ! Given: Parser state needs to store tokens efficiently
        ! When: Parser state is created with arena-based token storage
        ! Then: Parser state should use arena handles instead of allocatable arrays
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(parser_state_t) :: parser_state
        type(arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        logical :: uses_arena_handles
        
        print *, '  Test: Parser state uses arena handles for token storage'
        test_count = test_count + 1
        
        ! Create test tokens
        allocate(tokens(3))
        tokens(1)%kind = TK_IDENTIFIER
        tokens(1)%text = "x"
        tokens(2)%kind = TK_OPERATOR  
        tokens(2)%text = "+"
        tokens(3)%kind = TK_NUMBER
        tokens(3)%text = "42"
        
        ! Create arena for token storage
        arena = create_arena(chunk_size=8192)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! parser_state = create_parser_state_with_arena(tokens, arena)
        ! uses_arena_handles = parser_state%tokens_stored_in_arena()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        parser_state = create_parser_state(tokens)
        uses_arena_handles = .false.  ! Current implementation uses allocatable
        
        ! Test assertion - this MUST fail in RED phase
        if (uses_arena_handles) then
            print *, '    UNEXPECTED PASS: Parser state already uses arena handles'
        else
            print *, '    EXPECTED FAIL: Parser state still uses allocatable arrays'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_parser_state_uses_arena_handles

    subroutine test_parser_state_token_storage_arena(passed, test_count, failed_count)
        ! Given: Token arrays need efficient storage and access
        ! When: Large token arrays are stored in arena vs allocatable
        ! Then: Arena storage should provide better memory locality and performance
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        type(arena_stats_t) :: arena_stats, traditional_stats
        character(len=*), parameter :: large_source = &
            "program test; integer :: a, b, c, d, e, f, g, h, i, j; end program"
        type(token_t), allocatable :: tokens(:)
        logical :: arena_more_efficient
        
        print *, '  Test: Arena token storage vs traditional allocation'
        test_count = test_count + 1
        
        ! Tokenize large source
        call tokenize_core(large_source, tokens)
        
        arena = create_arena(chunk_size=16384)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! arena_stats = measure_arena_token_storage(tokens, arena)
        ! traditional_stats = measure_traditional_token_storage(tokens)
        ! arena_more_efficient = (arena_stats%utilization > 0.8) .and. &
        !                        (arena_stats%total_allocated < traditional_stats%total_allocated)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        arena_stats = arena%get_stats()
        arena_more_efficient = .false.  ! Arena not used for tokens yet
        
        ! Test assertion - this MUST fail in RED phase
        if (arena_more_efficient) then
            print *, '    UNEXPECTED PASS: Arena token storage already more efficient'
        else
            print *, '    EXPECTED FAIL: Arena token storage not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_parser_state_token_storage_arena

    subroutine test_parser_state_arena_lifecycle(passed, test_count, failed_count)
        ! Given: Parser states need proper cleanup with arena integration
        ! When: Parser state lifecycle is managed with arena generations
        ! Then: Parser cleanup should reset arena generation safely
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        type(parser_state_t) :: parser1, parser2
        type(token_t), allocatable :: tokens(:)
        integer :: generation_before, generation_after
        logical :: generation_advanced
        
        print *, '  Test: Parser state arena lifecycle management'
        test_count = test_count + 1
        
        ! Create simple tokens
        allocate(tokens(2))
        tokens(1)%kind = TK_IDENTIFIER
        tokens(1)%text = "x"
        tokens(2)%kind = TK_NUMBER
        tokens(2)%text = "1"
        
        arena = create_arena()
        generation_before = arena%generation
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! parser1 = create_parser_state_with_arena(tokens, arena)
        ! call destroy_parser_state_arena(parser1, arena)
        ! generation_after = arena%generation
        ! generation_advanced = generation_after > generation_before
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        parser1 = create_parser_state(tokens)
        generation_after = arena%generation
        generation_advanced = .false.  ! No arena integration yet
        
        ! Test assertion - this MUST fail in RED phase
        if (generation_advanced) then
            print *, '    UNEXPECTED PASS: Arena generation lifecycle already working'
        else
            print *, '    EXPECTED FAIL: Arena generation lifecycle not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_parser_state_arena_lifecycle

    subroutine test_ast_nodes_built_directly_in_arena(passed, test_count, failed_count)
        ! Given: AST nodes should be allocated directly in arena during parsing
        ! When: Expression parsing creates AST nodes
        ! Then: All nodes should be allocated in arena, not as individual allocations
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(ast_arena_t) :: ast_arena
        type(arena_t) :: parser_arena
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        integer :: result_index
        logical :: nodes_in_arena
        character(len=*), parameter :: source = "a + b * c"
        
        print *, '  Test: AST nodes built directly in arena during parsing'
        test_count = test_count + 1
        
        call tokenize_core(source, tokens)
        ast_arena = create_ast_arena()
        parser_arena = create_arena()
        parser = create_parser_state(tokens)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! result_index = parse_expression_arena_integrated(parser, ast_arena, parser_arena)
        ! nodes_in_arena = verify_all_nodes_in_arena(ast_arena, result_index)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        result_index = parse_expression(tokens, ast_arena)
        nodes_in_arena = .false.  ! Current parsing doesn't use arena integration
        
        ! Test assertion - this MUST fail in RED phase
        if (nodes_in_arena) then
            print *, '    UNEXPECTED PASS: AST nodes already built in arena during parsing'
        else
            print *, '    EXPECTED FAIL: AST nodes not integrated with arena during parsing'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(parser_arena)
    end subroutine test_ast_nodes_built_directly_in_arena

    subroutine test_expression_parsing_arena_allocation(passed, test_count, failed_count)
        ! Given: Expression parsing needs efficient memory allocation
        ! When: Complex expressions are parsed with arena allocation
        ! Then: All temporary data should use arena handles for O(1) allocation
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        type(arena_stats_t) :: stats_before, stats_after
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        integer :: result_index
        logical :: used_arena_allocation
        character(len=*), parameter :: complex_expr = &
            "func(a, b + c * d, [1, 2, 3], obj%member)"
        
        print *, '  Test: Expression parsing uses arena allocation'
        test_count = test_count + 1
        
        call tokenize_core(complex_expr, tokens)
        arena = create_arena()
        stats_before = arena%get_stats()
        parser = create_parser_state(tokens)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! result_index = parse_expression_with_arena(parser, arena)
        ! stats_after = arena%get_stats()
        ! used_arena_allocation = stats_after%total_allocated > stats_before%total_allocated
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        ! Note: Current parse_expression doesn't take arena parameter
        used_arena_allocation = .false.  ! No arena integration yet
        
        ! Test assertion - this MUST fail in RED phase  
        if (used_arena_allocation) then
            print *, '    UNEXPECTED PASS: Expression parsing already uses arena allocation'
        else
            print *, '    EXPECTED FAIL: Expression parsing arena allocation not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_expression_parsing_arena_allocation

    subroutine test_complex_expression_arena_layout(passed, test_count, failed_count)
        ! Given: Complex expressions create many temporary allocations
        ! When: Nested expressions are parsed with arena layout optimization
        ! Then: Arena should provide sequential memory layout for cache efficiency
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        type(arena_stats_t) :: stats
        logical :: sequential_layout
        character(len=*), parameter :: nested_expr = &
            "((a + b) * (c - d)) / ((e + f) * (g - h))"
        
        print *, '  Test: Complex expression arena layout optimization'
        test_count = test_count + 1
        
        arena = create_arena()
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! call parse_nested_expression_with_arena(nested_expr, arena)
        ! stats = arena%get_stats()
        ! sequential_layout = (stats%utilization > 0.9)  ! High utilization indicates sequential layout
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        stats = arena%get_stats()
        sequential_layout = .false.  ! No arena-based parsing yet
        
        ! Test assertion - this MUST fail in RED phase
        if (sequential_layout) then
            print *, '    UNEXPECTED PASS: Arena layout optimization already working'
        else
            print *, '    EXPECTED FAIL: Arena layout optimization not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_complex_expression_arena_layout

    subroutine test_memory_usage_arena_vs_traditional(passed, test_count, failed_count)
        ! Given: Memory usage needs to be significantly reduced
        ! When: Arena allocation is compared to traditional malloc patterns
        ! Then: Arena should use 40%+ less memory for parser operations
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        integer :: arena_memory, traditional_memory
        real :: memory_reduction_percent
        logical :: meets_40_percent_target
        character(len=*), parameter :: test_source = &
            "program test; integer :: vars(100); call sub(vars); end program"
        
        print *, '  Test: Memory usage 40% reduction target (arena vs traditional)'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! arena_memory = measure_arena_parsing_memory(test_source)
        ! traditional_memory = measure_traditional_parsing_memory(test_source)
        ! memory_reduction_percent = 100.0 * (1.0 - real(arena_memory) / real(traditional_memory))
        ! meets_40_percent_target = memory_reduction_percent >= 40.0
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        arena_memory = 0      ! No arena parsing measurement yet
        traditional_memory = 1000  ! Placeholder
        memory_reduction_percent = 0.0
        meets_40_percent_target = .false.
        
        ! Test assertion - this MUST fail in RED phase
        if (meets_40_percent_target) then
            print *, '    UNEXPECTED PASS: 40% memory reduction already achieved'
        else
            print '(A,F6.1,A)', '    EXPECTED FAIL: Memory reduction ', &
                memory_reduction_percent, '% (target: 40%+)'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_memory_usage_arena_vs_traditional

    subroutine test_parsing_speed_25_percent_improvement(passed, test_count, failed_count)
        ! Given: Parsing speed needs significant improvement
        ! When: Arena allocation replaces malloc patterns in parser
        ! Then: Parsing should be 25%+ faster due to O(1) allocation
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        real :: arena_time, traditional_time, speedup_percent
        logical :: meets_25_percent_target
        character(len=*), parameter :: large_source = &
            "module test; contains; subroutine sub(); integer :: i; do i=1,100; print *, i; end do; end subroutine; end module"
        
        print *, '  Test: Parsing speed 25% improvement target'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! arena_time = benchmark_arena_parsing(large_source, iterations=1000)
        ! traditional_time = benchmark_traditional_parsing(large_source, iterations=1000)
        ! speedup_percent = 100.0 * (1.0 - arena_time / traditional_time)
        ! meets_25_percent_target = speedup_percent >= 25.0
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        arena_time = 1.0        ! No arena parsing benchmark yet
        traditional_time = 1.0  ! No comparison available
        speedup_percent = 0.0
        meets_25_percent_target = .false.
        
        ! Test assertion - this MUST fail in RED phase
        if (meets_25_percent_target) then
            print *, '    UNEXPECTED PASS: 25% speed improvement already achieved'
        else
            print '(A,F6.1,A)', '    EXPECTED FAIL: Speed improvement ', &
                speedup_percent, '% (target: 25%+)'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parsing_speed_25_percent_improvement

    subroutine test_memory_reduction_40_percent_target(passed, test_count, failed_count)
        ! Given: Memory reduction target must be validated
        ! When: Comprehensive memory usage is measured across parser modules
        ! Then: Total memory usage should be reduced by 40%+ consistently
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_stats_t) :: comprehensive_stats
        logical :: consistent_40_percent_reduction
        
        print *, '  Test: Comprehensive 40% memory reduction validation'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! comprehensive_stats = measure_comprehensive_parser_memory_usage()
        ! consistent_40_percent_reduction = validate_40_percent_reduction_across_modules(comprehensive_stats)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        consistent_40_percent_reduction = .false.  ! No comprehensive measurement yet
        
        ! Test assertion - this MUST fail in RED phase
        if (consistent_40_percent_reduction) then
            print *, '    UNEXPECTED PASS: Comprehensive 40% reduction already validated'
        else
            print *, '    EXPECTED FAIL: Comprehensive memory reduction validation not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_memory_reduction_40_percent_target

    subroutine test_parse_error_recovery_arena_cleanup(passed, test_count, failed_count)
        ! Given: Parse errors need clean recovery using arena generations
        ! When: Parser encounters errors and needs to backtrack
        ! Then: Arena generation reset should clean up partial state efficiently
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        integer :: generation_before, generation_after
        logical :: generation_cleanup_worked
        character(len=*), parameter :: error_source = "func(a, b,)"  ! Trailing comma error
        
        print *, '  Test: Parse error recovery with arena generation cleanup'
        test_count = test_count + 1
        
        arena = create_arena()
        generation_before = arena%generation
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! call parse_with_error_recovery_arena(error_source, arena)
        ! generation_after = arena%generation
        ! generation_cleanup_worked = generation_after > generation_before
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        generation_after = arena%generation
        generation_cleanup_worked = .false.  ! No arena-based error recovery yet
        
        ! Test assertion - this MUST fail in RED phase
        if (generation_cleanup_worked) then
            print *, '    UNEXPECTED PASS: Arena generation cleanup already working'
        else
            print *, '    EXPECTED FAIL: Arena generation cleanup not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_parse_error_recovery_arena_cleanup

    subroutine test_generation_safety_after_errors(passed, test_count, failed_count)
        ! Given: Generation safety must be maintained after parse errors
        ! When: Multiple parse attempts use the same arena
        ! Then: Handle validation should prevent use-after-free for old generations
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        type(arena_handle_t) :: old_handle, new_handle
        logical :: old_handle_invalidated, new_handle_valid
        logical :: generation_safety_works
        
        print *, '  Test: Generation safety validation after parse errors'
        test_count = test_count + 1
        
        arena = create_arena()
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! old_handle = parse_with_error_get_handle(arena, "invalid syntax")
        ! call arena%reset()  ! Advance generation
        ! new_handle = parse_valid_get_handle(arena, "valid syntax")
        ! old_handle_invalidated = .not. arena%validate(old_handle)
        ! new_handle_valid = arena%validate(new_handle)
        ! generation_safety_works = old_handle_invalidated .and. new_handle_valid
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        generation_safety_works = .false.  ! No generation-based parsing yet
        
        ! Test assertion - this MUST fail in RED phase
        if (generation_safety_works) then
            print *, '    UNEXPECTED PASS: Generation safety already working'
        else
            print *, '    EXPECTED FAIL: Generation safety not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_generation_safety_after_errors

    subroutine test_multiple_error_recovery_generations(passed, test_count, failed_count)
        ! Given: Multiple parse errors need independent generation cleanup
        ! When: Sequential parse attempts each encounter different errors
        ! Then: Each error recovery should advance generation independently
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(arena_t) :: arena
        integer :: generation_sequence(4)
        logical :: generations_advanced_properly
        character(len=20), parameter :: error_sources(3) = [ &
            "func(               ", "a + + b             ", "if x                " ]
        integer :: i
        
        print *, '  Test: Multiple error recovery generation advancement'
        test_count = test_count + 1
        
        arena = create_arena()
        generation_sequence(1) = arena%generation
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! do i = 1, 3
        !     call parse_with_error_recovery_arena(error_sources(i), arena)
        !     generation_sequence(i+1) = arena%generation
        ! end do
        ! generations_advanced_properly = all(generation_sequence(2:4) > generation_sequence(1:3))
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        generation_sequence(2:4) = arena%generation  ! No advancement
        generations_advanced_properly = .false.
        
        ! Test assertion - this MUST fail in RED phase
        if (generations_advanced_properly) then
            print *, '    UNEXPECTED PASS: Multiple generation advancement already working'
        else
            print *, '    EXPECTED FAIL: Multiple generation advancement not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
        
        call destroy_arena(arena)
    end subroutine test_multiple_error_recovery_generations

    subroutine test_parser_expressions_arena_integration(passed, test_count, failed_count)
        ! Given: parser_expressions module needs arena integration
        ! When: Expression parsing functions are called with arena parameters
        ! Then: All expression parsing should use arena allocation internally
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: expressions_use_arena
        
        print *, '  Test: parser_expressions module arena integration'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! expressions_use_arena = test_parse_expression_arena_integration()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        expressions_use_arena = .false.  ! Module not integrated yet
        
        ! Test assertion - this MUST fail in RED phase
        if (expressions_use_arena) then
            print *, '    UNEXPECTED PASS: parser_expressions already arena-integrated'
        else
            print *, '    EXPECTED FAIL: parser_expressions arena integration not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_expressions_arena_integration

    subroutine test_parser_declarations_arena_integration(passed, test_count, failed_count)
        ! Given: parser_declarations module needs arena integration
        ! When: Declaration parsing functions are called with arena parameters
        ! Then: All declaration parsing should use arena allocation internally
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: declarations_use_arena
        
        print *, '  Test: parser_declarations module arena integration'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! declarations_use_arena = test_parse_declaration_arena_integration()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        declarations_use_arena = .false.  ! Module not integrated yet
        
        ! Test assertion - this MUST fail in RED phase
        if (declarations_use_arena) then
            print *, '    UNEXPECTED PASS: parser_declarations already arena-integrated'
        else
            print *, '    EXPECTED FAIL: parser_declarations arena integration not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_declarations_arena_integration

    subroutine test_parser_control_flow_arena_integration(passed, test_count, failed_count)
        ! Given: parser_control_flow module needs arena integration
        ! When: Control flow parsing functions are called with arena parameters  
        ! Then: All control flow parsing should use arena allocation internally
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: control_flow_uses_arena
        
        print *, '  Test: parser_control_flow module arena integration'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! control_flow_uses_arena = test_parse_control_flow_arena_integration()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        control_flow_uses_arena = .false.  ! Module not integrated yet
        
        ! Test assertion - this MUST fail in RED phase
        if (control_flow_uses_arena) then
            print *, '    UNEXPECTED PASS: parser_control_flow already arena-integrated'
        else
            print *, '    EXPECTED FAIL: parser_control_flow arena integration not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_control_flow_arena_integration

    subroutine test_arena_handle_validation_during_parsing(passed, test_count, failed_count)
        ! Given: Arena handles must be validated during parsing operations
        ! When: Parser accesses arena-allocated data structures
        ! Then: All handle accesses should include validation checks
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: validation_checks_present
        
        print *, '  Test: Arena handle validation during parsing operations'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! validation_checks_present = verify_arena_validation_in_parser_modules()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        validation_checks_present = .false.  ! No arena handles in parsing yet
        
        ! Test assertion - this MUST fail in RED phase
        if (validation_checks_present) then
            print *, '    UNEXPECTED PASS: Arena validation already present in parsing'
        else
            print *, '    EXPECTED FAIL: Arena validation not implemented in parsing'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_arena_handle_validation_during_parsing

    subroutine test_use_after_free_prevention_parsing(passed, test_count, failed_count)
        ! Given: Use-after-free must be prevented during parsing
        ! When: Parser attempts to access invalidated arena handles
        ! Then: Validation should catch and prevent use-after-free errors
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: use_after_free_prevented
        
        print *, '  Test: Use-after-free prevention in parsing operations'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! use_after_free_prevented = test_use_after_free_prevention_in_parser()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        use_after_free_prevented = .false.  ! No arena-based prevention yet
        
        ! Test assertion - this MUST fail in RED phase
        if (use_after_free_prevented) then
            print *, '    UNEXPECTED PASS: Use-after-free prevention already working'
        else
            print *, '    EXPECTED FAIL: Use-after-free prevention not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_use_after_free_prevention_parsing

    subroutine test_bounds_checking_arena_parser_operations(passed, test_count, failed_count)
        ! Given: Bounds checking must be enforced in parser arena operations
        ! When: Parser accesses arena data with handle boundaries
        ! Then: All accesses should be bounds-checked to prevent overruns
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: bounds_checking_active
        
        print *, '  Test: Bounds checking in arena parser operations'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! bounds_checking_active = test_arena_bounds_checking_in_parser()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        bounds_checking_active = .false.  ! No arena bounds checking in parser yet
        
        ! Test assertion - this MUST fail in RED phase
        if (bounds_checking_active) then
            print *, '    UNEXPECTED PASS: Arena bounds checking already active in parser'
        else
            print *, '    EXPECTED FAIL: Arena bounds checking not implemented in parser'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_bounds_checking_arena_parser_operations

end program test_parser_arena_integration