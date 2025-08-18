program test_arena_cleanup_consistency
    ! Comprehensive tests for Issue #318: Memory Management - Inconsistent arena cleanup pattern
    !
    ! Given: Arena cleanup operations should be standardized across all use cases
    ! When: Different cleanup scenarios are tested (normal, error, repeated, nested)
    ! Then: All cleanup operations should use arena%clear() consistently and safely
    
    use ast_arena, only: ast_arena_t, create_ast_arena, init_ast_arena, ast_arena_stats_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_core, only: create_identifier, create_literal, LITERAL_INTEGER
    implicit none

    integer :: total_tests, passed_tests
    logical :: all_passed

    total_tests = 0
    passed_tests = 0
    all_passed = .true.

    print *, "=== Arena Cleanup Consistency Tests (Issue #318) ==="
    print *, "Testing standardized cleanup patterns for memory safety"
    print *, ""

    ! Test standardized cleanup method
    call test_standardized_cleanup_method()
    
    ! Test repeated cleanup safety
    call test_repeated_cleanup_safety()
    
    ! Test cleanup after growth operations
    call test_cleanup_after_growth()
    
    ! Test cleanup with different arena states
    call test_cleanup_different_states()
    
    ! Test cleanup consistency across error conditions
    call test_cleanup_error_conditions()
    
    ! Test memory state after cleanup
    call test_memory_state_validation()
    
    ! Test cleanup with nested arenas
    call test_nested_arena_cleanup()

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (all_passed) then
        print *, "All arena cleanup consistency tests passed!"
        print *, "Arena cleanup patterns are standardized and safe."
        stop 0
    else
        print *, "Some arena cleanup consistency tests failed!"
        print *, "Memory management inconsistencies detected."
        stop 1
    end if

contains

    subroutine test_standardized_cleanup_method()
        ! Given: Arena with nodes that needs cleanup
        ! When: Using standardized arena%clear() method
        ! Then: Arena should be properly reset to initial state
        type(ast_arena_t) :: arena
        type(ast_arena_stats_t) :: stats_before, stats_after
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        
        call test_start("Standardized cleanup method consistency")
        
        ! Initialize arena and add nodes
        arena = create_ast_arena()
        id_node = create_identifier("test_var")
        lit_node = create_literal("42", LITERAL_INTEGER)
        
        call arena%push(id_node, "identifier")
        call arena%push(lit_node, "literal")
        
        stats_before = arena%get_stats()
        if (stats_before%total_nodes /= 2) then
            call test_fail("Arena setup failed")
            return
        end if
        
        ! Test standardized cleanup method
        call arena%clear()
        
        stats_after = arena%get_stats()
        
        ! Validate cleanup was complete and consistent
        if (stats_after%total_nodes == 0 .and. &
            arena%size == 0 .and. &
            arena%current_index == 0 .and. &
            arena%max_depth == 0) then
            call test_pass()
        else
            call test_fail("Cleanup method did not reset arena completely")
            print *, "  Expected: nodes=0, size=0, index=0, depth=0"
            print *, "  Got: nodes=", stats_after%total_nodes, &
                     " size=", arena%size, &
                     " index=", arena%current_index, &
                     " depth=", arena%max_depth
        end if
    end subroutine test_standardized_cleanup_method

    subroutine test_repeated_cleanup_safety()
        ! Given: Arena that has already been cleaned up
        ! When: Calling cleanup multiple times (defensive programming scenario)
        ! Then: Should handle repeated cleanup calls safely without errors
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        integer :: i
        logical :: cleanup_safe
        
        call test_start("Repeated cleanup safety")
        
        cleanup_safe = .true.
        arena = create_ast_arena()
        id_node = create_identifier("test_var")
        
        ! Add some content
        call arena%push(id_node, "identifier")
        
        ! Test multiple cleanup calls
        do i = 1, 3
            call arena%clear()
            
            ! Verify arena state remains consistent after each cleanup
            if (arena%size /= 0 .or. arena%current_index /= 0 .or. arena%max_depth /= 0) then
                cleanup_safe = .false.
                exit
            end if
        end do
        
        if (cleanup_safe) then
            call test_pass()
        else
            call test_fail("Repeated cleanup calls caused inconsistent state")
        end if
    end subroutine test_repeated_cleanup_safety

    subroutine test_cleanup_after_growth()
        ! Given: Arena that has grown beyond initial capacity
        ! When: Performing cleanup after capacity expansion
        ! Then: All allocated memory should be properly managed
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        integer :: initial_capacity, grown_capacity
        integer :: i
        
        call test_start("Cleanup after arena growth")
        
        ! Create arena with minimum possible capacity (256)
        arena = create_ast_arena(256)
        initial_capacity = arena%capacity
        
        ! Force growth by adding more nodes than initial capacity
        ! Need to exceed 256 to trigger growth
        do i = 1, 257
            id_node = create_identifier("var_" // char(48 + mod(i, 10)))
            call arena%push(id_node, "identifier")
        end do
        
        grown_capacity = arena%capacity
        if (grown_capacity <= initial_capacity) then
            call test_fail("Arena did not grow as expected")
            return
        end if
        
        if (arena%size /= 257) then
            call test_fail("Arena size incorrect after growth")
            return
        end if
        
        ! Test cleanup after growth
        call arena%clear()
        
        ! Validate cleanup handled grown arena properly
        if (arena%size == 0 .and. arena%current_index == 0 .and. arena%max_depth == 0) then
            call test_pass()
        else
            call test_fail("Cleanup after growth left arena in inconsistent state")
        end if
    end subroutine test_cleanup_after_growth

    subroutine test_cleanup_different_states()
        ! Given: Arenas in different states (empty, partial, full, after pop operations)
        ! When: Applying standardized cleanup to each state
        ! Then: All states should cleanup consistently
        type(ast_arena_t) :: arena1, arena2, arena3, arena4
        type(identifier_node) :: id_node
        logical :: all_states_clean
        integer :: i
        
        call test_start("Cleanup consistency across different arena states")
        
        all_states_clean = .true.
        
        ! State 1: Empty arena
        arena1 = create_ast_arena()
        call arena1%clear()
        if (arena1%size /= 0) all_states_clean = .false.
        
        ! State 2: Partially filled arena
        arena2 = create_ast_arena()
        id_node = create_identifier("partial")
        call arena2%push(id_node, "identifier")
        call arena2%clear()
        if (arena2%size /= 0) all_states_clean = .false.
        
        ! State 3: Arena with multiple nodes
        arena3 = create_ast_arena()
        do i = 1, 5
            id_node = create_identifier("node_" // char(48 + i))
            call arena3%push(id_node, "identifier")
        end do
        call arena3%clear()
        if (arena3%size /= 0) all_states_clean = .false.
        
        ! State 4: Arena after pop operations
        arena4 = create_ast_arena()
        do i = 1, 3
            id_node = create_identifier("temp_" // char(48 + i))
            call arena4%push(id_node, "identifier")
        end do
        call arena4%pop()  ! Remove one node
        call arena4%clear()
        if (arena4%size /= 0) all_states_clean = .false.
        
        if (all_states_clean) then
            call test_pass()
        else
            call test_fail("Cleanup inconsistent across different arena states")
        end if
    end subroutine test_cleanup_different_states

    subroutine test_cleanup_error_conditions()
        ! Given: Arenas that encounter error conditions during usage
        ! When: Cleanup is performed after error conditions
        ! Then: Cleanup should handle error states gracefully
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        logical :: cleanup_after_errors
        
        call test_start("Cleanup consistency after error conditions")
        
        cleanup_after_errors = .true.
        arena = create_ast_arena()
        
        ! Simulate error condition by attempting invalid operations
        id_node = create_identifier("test")
        call arena%push(id_node, "identifier")
        
        ! Simulate error recovery scenario
        ! In real usage, this would be after parsing errors, validation failures, etc.
        
        ! Test cleanup after simulated error
        call arena%clear()
        
        ! Verify cleanup worked despite error conditions
        if (arena%size /= 0 .or. arena%current_index /= 0) then
            cleanup_after_errors = .false.
        end if
        
        if (cleanup_after_errors) then
            call test_pass()
        else
            call test_fail("Cleanup failed after error conditions")
        end if
    end subroutine test_cleanup_error_conditions

    subroutine test_memory_state_validation()
        ! Given: Arena with allocated memory that needs cleanup
        ! When: Performing cleanup and checking memory state
        ! Then: Memory should be in consistent state without leaks
        type(ast_arena_t) :: arena
        type(ast_arena_stats_t) :: stats
        type(identifier_node) :: id_node
        integer :: i
        
        call test_start("Memory state validation after cleanup")
        
        arena = create_ast_arena()
        
        ! Add nodes to allocate memory
        do i = 1, 8
            id_node = create_identifier("memory_test_" // char(48 + i))
            call arena%push(id_node, "identifier")
        end do
        
        ! Verify arena has allocated memory
        stats = arena%get_stats()
        if (stats%memory_usage <= 0) then
            call test_fail("Arena memory usage reporting incorrect")
            return
        end if
        
        ! Perform cleanup
        call arena%clear()
        
        ! Validate memory state after cleanup
        stats = arena%get_stats()
        
        ! Memory should be reset but capacity may remain for efficiency
        if (arena%size == 0 .and. arena%current_index == 0 .and. &
            arena%max_depth == 0 .and. stats%total_nodes == 0) then
            call test_pass()
        else
            call test_fail("Memory state inconsistent after cleanup")
            print *, "  Size:", arena%size, " Index:", arena%current_index, &
                     " Depth:", arena%max_depth, " Nodes:", stats%total_nodes
        end if
    end subroutine test_memory_state_validation

    subroutine test_nested_arena_cleanup()
        ! Given: Scenarios with multiple arenas (nested usage patterns)
        ! When: Cleaning up arenas in different orders
        ! Then: Each arena should cleanup independently and safely
        type(ast_arena_t) :: outer_arena, inner_arena
        type(identifier_node) :: id_node
        logical :: nested_cleanup_safe
        
        call test_start("Nested arena cleanup independence")
        
        nested_cleanup_safe = .true.
        
        ! Create nested arena scenario
        outer_arena = create_ast_arena()
        inner_arena = create_ast_arena()
        
        id_node = create_identifier("outer")
        call outer_arena%push(id_node, "identifier")
        
        id_node = create_identifier("inner")
        call inner_arena%push(id_node, "identifier")
        
        ! Test cleanup in different orders
        ! First cleanup inner arena
        call inner_arena%clear()
        if (inner_arena%size /= 0) nested_cleanup_safe = .false.
        
        ! Outer arena should be unaffected
        if (outer_arena%size /= 1) nested_cleanup_safe = .false.
        
        ! Then cleanup outer arena
        call outer_arena%clear()
        if (outer_arena%size /= 0) nested_cleanup_safe = .false.
        
        if (nested_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Nested arena cleanup not independent")
        end if
    end subroutine test_nested_arena_cleanup

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
        all_passed = .false.
    end subroutine test_fail

end program test_arena_cleanup_consistency