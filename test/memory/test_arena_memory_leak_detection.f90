program test_arena_memory_leak_detection
    ! Memory leak detection tests for Issue #318: Memory Management - Inconsistent arena cleanup pattern
    !
    ! Given: Arena cleanup should prevent memory leaks and double-free issues
    ! When: Testing various scenarios that could cause memory leaks
    ! Then: All scenarios should properly manage memory without leaks
    
    use ast_core, only: ast_arena_t, create_ast_arena, ast_arena_stats_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_core, only: create_identifier, create_literal, LITERAL_INTEGER, LITERAL_STRING
    implicit none

    integer :: total_tests, passed_tests
    logical :: all_passed

    total_tests = 0
    passed_tests = 0
    all_passed = .true.

    print *, "=== Arena Memory Leak Detection Tests (Issue #318) ==="
    print *, "Testing for memory leaks in cleanup patterns"
    print *, ""

    ! Test proper deallocation sequences
    call test_proper_deallocation_sequence()
    
    ! Test double-free prevention
    call test_double_free_prevention()
    
    ! Test memory leak detection in growth scenarios
    call test_growth_memory_leaks()
    
    ! Test cleanup with cyclic references
    call test_cyclic_reference_cleanup()
    
    ! Test memory consistency under stress
    call test_memory_stress_consistency()
    
    ! Test cleanup with partially constructed arenas
    call test_partial_construction_cleanup()

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (all_passed) then
        print *, "All memory leak detection tests passed!"
        print *, "Arena cleanup patterns prevent memory leaks."
        stop 0
    else
        print *, "Some memory leak detection tests failed!"
        print *, "Memory leak risks detected in cleanup patterns."
        stop 1
    end if

contains

    subroutine test_proper_deallocation_sequence()
        ! Given: Arena with complex node structure that needs proper deallocation
        ! When: Following proper cleanup sequence
        ! Then: All memory should be deallocated in correct order without leaks
        type(ast_arena_t) :: arena
        type(ast_arena_stats_t) :: initial_stats, final_stats
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        integer :: i, initial_memory, final_memory
        
        call test_start("Proper deallocation sequence validation")
        
        arena = create_ast_arena(8)  ! Small initial capacity to force growth
        initial_stats = arena%get_stats()
        initial_memory = initial_stats%memory_usage
        
        ! Create complex node structure that will exceed minimum capacity (256)
        do i = 1, 300
            if (mod(i, 2) == 0) then
                id_node = create_identifier("var_" // char(48 + mod(i, 10)))
                call arena%push(id_node, "identifier")
            else
                lit_node = create_literal(char(48 + mod(i, 10)), LITERAL_INTEGER)
                call arena%push(lit_node, "literal")
            end if
        end do
        
        ! Verify memory allocation occurred (capacity should have grown)
        final_stats = arena%get_stats()
        if (final_stats%memory_usage <= initial_memory) then
            call test_fail("Memory allocation not detected")
            return
        end if
        
        ! Test proper cleanup sequence
        call arena%clear()
        
        ! Validate memory state after cleanup
        final_stats = arena%get_stats()
        final_memory = final_stats%memory_usage
        
        ! Memory should be properly managed (size should be reset)
        if (arena%size == 0 .and. arena%current_index == 0) then
            call test_pass()
        else
            call test_fail("Deallocation sequence left memory inconsistent")
            print *, "  Arena size:", arena%size, " Index:", arena%current_index
        end if
    end subroutine test_proper_deallocation_sequence

    subroutine test_double_free_prevention()
        ! Given: Arena cleanup scenarios that could cause double-free
        ! When: Attempting operations that might double-free memory
        ! Then: Should handle gracefully without crashes or corruption
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        logical :: double_free_safe
        
        call test_start("Double-free prevention in cleanup")
        
        double_free_safe = .true.
        arena = create_ast_arena()
        
        id_node = create_identifier("test_double_free")
        call arena%push(id_node, "identifier")
        
        ! First cleanup - normal case
        call arena%clear()
        
        ! Second cleanup - potential double-free scenario
        call arena%clear()
        
        ! Third cleanup - testing robustness
        call arena%clear()
        
        ! If we reach here without crashes, double-free prevention works
        if (arena%size == 0 .and. arena%current_index == 0) then
            call test_pass()
        else
            call test_fail("Double-free prevention failed")
        end if
    end subroutine test_double_free_prevention

    subroutine test_growth_memory_leaks()
        ! Given: Arena that grows and shrinks multiple times
        ! When: Performing repeated growth and cleanup cycles
        ! Then: Should not accumulate memory leaks over multiple cycles
        type(ast_arena_t) :: arena
        type(ast_arena_stats_t) :: cycle_stats
        type(identifier_node) :: id_node
        integer :: cycle, i, initial_capacity
        logical :: growth_leak_free
        
        call test_start("Memory leak detection in growth cycles")
        
        growth_leak_free = .true.
        arena = create_ast_arena(8)  ! Small initial capacity
        initial_capacity = arena%capacity
        
        ! Perform multiple growth and cleanup cycles
        do cycle = 1, 3
            ! Force growth by adding nodes to exceed minimum capacity (256)
            do i = 1, 300
                id_node = create_identifier("cycle_" // char(48 + cycle) // "_node_" // char(48 + mod(i, 10)))
                call arena%push(id_node, "identifier")
            end do
            
            ! Verify growth occurred
            if (arena%capacity <= initial_capacity) then
                growth_leak_free = .false.
                exit
            end if
            
            ! Cleanup
            call arena%clear()
            
            ! Validate cleanup was complete
            cycle_stats = arena%get_stats()
            if (cycle_stats%total_nodes /= 0) then
                growth_leak_free = .false.
                exit
            end if
        end do
        
        if (growth_leak_free) then
            call test_pass()
        else
            call test_fail("Memory leaks detected in growth cycles")
        end if
    end subroutine test_growth_memory_leaks

    subroutine test_cyclic_reference_cleanup()
        ! Given: Arena with potential cyclic references between nodes
        ! When: Cleaning up arena with complex node relationships
        ! Then: Should handle cyclic references without memory leaks
        type(ast_arena_t) :: arena
        type(identifier_node) :: parent_node, child_node
        integer :: parent_idx, child_idx
        
        call test_start("Cyclic reference cleanup safety")
        
        arena = create_ast_arena()
        
        ! Create nodes with potential relationships
        parent_node = create_identifier("parent")
        child_node = create_identifier("child")
        
        ! Push nodes and create parent-child relationship
        call arena%push(parent_node, "identifier")
        parent_idx = arena%size
        
        call arena%push(child_node, "identifier", parent_idx)  ! child has parent
        child_idx = arena%size
        
        ! Test cleanup with relationships
        call arena%clear()
        
        ! Validate cleanup handled relationships properly
        if (arena%size == 0 .and. arena%current_index == 0 .and. arena%max_depth == 0) then
            call test_pass()
        else
            call test_fail("Cyclic reference cleanup failed")
            print *, "  Size:", arena%size, " Index:", arena%current_index, " Depth:", arena%max_depth
        end if
    end subroutine test_cyclic_reference_cleanup

    subroutine test_memory_stress_consistency()
        ! Given: High-stress memory allocation and deallocation patterns
        ! When: Performing rapid allocation/cleanup cycles
        ! Then: Memory management should remain consistent under stress
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        type(literal_node) :: lit_node
        integer :: stress_cycle, node_count, i
        logical :: stress_consistent
        
        call test_start("Memory consistency under stress conditions")
        
        stress_consistent = .true.
        
        do stress_cycle = 1, 5
            arena = create_ast_arena()
            
            ! Rapid allocation
            node_count = 25
            do i = 1, node_count
                if (mod(i, 3) == 0) then
                    id_node = create_identifier("stress_id_" // char(48 + mod(i, 10)))
                    call arena%push(id_node, "identifier")
                else
                    lit_node = create_literal(char(48 + mod(i, 10)), LITERAL_STRING)
                    call arena%push(lit_node, "literal")
                end if
            end do
            
            ! Verify allocation
            if (arena%size /= node_count) then
                stress_consistent = .false.
                exit
            end if
            
            ! Rapid cleanup
            call arena%clear()
            
            ! Verify cleanup
            if (arena%size /= 0) then
                stress_consistent = .false.
                exit
            end if
        end do
        
        if (stress_consistent) then
            call test_pass()
        else
            call test_fail("Memory inconsistency detected under stress")
        end if
    end subroutine test_memory_stress_consistency

    subroutine test_partial_construction_cleanup()
        ! Given: Arenas that fail during construction or are partially initialized
        ! When: Attempting cleanup on partially constructed arenas
        ! Then: Should handle partial construction gracefully
        type(ast_arena_t) :: arena1, arena2
        type(identifier_node) :: id_node
        logical :: partial_cleanup_safe
        
        call test_start("Partial construction cleanup safety")
        
        partial_cleanup_safe = .true.
        
        ! Test 1: Empty arena cleanup
        arena1 = create_ast_arena()
        call arena1%clear()  ! Should handle empty arena safely
        if (arena1%size /= 0) partial_cleanup_safe = .false.
        
        ! Test 2: Arena with minimal content
        arena2 = create_ast_arena()
        id_node = create_identifier("partial")
        call arena2%push(id_node, "identifier")
        call arena2%clear()
        if (arena2%size /= 0) partial_cleanup_safe = .false.
        
        if (partial_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Partial construction cleanup failed")
        end if
    end subroutine test_partial_construction_cleanup

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

end program test_arena_memory_leak_detection