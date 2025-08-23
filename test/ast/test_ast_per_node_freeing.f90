program test_ast_per_node_freeing
    ! Test per-node freeing functionality for Issue #399
    use ast_arena_modern
    implicit none

    integer :: tests_passed = 0
    integer :: tests_failed = 0

    print *, "==============================="
    print *, "AST Per-Node Freeing Test Suite"
    print *, "==============================="
    print *, ""

    ! Run all tests
    call test_basic_node_freeing()
    call test_generation_safety_after_freeing()
    call test_slot_reuse_after_freeing()
    call test_free_statistics()
    call test_error_cases()

    ! Report results
    print *, ""
    print *, "==============================="
    print *, "Per-Node Freeing Test Results:"
    print *, "Tests passed:", tests_passed
    print *, "Tests failed:", tests_failed
    print *, "==============================="
    
    if (tests_failed > 0) then
        print *, "VERDICT: Per-node freeing has issues"
        stop 1
    else
        print *, "VERDICT: Per-node freeing working correctly"
    end if

contains

    ! Test basic node freeing functionality
    subroutine test_basic_node_freeing()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle
        type(ast_free_result_t) :: free_result
        logical :: was_active, is_active_after

        print *, "Testing basic node freeing..."
        
        arena = create_ast_arena(10)
        
        ! Create a test node
        node%node_type_name = "TEST_NODE"
        node%node_kind = 1
        handle = store_ast_node(arena, node)
        
        ! Verify node is initially active
        was_active = is_node_active(arena, handle)
        
        ! Free the node
        free_result = free_ast_node(arena, handle)
        
        ! Verify node is no longer active
        is_active_after = is_node_active(arena, handle)
        
        if (.not. was_active) then
            print *, "  FAILED: Node was not initially active"
            tests_failed = tests_failed + 1
        else if (.not. free_result%success) then
            print *, "  FAILED: Node freeing failed"
            if (allocated(free_result%error_message)) then
                print *, "    Error:", free_result%error_message
            end if
            tests_failed = tests_failed + 1
        else if (is_active_after) then
            print *, "  FAILED: Node still active after freeing"
            tests_failed = tests_failed + 1
        else if (free_result%freed_generation /= handle%generation) then
            print *, "  FAILED: Wrong freed generation"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Basic node freeing works correctly"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_basic_node_freeing

    ! Test generation safety after freeing
    subroutine test_generation_safety_after_freeing()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node1, node2
        type(ast_handle_t) :: handle1, handle2
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats

        print *, "Testing generation safety after freeing..."
        
        arena = create_ast_arena(10)
        
        ! Create and free first node
        node1%node_type_name = "NODE_1"
        handle1 = store_ast_node(arena, node1)
        free_result = free_ast_node(arena, handle1)
        
        ! Create second node (might reuse freed slot)
        node2%node_type_name = "NODE_2"
        handle2 = store_ast_node(arena, node2)
        
        ! Old handle should still be invalid even if slot was reused
        if (is_node_active(arena, handle1)) then
            print *, "  FAILED: Freed handle became valid again"
            tests_failed = tests_failed + 1
        else if (.not. is_node_active(arena, handle2)) then
            print *, "  FAILED: New handle is not active"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Generation safety maintained after freeing"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_generation_safety_after_freeing

    ! Test slot reuse after freeing
    subroutine test_slot_reuse_after_freeing()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(5)
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats_before, stats_after
        integer :: i

        print *, "Testing slot reuse after freeing..."
        
        arena = create_ast_arena(10)
        
        ! Create several nodes
        do i = 1, 5
            write(node%node_type_name, '(A,I0)') "NODE_", i
            handles(i) = store_ast_node(arena, node)
        end do
        
        stats_before = get_free_statistics(arena)
        
        ! Free every other node
        free_result = free_ast_node(arena, handles(2))
        free_result = free_ast_node(arena, handles(4))
        
        stats_after = get_free_statistics(arena)
        
        ! Check statistics
        if (stats_after%active_nodes /= stats_before%active_nodes - 2) then
            print *, "  FAILED: Active node count not decremented correctly"
            tests_failed = tests_failed + 1
        else if (stats_after%freed_nodes /= stats_before%freed_nodes + 2) then
            print *, "  FAILED: Freed node count not incremented correctly"
            tests_failed = tests_failed + 1
        else if (stats_after%fragmentation <= stats_before%fragmentation) then
            print *, "  FAILED: Fragmentation not increased after freeing"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Slot reuse tracking works correctly"
            print *, "    Active nodes:", stats_after%active_nodes
            print *, "    Freed nodes:", stats_after%freed_nodes
            print *, "    Fragmentation:", int(stats_after%fragmentation * 100), "%"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_slot_reuse_after_freeing

    ! Test free statistics
    subroutine test_free_statistics()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(3)
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats
        integer :: i

        print *, "Testing free statistics..."
        
        arena = create_ast_arena(10)
        
        ! Create 3 nodes
        do i = 1, 3
            write(node%node_type_name, '(A,I0)') "STAT_NODE_", i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Free 1 node
        free_result = free_ast_node(arena, handles(2))
        
        stats = get_free_statistics(arena)
        
        if (stats%active_nodes /= 2) then
            print *, "  FAILED: Wrong active node count:", stats%active_nodes
            tests_failed = tests_failed + 1
        else if (stats%freed_nodes /= 1) then
            print *, "  FAILED: Wrong freed node count:", stats%freed_nodes  
            tests_failed = tests_failed + 1
        else if (stats%node_count /= 2) then
            print *, "  FAILED: Wrong total node count:", stats%node_count
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Free statistics are correct"
            print *, "    Total allocated:", stats%total_allocations
            print *, "    Active nodes:", stats%active_nodes
            print *, "    Freed nodes:", stats%freed_nodes
            tests_passed = tests_passed + 1
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_free_statistics

    ! Test error cases
    subroutine test_error_cases()
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: handle, null_handle, invalid_handle
        type(ast_free_result_t) :: free_result

        print *, "Testing error cases..."
        
        arena = create_ast_arena(10)
        
        ! Test freeing null handle
        null_handle = null_ast_handle()
        free_result = free_ast_node(arena, null_handle)
        
        if (free_result%success) then
            print *, "  FAILED: Freeing null handle should fail"
            tests_failed = tests_failed + 1
            return
        end if
        
        ! Test freeing invalid handle
        invalid_handle%node_id = 999
        invalid_handle%generation = 1
        free_result = free_ast_node(arena, invalid_handle)
        
        if (free_result%success) then
            print *, "  FAILED: Freeing invalid handle should fail"
            tests_failed = tests_failed + 1
            return
        end if
        
        print *, "  PASSED: Error cases handled correctly"
        tests_passed = tests_passed + 1
        
        call destroy_ast_arena(arena)
    end subroutine test_error_cases

end program test_ast_per_node_freeing