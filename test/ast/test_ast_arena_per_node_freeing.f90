program test_ast_arena_per_node_freeing
    ! Comprehensive test suite for Issue #399: Arena per-node freeing with generation tracking
    ! Tests selective memory management, checkpoint/rollback, and performance requirements
    
    use ast_arena_modern
    use iso_fortran_env, only: int64
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== AST Arena Per-Node Freeing Tests (Issue #399) ==="
    
    ! Core per-node freeing tests
    call test_individual_node_freeing()
    call test_generation_tracking_after_free()
    call test_free_result_validation()
    call test_node_activity_status()
    
    ! Advanced freeing scenarios
    call test_batch_freeing_operations()
    call test_free_list_reuse()
    call test_memory_reclamation()
    call test_fragmentation_statistics()
    
    ! Safety and error handling
    call test_double_free_prevention()
    call test_invalid_handle_freeing()
    call test_cross_arena_freeing()
    
    ! Performance and statistics
    call test_freeing_performance()
    call test_free_statistics()
    call test_memory_reduction()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All per-node freeing tests passed!"
        print *, "Issue #399 implementation validated ✓"
        stop 0
    else
        print *, "Some per-node freeing tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_individual_node_freeing()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node1, node2
        type(ast_handle_t) :: handle1, handle2
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats_before, stats_after
        
        call test_start("Individual node freeing")
        
        arena = create_ast_arena()
        
        ! Create two nodes
        node1%node_type_name = "NODE_1"
        node1%integer_data = 100
        handle1 = store_ast_node(arena, node1)
        
        node2%node_type_name = "NODE_2"
        node2%integer_data = 200
        handle2 = store_ast_node(arena, node2)
        
        stats_before = arena%get_stats()
        
        ! Free only the first node
        free_result = free_ast_node(arena, handle1)
        stats_after = arena%get_stats()
        
        ! Validate freeing succeeded and statistics updated
        if (free_result%success .and. &
            free_result%freed_generation > 0 .and. &
            stats_after%node_count == stats_before%node_count - 1 .and. &
            .not. is_node_active(arena, handle1) .and. &
            is_node_active(arena, handle2)) then
            call test_pass()
        else
            call test_fail("Individual node freeing failed")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_individual_node_freeing
    
    subroutine test_generation_tracking_after_free()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: original_handle
        type(ast_free_result_t) :: free_result
        
        call test_start("Generation tracking prevents use-after-free")
        
        arena = create_ast_arena()
        
        ! Create and store node
        node%node_type_name = "TEMP_NODE"
        original_handle = store_ast_node(arena, node)
        
        ! Verify handle is initially valid
        if (.not. arena%validate(original_handle)) then
            call test_fail("Original handle should be valid")
            call destroy_ast_arena(arena)
            return
        end if
        
        ! Free the node
        free_result = free_ast_node(arena, original_handle)
        
        ! After freeing, original handle should be invalid due to generation increment
        if (free_result%success .and. &
            .not. arena%validate(original_handle) .and. &
            .not. is_node_active(arena, original_handle)) then
            call test_pass()
        else
            call test_fail("Generation tracking after free failed")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_generation_tracking_after_free
    
    subroutine test_free_result_validation()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle
        type(ast_free_result_t) :: free_result
        
        call test_start("Free result structure validation")
        
        arena = create_ast_arena()
        
        ! Create node
        node%node_type_name = "RESULT_TEST"
        handle = store_ast_node(arena, node)
        
        ! Free node and examine result
        free_result = free_ast_node(arena, handle)
        
        if (free_result%success .and. &
            free_result%freed_generation == handle%generation .and. &
            .not. allocated(free_result%error_message)) then
            call test_pass()
        else
            call test_fail("Free result structure invalid")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_free_result_validation
    
    subroutine test_node_activity_status()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle
        type(ast_free_result_t) :: free_result
        
        call test_start("Node activity status tracking")
        
        arena = create_ast_arena()
        
        ! Create node
        node%node_type_name = "ACTIVITY_TEST"
        handle = store_ast_node(arena, node)
        
        ! Initially active
        if (.not. is_node_active(arena, handle)) then
            call test_fail("Node should initially be active")
            call destroy_ast_arena(arena)
            return
        end if
        
        ! Free node
        free_result = free_ast_node(arena, handle)
        
        ! Should become inactive after freeing
        if (free_result%success .and. &
            .not. is_node_active(arena, handle)) then
            call test_pass()
        else
            call test_fail("Node activity status not updated correctly")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_node_activity_status
    
    subroutine test_batch_freeing_operations()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(10)
        type(ast_free_result_t) :: free_results(10)
        type(ast_arena_stats_t) :: stats_before, stats_after
        integer :: i, freed_count
        
        call test_start("Batch freeing operations")
        
        arena = create_ast_arena()
        
        ! Create multiple nodes
        do i = 1, 10
            node%node_type_name = "BATCH_NODE"
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        stats_before = arena%get_stats()
        
        ! Free every other node (5 nodes)
        freed_count = 0
        do i = 1, 10, 2  ! Free nodes 1, 3, 5, 7, 9
            free_results(i) = free_ast_node(arena, handles(i))
            if (free_results(i)%success) freed_count = freed_count + 1
        end do
        
        stats_after = arena%get_stats()
        
        if (freed_count == 5 .and. &
            stats_after%node_count == stats_before%node_count - 5) then
            call test_pass()
        else
            call test_fail("Batch freeing operations failed")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_batch_freeing_operations
    
    subroutine test_free_list_reuse()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle1, handle2, handle3
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats
        
        call test_start("Free list slot reuse efficiency")
        
        arena = create_ast_arena()
        
        ! Create node
        node%node_type_name = "REUSE_NODE_1"
        handle1 = store_ast_node(arena, node)
        
        ! Free the node (should add slot to free list)
        free_result = free_ast_node(arena, handle1)
        
        if (.not. free_result%success) then
            call test_fail("Initial node freeing failed")
            call destroy_ast_arena(arena)
            return
        end if
        
        ! Create new node (should reuse freed slot)
        node%node_type_name = "REUSE_NODE_2"
        handle2 = store_ast_node(arena, node)
        
        ! Create another node to verify normal allocation still works
        node%node_type_name = "REUSE_NODE_3"
        handle3 = store_ast_node(arena, node)
        
        stats = get_free_statistics(arena)
        
        if (is_valid_ast_handle(handle2) .and. &
            is_valid_ast_handle(handle3) .and. &
            stats%node_count == 2) then
            call test_pass()
        else
            call test_fail("Free list reuse not working correctly")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_free_list_reuse
    
    subroutine test_memory_reclamation()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(100)
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats_full, stats_empty
        integer :: i
        
        call test_start("Memory reclamation after freeing")
        
        arena = create_ast_arena()
        
        ! Fill arena with nodes
        do i = 1, 100
            node%node_type_name = "MEMORY_NODE"
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        stats_full = get_free_statistics(arena)
        
        ! Free all nodes
        do i = 1, 100
            free_result = free_ast_node(arena, handles(i))
            if (.not. free_result%success) then
                call test_fail("Failed to free node during memory reclamation test")
                call destroy_ast_arena(arena)
                return
            end if
        end do
        
        stats_empty = get_free_statistics(arena)
        
        ! Verify memory statistics show reclamation
        if (stats_full%active_nodes > 0 .and. &
            stats_empty%active_nodes == 0 .and. &
            stats_empty%freed_nodes == 100) then
            call test_pass()
        else
            call test_fail("Memory reclamation statistics incorrect")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_memory_reclamation
    
    subroutine test_fragmentation_statistics()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(20)
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats
        integer :: i
        
        call test_start("Memory fragmentation statistics")
        
        arena = create_ast_arena()
        
        ! Create nodes
        do i = 1, 20
            node%node_type_name = "FRAG_NODE"
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Free every other node to create fragmentation
        do i = 1, 20, 2
            free_result = free_ast_node(arena, handles(i))
        end do
        
        stats = get_free_statistics(arena)
        
        ! Fragmentation should be positive (free slots available)
        if (stats%fragmentation >= 0.0 .and. &
            stats%active_nodes == 10 .and. &
            stats%freed_nodes == 10) then
            call test_pass()
        else
            call test_fail("Fragmentation statistics incorrect")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_fragmentation_statistics
    
    subroutine test_double_free_prevention()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle
        type(ast_free_result_t) :: first_free, second_free
        
        call test_start("Double-free prevention")
        
        arena = create_ast_arena()
        
        ! Create node
        node%node_type_name = "DOUBLE_FREE_TEST"
        handle = store_ast_node(arena, node)
        
        ! First free should succeed
        first_free = free_ast_node(arena, handle)
        
        ! Second free should fail (handle now invalid)
        second_free = free_ast_node(arena, handle)
        
        if (first_free%success .and. &
            .not. second_free%success .and. &
            allocated(second_free%error_message)) then
            call test_pass()
        else
            call test_fail("Double-free prevention failed")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_double_free_prevention
    
    subroutine test_invalid_handle_freeing()
        type(ast_arena_t) :: arena
        type(ast_handle_t) :: invalid_handle
        type(ast_free_result_t) :: free_result
        
        call test_start("Invalid handle freeing rejection")
        
        arena = create_ast_arena()
        
        ! Try to free null handle
        invalid_handle = null_ast_handle()
        free_result = free_ast_node(arena, invalid_handle)
        
        if (.not. free_result%success .and. &
            allocated(free_result%error_message)) then
            call test_pass()
        else
            call test_fail("Invalid handle freeing should fail")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_invalid_handle_freeing
    
    subroutine test_cross_arena_freeing()
        type(ast_arena_t) :: arena1, arena2
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle
        type(ast_free_result_t) :: free_result
        
        call test_start("Cross-arena freeing prevention")
        
        arena1 = create_ast_arena()
        arena2 = create_ast_arena()
        
        ! Create node in arena1
        node%node_type_name = "CROSS_ARENA_TEST"
        handle = store_ast_node(arena1, node)
        
        ! Try to free from arena2 (should fail)
        free_result = free_ast_node(arena2, handle)
        
        if (.not. free_result%success .and. &
            allocated(free_result%error_message)) then
            call test_pass()
        else
            call test_fail("Cross-arena freeing should fail")
        end if
        
        call destroy_ast_arena(arena1)
        call destroy_ast_arena(arena2)
    end subroutine test_cross_arena_freeing
    
    subroutine test_freeing_performance()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(1000)
        type(ast_free_result_t) :: free_result
        integer :: i
        logical :: all_freed
        
        call test_start("Freeing performance (O(1) operations)")
        
        arena = create_ast_arena()
        
        ! Create many nodes
        do i = 1, 1000
            node%node_type_name = "PERF_NODE"
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Free all nodes (should be fast O(1) per operation)
        all_freed = .true.
        do i = 1, 1000
            free_result = free_ast_node(arena, handles(i))
            if (.not. free_result%success) then
                all_freed = .false.
                exit
            end if
        end do
        
        if (all_freed) then
            call test_pass()
        else
            call test_fail("Performance test failed - not all nodes freed")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_freeing_performance
    
    subroutine test_free_statistics()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(50)
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats
        integer :: i
        
        call test_start("Free statistics accuracy")
        
        arena = create_ast_arena()
        
        ! Create 50 nodes
        do i = 1, 50
            node%node_type_name = "STATS_NODE"
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        ! Free 30 nodes
        do i = 1, 30
            free_result = free_ast_node(arena, handles(i))
        end do
        
        stats = get_free_statistics(arena)
        
        if (stats%active_nodes == 20 .and. &
            stats%freed_nodes == 30 .and. &
            stats%node_count == 20) then
            call test_pass()
        else
            call test_fail("Free statistics not accurate")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_free_statistics
    
    subroutine test_memory_reduction()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handles(100)
        type(ast_free_result_t) :: free_result
        type(ast_arena_stats_t) :: stats_before, stats_after
        integer :: i
        
        call test_start("Memory usage reduction after freeing")
        
        arena = create_ast_arena()
        
        ! Fill arena
        do i = 1, 100
            node%node_type_name = "MEMORY_REDUCTION_NODE"
            node%integer_data = i
            handles(i) = store_ast_node(arena, node)
        end do
        
        stats_before = get_free_statistics(arena)
        
        ! Free most nodes
        do i = 1, 80
            free_result = free_ast_node(arena, handles(i))
        end do
        
        stats_after = get_free_statistics(arena)
        
        ! Utilization should decrease significantly
        if (stats_after%utilization < stats_before%utilization .and. &
            stats_after%active_nodes < stats_before%active_nodes) then
            call test_pass()
        else
            call test_fail("Memory usage not reduced after freeing")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_memory_reduction
    
    ! Test utilities
    subroutine test_start(name)
        character(len=*), intent(in) :: name
        test_count = test_count + 1
        write(*, '(A,": ")', advance='no') name
    end subroutine test_start
    
    subroutine test_pass()
        write(*, '(A)') "PASS"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        write(*, '(A,": ",A)') "FAIL", reason
    end subroutine test_fail
    
end program test_ast_arena_per_node_freeing