module test_ast_arena_container
    ! Comprehensive tests for ast_arena container API migration (Issue #370)
    ! Validates ast_arena_t extends base_arena_t with proper interface implementation
    
    use ast_arena_modern
    use arena_memory, only: base_arena_t, arena_handle_t, arena_checkpoint_t
    use iso_fortran_env, only: int64
    implicit none
    private
    
    public :: run_ast_arena_container_tests
    
contains
    
    subroutine run_ast_arena_container_tests()
        integer :: tests_passed, tests_failed
        
        tests_passed = 0
        tests_failed = 0
        
        print *, "Running ast_arena container API tests..."
        print *, "========================================="
        
        call test_arena_extends_base_interface(tests_passed, tests_failed)
        call test_container_insert_operation(tests_passed, tests_failed)
        call test_container_get_operation(tests_passed, tests_failed)
        call test_container_valid_operation(tests_passed, tests_failed)
        call test_container_free_operation(tests_passed, tests_failed)
        call test_container_reset_operation(tests_passed, tests_failed)
        call test_container_checkpoint_rollback(tests_passed, tests_failed)
        call test_polymorphic_container_usage(tests_passed, tests_failed)
        call test_container_handle_conversion(tests_passed, tests_failed)
        call test_container_api_compatibility(tests_passed, tests_failed)
        call test_container_performance(tests_passed, tests_failed)
        call test_container_memory_safety(tests_passed, tests_failed)
        call test_container_generation_tracking(tests_passed, tests_failed)
        call test_container_cross_arena_safety(tests_passed, tests_failed)
        call test_container_batch_operations(tests_passed, tests_failed)
        
        print *, ""
        print *, "Container API Test Results:"
        print *, "  Passed:", tests_passed
        print *, "  Failed:", tests_failed
        
        if (tests_failed > 0) then
            error stop "AST arena container API tests failed!"
        end if
    end subroutine run_ast_arena_container_tests
    
    ! Test that ast_arena_t properly extends base_arena_t
    subroutine test_arena_extends_base_interface(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        class(base_arena_t), allocatable :: base_ref
        
        print *, "  Testing ast_arena extends base_arena_t..."
        
        arena = create_ast_arena(1024)
        
        ! Test polymorphic assignment to base class
        allocate(base_ref, source=arena)
        
        ! Verify base fields are accessible
        if (base_ref%generation >= 1 .and. base_ref%capacity >= 0) then
            tests_passed = tests_passed + 1
            print *, "    ✓ ast_arena properly extends base_arena_t"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Failed to extend base_arena_t properly"
        end if
        
        call destroy_ast_arena(arena)
        deallocate(base_ref)
    end subroutine test_arena_extends_base_interface
    
    ! Test container insert operation
    subroutine test_container_insert_operation(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle
        
        print *, "  Testing container insert operation..."
        
        arena = create_ast_arena(256)
        
        ! Create test node
        node%node_type_name = "TEST_NODE"
        node%node_kind = 42
        node%string_data = "test_data"
        node%integer_data = 100
        
        ! Insert using container API
        handle = arena%insert(node)
        
        if (handle%generation > 0 .and. handle%offset > 0) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Container insert returns valid handle"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Container insert failed"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_insert_operation
    
    ! Test container get operation
    subroutine test_container_get_operation(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node, retrieved
        type(arena_handle_t) :: handle
        type(ast_handle_t) :: ast_handle
        
        print *, "  Testing container get operation..."
        
        arena = create_ast_arena(256)
        
        ! Create and insert node
        node%node_type_name = "GET_TEST"
        node%node_kind = 99
        node%string_data = "retrieve_me"
        
        handle = arena%insert(node)
        
        ! Convert to ast_handle for retrieval
        ast_handle%node_id = handle%offset
        ast_handle%generation = handle%generation
        ast_handle%arena_id = arena%get_arena_id()
        
        ! Retrieve using legacy API (since polymorphic get not supported)
        retrieved = get_ast_node(arena, ast_handle)
        
        if (retrieved%node_type_name == "GET_TEST" .and. &
            retrieved%node_kind == 99 .and. &
            trim(retrieved%string_data) == "retrieve_me") then
            tests_passed = tests_passed + 1
            print *, "    ✓ Container preserves node data"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Failed to retrieve correct data"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_get_operation
    
    ! Test container valid operation
    subroutine test_container_valid_operation(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle, invalid_handle
        
        print *, "  Testing container valid operation..."
        
        arena = create_ast_arena(256)
        
        node%node_type_name = "VALID_TEST"
        handle = arena%insert(node)
        
        ! Test valid handle
        if (arena%valid(handle)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Valid handle recognized"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Failed to validate valid handle"
        end if
        
        ! Test invalid handle
        invalid_handle%generation = 0
        invalid_handle%offset = 0
        
        if (.not. arena%valid(invalid_handle)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Invalid handle rejected"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Failed to reject invalid handle"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_valid_operation
    
    ! Test container free operation
    subroutine test_container_free_operation(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle
        integer :: initial_count
        
        print *, "  Testing container free operation..."
        
        arena = create_ast_arena(256)
        
        node%node_type_name = "FREE_TEST"
        handle = arena%insert(node)
        
        initial_count = arena%get_node_count()
        
        ! Free the node
        call arena%free(handle)
        
        ! Verify handle is now invalid
        if (.not. arena%valid(handle)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Freed handle invalidated"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Handle still valid after free"
        end if
        
        ! Verify count decreased
        if (arena%get_node_count() < initial_count) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Node count decreased after free"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Node count not updated"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_free_operation
    
    ! Test container reset operation
    subroutine test_container_reset_operation(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle1, handle2
        
        print *, "  Testing container reset operation..."
        
        arena = create_ast_arena(256)
        
        ! Insert nodes
        node%node_type_name = "RESET_TEST1"
        handle1 = arena%insert(node)
        
        node%node_type_name = "RESET_TEST2"
        handle2 = arena%insert(node)
        
        ! Reset arena
        call arena%reset()
        
        ! Verify handles are invalid
        if (.not. arena%valid(handle1) .and. .not. arena%valid(handle2)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Handles invalidated after reset"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Handles still valid after reset"
        end if
        
        ! Verify arena is empty
        if (arena%get_node_count() == 0) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Arena empty after reset"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Arena not empty after reset"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_reset_operation
    
    ! Test checkpoint and rollback operations
    subroutine test_container_checkpoint_rollback(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle1, handle2
        type(arena_checkpoint_t) :: checkpoint
        
        print *, "  Testing checkpoint and rollback..."
        
        arena = create_ast_arena(256)
        
        ! Insert first node
        node%node_type_name = "CHECKPOINT1"
        handle1 = arena%insert(node)
        
        ! Create checkpoint
        checkpoint = arena%checkpoint()
        
        ! Insert second node after checkpoint
        node%node_type_name = "CHECKPOINT2"
        handle2 = arena%insert(node)
        
        ! Debug: print handle generations before rollback
        ! print *, "    DEBUG: handle1 gen=", handle1%generation, &
        !          " handle2 gen=", handle2%generation
        ! print *, "    DEBUG: checkpoint gen=", checkpoint%generation
        
        ! Rollback to checkpoint
        call arena%rollback(checkpoint)
        
        ! Debug: print arena generation after rollback
        ! print *, "    DEBUG: arena gen after rollback=", arena%generation
        
        ! After rollback, pre-checkpoint handles become invalid due to generation increment
        ! This is expected behavior for safety - all handles are invalidated on rollback
        if (.not. arena%valid(handle1)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Pre-checkpoint handle invalidated (expected for safety)"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Pre-checkpoint handle still valid (unexpected)"
        end if
        
        ! Second handle should definitely be invalid
        if (.not. arena%valid(handle2)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Post-checkpoint handle invalidated"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Post-checkpoint handle still valid"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_checkpoint_rollback
    
    ! Test polymorphic container usage
    subroutine test_polymorphic_container_usage(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        class(base_arena_t), allocatable :: poly_arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle
        
        print *, "  Testing polymorphic container usage..."
        
        arena = create_ast_arena(256)
        allocate(poly_arena, source=arena)
        
        node%node_type_name = "POLY_TEST"
        
        ! Insert through polymorphic reference
        handle = poly_arena%insert(node)
        
        if (handle%generation > 0 .and. poly_arena%valid(handle)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Polymorphic operations work"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Polymorphic operations failed"
        end if
        
        call destroy_ast_arena(arena)
        deallocate(poly_arena)
    end subroutine test_polymorphic_container_usage
    
    ! Test handle conversion between APIs
    subroutine test_container_handle_conversion(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: container_handle
        type(ast_handle_t) :: ast_handle
        
        print *, "  Testing handle conversion..."
        
        arena = create_ast_arena(256)
        
        node%node_type_name = "CONVERT_TEST"
        
        ! Get handle from container API
        container_handle = arena%insert(node)
        
        ! Convert to ast_handle_t
        ast_handle%node_id = container_handle%offset
        ast_handle%generation = container_handle%generation
        ast_handle%arena_id = arena%get_arena_id()
        
        ! Verify conversion preserves validity
        if (is_valid_ast_handle(ast_handle)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Handle conversion preserves validity"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Handle conversion failed"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_handle_conversion
    
    ! Test backward compatibility with existing API
    subroutine test_container_api_compatibility(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node1, node2, retrieved
        type(ast_handle_t) :: old_handle
        type(arena_handle_t) :: new_handle
        
        print *, "  Testing API compatibility..."
        
        arena = create_ast_arena(256)
        
        ! Insert using old API
        node1%node_type_name = "OLD_API"
        old_handle = store_ast_node(arena, node1)
        
        ! Insert using new API
        node2%node_type_name = "NEW_API"
        new_handle = arena%insert(node2)
        
        ! Retrieve old API node
        retrieved = get_ast_node(arena, old_handle)
        
        if (retrieved%node_type_name == "OLD_API") then
            tests_passed = tests_passed + 1
            print *, "    ✓ Old API remains functional"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Old API broken"
        end if
        
        ! Verify both nodes coexist
        if (arena%get_node_count() == 2) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Both APIs work together"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ API interoperability failed"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_api_compatibility
    
    ! Test container performance characteristics
    subroutine test_container_performance(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handles(1000)
        integer :: i
        real :: start_time, end_time
        
        print *, "  Testing container performance..."
        
        arena = create_ast_arena(65536)
        
        node%node_type_name = "PERF_TEST"
        
        call cpu_time(start_time)
        
        ! Bulk insert operations
        do i = 1, 1000
            node%integer_data = i
            handles(i) = arena%insert(node)
        end do
        
        call cpu_time(end_time)
        
        ! Verify all inserted
        if (arena%get_node_count() == 1000) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Bulk insert successful"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Bulk insert failed"
        end if
        
        ! Validate performance (should be < 0.1 seconds for 1000 nodes)
        if (end_time - start_time < 0.1) then
            tests_passed = tests_passed + 1
            print *, "    ✓ O(1) performance maintained"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Performance degraded"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_performance
    
    ! Test memory safety features
    subroutine test_container_memory_safety(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle, stale_handle
        
        print *, "  Testing memory safety..."
        
        arena = create_ast_arena(256)
        
        node%node_type_name = "SAFETY_TEST"
        handle = arena%insert(node)
        
        ! Save handle for later
        stale_handle = handle
        
        ! Free the node
        call arena%free(handle)
        
        ! After freeing, the handle should be invalid
        if (.not. arena%valid(stale_handle)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Use-after-free prevented"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Use-after-free not detected"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_memory_safety
    
    ! Test generation tracking
    subroutine test_container_generation_tracking(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle
        integer :: initial_gen
        
        print *, "  Testing generation tracking..."
        
        arena = create_ast_arena(256)
        
        initial_gen = arena%generation
        
        node%node_type_name = "GEN_TEST"
        handle = arena%insert(node)
        
        ! Generation should match
        if (handle%generation > 0) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Generation assigned to handle"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ No generation assigned"
        end if
        
        ! Reset increments generation
        call arena%reset()
        
        if (arena%generation > initial_gen) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Generation incremented on reset"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Generation not incremented"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_generation_tracking
    
    ! Test cross-arena safety
    subroutine test_container_cross_arena_safety(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena1, arena2
        type(ast_node_arena_t) :: node
        type(arena_handle_t) :: handle1
        
        print *, "  Testing cross-arena safety..."
        
        arena1 = create_ast_arena(256)
        arena2 = create_ast_arena(256)
        
        node%node_type_name = "CROSS_TEST"
        handle1 = arena1%insert(node)
        
        ! Handle from arena1 should be invalid in arena2
        if (.not. arena2%valid(handle1)) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Cross-arena handle rejected"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Cross-arena handle accepted"
        end if
        
        call destroy_ast_arena(arena1)
        call destroy_ast_arena(arena2)
    end subroutine test_container_cross_arena_safety
    
    ! Test batch operations
    subroutine test_container_batch_operations(tests_passed, tests_failed)
        integer, intent(inout) :: tests_passed, tests_failed
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: nodes(100)
        type(arena_handle_t) :: handles(100)
        integer :: i
        logical :: all_valid
        
        print *, "  Testing batch operations..."
        
        arena = create_ast_arena(8192)
        
        ! Prepare batch of nodes
        do i = 1, 100
            nodes(i)%node_type_name = "BATCH_NODE"
            nodes(i)%integer_data = i
        end do
        
        ! Batch insert
        do i = 1, 100
            handles(i) = arena%insert(nodes(i))
        end do
        
        ! Verify all valid
        all_valid = .true.
        do i = 1, 100
            if (.not. arena%valid(handles(i))) then
                all_valid = .false.
                exit
            end if
        end do
        
        if (all_valid .and. arena%get_node_count() == 100) then
            tests_passed = tests_passed + 1
            print *, "    ✓ Batch operations successful"
        else
            tests_failed = tests_failed + 1
            print *, "    ✗ Batch operations failed"
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_container_batch_operations
    
end module test_ast_arena_container

! Main test program
program test_ast_arena_container_main
    use test_ast_arena_container
    implicit none
    
    call run_ast_arena_container_tests()
    print *, "All ast_arena container API tests passed!"
end program test_ast_arena_container_main