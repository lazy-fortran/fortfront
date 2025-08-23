program test_ast_arena_container_api
    ! Test suite for ast_arena_t container API implementation (Issue #370)
    ! Validates that ast_arena_t properly extends base_arena_t with type-bound procedures
    
    use ast_arena_modern
    use arena_memory, only: base_arena_t, arena_handle_t
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    print *, "=== AST Arena Container API Tests (Issue #370) ==="
    
    ! Test container API through base_arena_t interface
    call test_polymorphic_usage()
    call test_insert_operation()
    call test_valid_operation()
    call test_free_operation()
    call test_base_arena_methods()
    call test_backward_compatibility()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All container API tests passed!"
        print *, "Issue #370: ast_arena successfully migrated to container API"
        stop 0
    else
        print *, "Some container API tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_polymorphic_usage()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        type(ast_node_arena_t) :: node
        
        call test_start("Polymorphic base_arena_t usage")
        
        ! Allocate as ast_arena_t but use through base_arena_t interface
        allocate(ast_arena_t :: arena)
        select type (arena)
        type is (ast_arena_t)
            arena = create_ast_arena()
        end select
        
        ! Test that base interface fields are accessible
        if (arena%generation == 1 .and. &
            arena%size == 0 .and. &
            arena%capacity > 0) then
            call test_pass()
        else
            write(*, '(A,I0,A,I0,A,I0)') "DEBUG: gen=", arena%generation, &
                " size=", arena%size, " capacity=", arena%capacity
            call test_fail("Base arena fields not properly initialized")
        end if
        
        deallocate(arena)
    end subroutine test_polymorphic_usage
    
    subroutine test_insert_operation()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        type(ast_node_arena_t) :: node
        
        call test_start("Container API insert operation")
        
        allocate(ast_arena_t :: arena)
        select type (arena)
        type is (ast_arena_t)
            arena = create_ast_arena()
        end select
        
        ! Create test node
        node%node_type_name = "TEST_INSERT"
        node%node_kind = 99
        node%string_data = "test_data"
        
        ! Insert through base interface
        handle = arena%insert(node)
        
        ! Verify handle is valid and size was updated
        if (arena%valid(handle) .and. arena%size == 1) then
            call test_pass()
        else
            call test_fail("Insert operation failed")
        end if
        
        deallocate(arena)
    end subroutine test_insert_operation
    
    subroutine test_valid_operation()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: valid_handle, invalid_handle
        type(ast_node_arena_t) :: node
        
        call test_start("Container API valid operation")
        
        allocate(ast_arena_t :: arena)
        select type (arena)
        type is (ast_arena_t)
            arena = create_ast_arena()
        end select
        
        ! Create valid handle
        node%node_type_name = "TEST_VALID"
        valid_handle = arena%insert(node)
        
        ! Create invalid handle
        invalid_handle%offset = 999
        invalid_handle%generation = 999
        invalid_handle%chunk_id = 999
        
        ! Test validation through base interface
        if (arena%valid(valid_handle) .and. &
            .not. arena%valid(invalid_handle)) then
            call test_pass()
        else
            call test_fail("Valid operation not working correctly")
        end if
        
        deallocate(arena)
    end subroutine test_valid_operation
    
    subroutine test_free_operation()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        type(ast_node_arena_t) :: node
        integer :: size_before, size_after
        
        call test_start("Container API free operation")
        
        allocate(ast_arena_t :: arena)
        select type (arena)
        type is (ast_arena_t)
            arena = create_ast_arena()
        end select
        
        ! Insert node
        node%node_type_name = "TEST_FREE"
        handle = arena%insert(node)
        size_before = arena%size
        
        ! Free through base interface
        call arena%free(handle)
        size_after = arena%size
        
        ! Verify size decremented (arena uses bulk deallocation, so this is a counter update)
        if (size_before == 1 .and. size_after == 0) then
            call test_pass()
        else
            call test_fail("Free operation not updating size correctly")
        end if
        
        deallocate(arena)
    end subroutine test_free_operation
    
    subroutine test_base_arena_methods()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        type(ast_node_arena_t) :: node
        integer :: gen_before, gen_after
        
        call test_start("Base arena reset/checkpoint/rollback")
        
        allocate(ast_arena_t :: arena)
        select type (arena)
        type is (ast_arena_t)
            arena = create_ast_arena()
        end select
        
        ! Test checkpoint
        call arena%checkpoint()
        
        ! Insert node
        node%node_type_name = "TEST_CHECKPOINT"
        handle = arena%insert(node)
        gen_before = arena%generation
        
        ! Test reset (should invalidate handles)
        call arena%reset()
        gen_after = arena%generation
        
        if (gen_after > gen_before .and. &
            .not. arena%valid(handle) .and. &
            arena%size == 0) then
            call test_pass()
        else
            call test_fail("Base arena methods not working correctly")
        end if
        
        deallocate(arena)
    end subroutine test_base_arena_methods
    
    subroutine test_backward_compatibility()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node, retrieved
        type(ast_handle_t) :: ast_handle
        type(ast_arena_stats_t) :: stats
        
        call test_start("Backward compatibility with existing API")
        
        arena = create_ast_arena()
        
        ! Test existing API still works
        node%node_type_name = "BACKWARD_COMPAT"
        node%node_kind = 42
        node%string_data = "legacy_api"
        
        ! Use existing store/get functions
        ast_handle = store_ast_node(arena, node)
        
        if (.not. is_valid_ast_handle(ast_handle)) then
            call test_fail("Failed to store node with legacy API")
            call destroy_ast_arena(arena)
            return
        end if
        
        retrieved = get_ast_node(arena, ast_handle)
        
        ! Verify retrieval
        if (len_trim(retrieved%node_type_name) > 0 .and. &
            retrieved%node_kind == 42) then
            
            ! Also test other existing methods
            stats = arena%get_stats()
            if (stats%node_count == 1 .and. &
                arena%get_node_count() == 1) then
                call test_pass()
            else
                call test_fail("Stats not correct")
            end if
        else
            call test_fail("Legacy API not working correctly")
        end if
        
        call destroy_ast_arena(arena)
    end subroutine test_backward_compatibility
    
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
    
end program test_ast_arena_container_api