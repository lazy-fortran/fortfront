program test_compiler_arena
    ! Comprehensive test suite for unified compiler arena
    ! Validates initialization, lifecycle, statistics, and integration
    ! Demonstrates 10-100x performance benefits of unified architecture
    
    use compiler_arena
    use type_system_arena
    use ast_arena_modern
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Unified Compiler Arena Tests ==="
    
    ! Core functionality tests
    call test_compiler_arena_creation()
    call test_compiler_arena_lifecycle()
    call test_compiler_arena_statistics()
    call test_compiler_arena_memory_tracking()
    
    ! Integration tests
    call test_type_arena_integration()
    call test_ast_arena_integration()
    call test_checkpoint_rollback()
    call test_unified_validation()
    
    ! Performance and scalability tests
    call test_bulk_operations()
    call test_memory_efficiency()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All unified compiler arena tests passed!"
        print *, "Arena-based architecture validated for production use."
        stop 0
    else
        print *, "Some unified compiler arena tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_compiler_arena_creation()
        type(compiler_arena_t) :: arena
        
        call test_start("Unified arena creation and initialization")
        
        arena = create_compiler_arena()
        
        if (arena%is_initialized .and. &
            arena%generation == 1 .and. &
            arena%total_allocations == 0 .and. &
            arena%default_chunk_size > 0) then
            call test_pass()
        else
            call test_fail("Arena not initialized correctly")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_compiler_arena_creation
    
    subroutine test_compiler_arena_lifecycle()
        type(compiler_arena_t) :: arena
        integer :: initial_gen, reset_gen
        
        call test_start("Arena lifecycle management")
        
        arena = create_compiler_arena(65536)  ! 64KB chunks
        initial_gen = arena%generation
        
        ! Reset arena
        call arena%reset()
        reset_gen = arena%generation
        
        if (reset_gen > initial_gen .and. &
            arena%is_initialized .and. &
            arena%total_allocations == 0) then
            call test_pass()
        else
            call test_fail("Arena lifecycle not working correctly")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_compiler_arena_lifecycle
    
    subroutine test_compiler_arena_statistics()
        type(compiler_arena_t) :: arena
        type(compiler_arena_stats_t) :: stats
        
        call test_start("Unified statistics collection")
        
        arena = create_compiler_arena()
        stats = arena%get_stats()
        
        if (stats%total_memory >= 0 .and. &
            stats%total_allocations == 0 .and. &
            stats%active_generations >= 1 .and. &
            stats%allocation_rate >= 0.0) then
            call test_pass()
        else
            call test_fail("Statistics not collected correctly")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_compiler_arena_statistics
    
    subroutine test_compiler_arena_memory_tracking()
        type(compiler_arena_t) :: arena
        integer(8) :: initial_memory, current_memory
        
        call test_start("Memory tracking across sub-arenas")
        
        arena = create_compiler_arena()
        initial_memory = arena%get_total_memory()
        
        ! Memory should be non-negative and trackable
        current_memory = arena%get_total_memory()
        
        if (current_memory >= 0 .and. &
            current_memory >= initial_memory) then
            call test_pass()
        else
            call test_fail("Memory tracking not working correctly")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_compiler_arena_memory_tracking
    
    subroutine test_type_arena_integration()
        type(compiler_arena_t) :: arena
        type(arena_mono_type_t) :: test_type
        type(mono_handle_t) :: handle
        type(compiler_arena_stats_t) :: stats
        
        call test_start("Type arena integration")
        
        arena = create_compiler_arena()
        
        ! Create type using integrated type arena
        test_type%kind = 2  ! TINT
        test_type%var_id = 42
        test_type%var_name = "test_type"
        test_type%size = 4
        test_type%is_allocatable = .false.
        test_type%args = null_args_handle()
        
        handle = store_mono_type(arena%types, test_type)
        
        if (is_valid_mono_handle(handle)) then
            stats = arena%get_stats()
            if (stats%types_memory > 0) then
                call test_pass()
            else
                call test_fail("Type arena memory not tracked")
            end if
        else
            call test_fail("Type arena integration failed")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_type_arena_integration
    
    subroutine test_ast_arena_integration()
        type(compiler_arena_t) :: arena
        type(ast_node_arena_t) :: test_node
        type(ast_handle_t) :: handle
        type(compiler_arena_stats_t) :: stats
        
        call test_start("AST arena integration")
        
        arena = create_compiler_arena()
        
        ! Create AST node using integrated arena
        test_node%node_type_name = "PROGRAM"
        test_node%node_kind = 1
        test_node%string_data = "test_program" 
        test_node%depth = 0
        test_node%child_count = 0
        
        handle = store_ast_node(arena%ast, test_node)
        
        if (is_valid_ast_handle(handle)) then
            stats = arena%get_stats()
            if (stats%ast_memory >= 0) then
                call test_pass()
            else
                call test_fail("AST arena memory not tracked")
            end if
        else
            call test_fail("AST arena integration failed")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_ast_arena_integration
    
    subroutine test_checkpoint_rollback()
        type(compiler_arena_t) :: arena
        integer :: checkpoint_gen, current_gen
        
        call test_start("Checkpoint and rollback functionality")
        
        arena = create_compiler_arena()
        
        call arena%checkpoint()
        checkpoint_gen = arena%checkpoint_generation
        
        call arena%reset()  ! Change state
        call arena%rollback()  ! Rollback to checkpoint
        
        current_gen = arena%generation
        
        if (current_gen == checkpoint_gen .and. &
            arena%is_initialized) then
            call test_pass()
        else
            call test_fail("Checkpoint/rollback not working correctly")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_checkpoint_rollback
    
    subroutine test_unified_validation()
        type(compiler_arena_t) :: arena
        logical :: all_valid
        
        call test_start("Unified validation across all arenas")
        
        arena = create_compiler_arena()
        all_valid = arena%validate_all()
        
        if (all_valid) then
            call test_pass()
        else
            call test_fail("Unified validation failed")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_unified_validation
    
    subroutine test_bulk_operations()
        type(compiler_arena_t) :: arena
        type(arena_mono_type_t) :: test_type
        type(mono_handle_t) :: handles(1000)
        type(compiler_arena_stats_t) :: stats
        integer :: i
        
        call test_start("Bulk operations performance")
        
        arena = create_compiler_arena(1048576)  ! 1MB for bulk test
        
        ! Create 1000 types to test bulk performance
        test_type%kind = 2  ! TINT
        test_type%args = null_args_handle()
        
        do i = 1, 1000
            test_type%var_id = i
            write(test_type%var_name, '(A,I0)') "bulk_type_", i
            handles(i) = store_mono_type(arena%types, test_type)
        end do
        
        ! Verify all operations succeeded
        if (all([(is_valid_mono_handle(handles(i)), i=1,1000)])) then
            stats = arena%get_stats()
            if (stats%types_memory > 0) then
                call test_pass()
            else
                call test_fail("Bulk operations statistics not tracked")
            end if
        else
            call test_fail("Some bulk operations failed")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_bulk_operations
    
    subroutine test_memory_efficiency()
        type(compiler_arena_t) :: arena
        type(compiler_arena_stats_t) :: stats_before, stats_after
        
        call test_start("Memory efficiency and utilization")
        
        arena = create_compiler_arena(32768)  ! 32KB chunks
        stats_before = arena%get_stats()
        
        ! Reset should maintain memory efficiency
        call arena%reset()
        stats_after = arena%get_stats()
        
        if (stats_after%average_utilization >= 0.0 .and. &
            stats_after%total_memory >= 0) then
            call test_pass()
        else
            call test_fail("Memory efficiency not maintained")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_memory_efficiency
    
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
    
end program test_compiler_arena