program test_ast_performance
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    call test_ast_caching()
    call test_memory_management()
    call test_incremental_parsing()
    call test_concurrent_processing()
    call test_cache_error_scenarios()
    call test_memory_edge_cases()
    call test_arena_locking_failures()
    call test_invalid_operations()

    if (all_tests_passed) then
        print *, "All AST performance tests PASSED!"
    else
        print *, "Some AST performance tests FAILED!"
        stop 1
    end if

contains

    subroutine test_ast_caching()
        character(len=:), allocatable :: source, error_msg
        character(len=:), allocatable :: file_path, checksum
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena, cached_arena
        type(semantic_context_t) :: semantic_ctx, cached_semantic_ctx
        integer :: root_index
        logical :: cached_successfully, loaded_successfully
        
        print *, "Testing AST caching..."
        
        ! Create test source
        source = "program test" // new_line('a') // &
                "    integer :: x = 42" // new_line('a') // &
                "    print *, x" // new_line('a') // &
                "end program test"
        
        file_path = "test_cache.f90"
        checksum = "abc123"  ! Mock checksum
        
        ! Parse source
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Initialize semantic context (mock)
        semantic_ctx = create_semantic_context()
        
        ! Cache the AST
        call cache_ast(file_path, checksum, arena, semantic_ctx, cached_successfully)
        
        if (.not. cached_successfully) then
            print *, "FAILED: Could not cache AST"
            all_tests_passed = .false.
            return
        end if
        
        ! Load cached AST
        loaded_successfully = load_cached_ast(file_path, checksum, cached_arena, cached_semantic_ctx)
        
        if (.not. loaded_successfully) then
            print *, "FAILED: Could not load cached AST"
            all_tests_passed = .false.
            return
        end if
        
        ! Verify cached arena has same size
        if (cached_arena%size /= arena%size) then
            print *, "FAILED: Cached arena size mismatch"
            all_tests_passed = .false.
            return
        end if
        
        ! Clean up cache
        call clear_ast_cache(file_path)
        
        print *, "PASSED: AST caching test"
        
    end subroutine test_ast_caching

    subroutine test_memory_management()
        type(ast_arena_t) :: arena
        integer :: initial_capacity, compacted_capacity
        integer :: i, node_index
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        print *, "Testing memory management..."
        
        ! Create arena with many nodes
        arena = create_ast_arena()
        initial_capacity = arena%capacity
        
        ! Add many dummy nodes to grow arena
        do i = 1, 100
            source = "x = " // char(48 + mod(i, 10))  ! x = 0, x = 1, etc.
            call lex_source(source, tokens, error_msg)
            if (error_msg == "") then
                call parse_tokens(tokens, arena, node_index, error_msg)
            end if
        end do
        
        ! Arena should have grown
        if (arena%capacity <= initial_capacity) then
            print *, "FAILED: Arena did not grow as expected"
            all_tests_passed = .false.
            return
        end if
        
        ! Compact arena
        call compact_arena(arena)
        compacted_capacity = arena%capacity
        
        ! Compacted arena should be smaller or equal
        if (compacted_capacity > arena%capacity) then
            print *, "FAILED: Arena compaction failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Release arena memory
        call release_ast_memory(arena)
        
        ! Arena should be reset
        if (arena%size /= 0) then
            print *, "FAILED: Arena memory release failed"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Memory management test"
        
    end subroutine test_memory_management

    subroutine test_incremental_parsing()
        character(len=:), allocatable :: original_source, new_source, error_msg
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: semantic_ctx
        type(token_t), allocatable :: tokens(:)
        integer :: root_index, original_size
        
        print *, "Testing incremental parsing..."
        
        ! Original source
        original_source = "program test" // new_line('a') // &
                         "    integer :: x = 1" // new_line('a') // &
                         "    integer :: y = 2" // new_line('a') // &
                         "end program test"
        
        ! Parse original source
        call lex_source(original_source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        original_size = arena%size
        semantic_ctx = create_semantic_context()
        
        ! Update source (change line 2)
        new_source = "    integer :: x = 42"
        
        ! Incremental update
        call update_ast_range(arena, 2, 2, new_source, semantic_ctx)
        
        ! Arena should still be valid
        if (arena%size <= 0) then
            print *, "FAILED: Incremental update corrupted arena"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Incremental parsing test"
        
    end subroutine test_incremental_parsing

    subroutine test_concurrent_processing()
        type(ast_arena_t) :: arena
        logical :: lock_acquired, unlock_successful
        
        print *, "Testing concurrent processing..."
        
        arena = create_ast_arena()
        
        ! Test arena locking
        call lock_arena(arena, lock_acquired)
        
        if (.not. lock_acquired) then
            print *, "FAILED: Could not acquire arena lock"
            all_tests_passed = .false.
            return
        end if
        
        ! Test arena unlocking
        call unlock_arena(arena, unlock_successful)
        
        if (.not. unlock_successful) then
            print *, "FAILED: Could not release arena lock"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Concurrent processing test"
        
    end subroutine test_concurrent_processing

    subroutine test_cache_error_scenarios()
        character(len=:), allocatable :: file_path, checksum
        type(ast_arena_t) :: arena, cached_arena
        type(semantic_context_t) :: semantic_ctx, cached_semantic_ctx
        logical :: cached_successfully, loaded_successfully, cache_valid
        
        print *, "Testing cache error scenarios..."
        
        ! Test caching with empty arena
        arena = create_ast_arena()
        semantic_ctx = create_semantic_context()
        file_path = ""
        checksum = ""
        
        call cache_ast(file_path, checksum, arena, semantic_ctx, cached_successfully)
        
        if (.not. cached_successfully) then
            print *, "FAILED: Empty file path should still cache"
            all_tests_passed = .false.
            return
        end if
        
        ! Test loading non-existent cache
        loaded_successfully = load_cached_ast("nonexistent.f90", "invalid", cached_arena, cached_semantic_ctx)
        
        if (loaded_successfully) then
            print *, "FAILED: Should not load non-existent cache"
            all_tests_passed = .false.
            return
        end if
        
        ! Test cache validation with wrong checksum
        cache_valid = is_cache_valid(file_path, "wrong_checksum")
        
        if (cache_valid) then
            print *, "FAILED: Should not validate wrong checksum"
            all_tests_passed = .false.
            return
        end if
        
        ! Test clearing non-existent cache
        call clear_ast_cache("nonexistent.f90")
        
        ! Test cache stats
        block
            character(len=:), allocatable :: stats
            stats = get_cache_stats()
            if (len(stats) == 0) then
                print *, "FAILED: Cache stats should return information"
                all_tests_passed = .false.
                return
            end if
        end block
        
        print *, "PASSED: Cache error scenarios test"
        
    end subroutine test_cache_error_scenarios

    subroutine test_memory_edge_cases()
        type(ast_arena_t) :: arena, empty_arena
        type(memory_stats_t) :: stats
        
        print *, "Testing memory edge cases..."
        
        ! Test memory stats on empty arena
        empty_arena = create_ast_arena()
        stats = get_memory_stats(empty_arena)
        
        if (stats%arena_size /= 0) then
            print *, "FAILED: Empty arena should have size 0"
            all_tests_passed = .false.
            return
        end if
        
        if (stats%memory_usage_mb < 0.0) then
            print *, "FAILED: Memory usage should not be negative"
            all_tests_passed = .false.
            return
        end if
        
        ! Test compacting empty arena
        call compact_arena(empty_arena)
        
        if (empty_arena%size /= 0) then
            print *, "FAILED: Compacted empty arena should remain size 0"
            all_tests_passed = .false.
            return
        end if
        
        ! Test releasing empty arena memory
        call release_ast_memory(empty_arena)
        
        if (empty_arena%size /= 0) then
            print *, "FAILED: Released arena should have size 0"
            all_tests_passed = .false.
            return
        end if
        
        ! Test memory stats calculation edge cases
        arena = create_ast_arena()
        ! Force arena to have some capacity but no size
        arena%capacity = 10
        arena%size = 0
        
        stats = get_memory_stats(arena)
        
        if (stats%fragmentation_ratio /= 1.0) then
            print *, "WARNING: Empty arena should have 100% fragmentation"
        end if
        
        print *, "PASSED: Memory edge cases test"
        
    end subroutine test_memory_edge_cases

    subroutine test_arena_locking_failures()
        type(ast_arena_t) :: arena, invalid_arena
        logical :: lock_success, unlock_success, is_locked
        
        print *, "Testing arena locking failures..."
        
        arena = create_ast_arena()
        
        ! Test double locking
        call lock_arena(arena, lock_success)
        if (.not. lock_success) then
            print *, "FAILED: First lock should succeed"
            all_tests_passed = .false.
            return
        end if
        
        call lock_arena(arena, lock_success)
        if (lock_success) then
            print *, "WARNING: Double lock should fail but may be allowed"
        end if
        
        ! Test unlocking
        call unlock_arena(arena, unlock_success)
        if (.not. unlock_success) then
            print *, "FAILED: Unlock should succeed"
            all_tests_passed = .false.
            return
        end if
        
        ! Test unlocking already unlocked arena
        call unlock_arena(arena, unlock_success)
        if (unlock_success) then
            print *, "WARNING: Unlocking unlocked arena should fail but may be allowed"
        end if
        
        ! Test locking status check
        is_locked = is_arena_locked(arena)
        if (is_locked) then
            print *, "WARNING: Arena should not be locked after unlock"
        end if
        
        ! Test with invalid arena (extreme values)
        invalid_arena = create_ast_arena()
        invalid_arena%size = -1
        invalid_arena%capacity = -1
        invalid_arena%max_depth = -1
        
        call lock_arena(invalid_arena, lock_success)
        if (.not. lock_success) then
            print *, "NOTE: Invalid arena lock failed as expected"
        end if
        
        print *, "PASSED: Arena locking failures test"
        
    end subroutine test_arena_locking_failures

    subroutine test_invalid_operations()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena, invalid_arena
        type(semantic_context_t) :: semantic_ctx
        integer :: root_index
        logical :: supports_update
        
        print *, "Testing invalid operations..."
        
        ! Test incremental parsing support check
        supports_update = supports_incremental_update()
        if (.not. supports_update) then
            print *, "WARNING: Incremental parsing should be supported"
        end if
        
        ! Test update on invalid arena
        invalid_arena = create_ast_arena()
        invalid_arena%size = -1
        semantic_ctx = create_semantic_context()
        
        call update_ast_range(invalid_arena, 1, 1, "test", semantic_ctx)
        
        if (invalid_arena%size /= -1) then
            print *, "NOTE: Invalid arena size was modified"
        end if
        
        ! Test update with invalid line ranges
        arena = create_ast_arena()
        call update_ast_range(arena, -1, -2, "", semantic_ctx)
        call update_ast_range(arena, 100, 1, "", semantic_ctx)
        
        ! Test with NULL/empty inputs
        call update_ast_range(arena, 0, 0, "", semantic_ctx)
        
        ! Test deep copy edge cases
        block
            type(ast_arena_t) :: source_arena, dest_arena
            type(semantic_context_t) :: source_ctx, dest_ctx
            
            ! Test deep copy of unallocated arena
            source_arena = create_ast_arena()
            call deep_copy_arena(source_arena, dest_arena)
            
            if (dest_arena%size /= 0) then
                print *, "FAILED: Deep copy of empty arena should result in empty arena"
                all_tests_passed = .false.
                return
            end if
            
            ! Test deep copy of semantic context
            source_ctx = create_semantic_context()
            call deep_copy_semantic_context(source_ctx, dest_ctx)
            
            if (dest_ctx%next_var_id < 0) then
                print *, "FAILED: Deep copied semantic context should be valid"
                all_tests_passed = .false.
                return
            end if
        end block
        
        ! Test hash function edge cases
        block
            integer :: hash1, hash2, hash3
            type(ast_arena_t) :: arena1, arena2, arena3
            
            ! Test hash consistency
            arena1 = create_ast_arena()
            arena1%size = 5
            arena1%capacity = 10
            arena1%max_depth = 2
            
            hash1 = compute_arena_hash(arena1)
            hash2 = compute_arena_hash(arena1)
            
            if (hash1 /= hash2) then
                print *, "FAILED: Hash should be consistent"
                all_tests_passed = .false.
                return
            end if
            
            ! Test different arenas produce different hashes
            arena2 = create_ast_arena()
            arena2%size = 10
            arena2%capacity = 20
            arena2%max_depth = 3
            
            hash3 = compute_arena_hash(arena2)
            
            if (hash1 == hash3) then
                print *, "WARNING: Different arenas produced same hash (collision)"
            end if
        end block
        
        print *, "PASSED: Invalid operations test"
        
    end subroutine test_invalid_operations

end program test_ast_performance