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

    if (all_tests_passed) then
        print *, "All AST performance tests PASSED!"
    else
        error stop "Some AST performance tests FAILED!"
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

end program test_ast_performance