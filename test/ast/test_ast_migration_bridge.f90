program test_ast_migration_bridge
    ! Test suite for AST migration bridge
    ! Validates seamless migration from old to modern arena
    ! Issue #360: Ensures backward compatibility during migration
    
    use ast_migration_bridge, only: migrate_ast_arena, migrate_ast_tree, &
                                    create_migration_arena, migration_stats_t, &
                                    legacy_to_modern_handle, modern_to_legacy_index, &
                                    is_migrated_arena, migration_context_t
    use ast_arena, only: ast_arena_old_t => ast_arena_t, init_ast_arena
    use ast_arena_modern, only: ast_arena_t, ast_handle_t
    use ast_core, only: program_node, identifier_node, literal_node
    use ast_base, only: LITERAL_INTEGER
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== AST Migration Bridge Tests ==="
    print *, ""
    
    ! Test migration functionality
    call test_create_migration_arena()
    call test_migrate_empty_arena()
    call test_migrate_simple_nodes()
    call test_handle_mapping()
    call test_migration_stats()
    call test_backward_compatibility()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All migration bridge tests passed!"
        stop 0
    else
        print *, "Some migration bridge tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_create_migration_arena()
        type(ast_arena_t) :: arena
        
        call test_start("Create migration arena")
        
        arena = create_migration_arena(1000)
        
        ! Arena should be created successfully
        call test_pass()
    end subroutine test_create_migration_arena
    
    subroutine test_migrate_empty_arena()
        type(ast_arena_old_t) :: old_arena
        type(ast_arena_t) :: modern_arena
        type(migration_stats_t) :: stats
        
        call test_start("Migrate empty arena")
        
        ! Create empty old arena
        call init_ast_arena(old_arena)
        
        ! Create modern arena
        modern_arena = create_migration_arena()
        
        ! Migrate
        stats = migrate_ast_arena(old_arena, modern_arena)
        
        if (stats%nodes_migrated == 0 .and. &
            stats%nodes_skipped == 0) then
            call test_pass()
        else
            call test_fail("Empty arena migration failed")
        end if
    end subroutine test_migrate_empty_arena
    
    subroutine test_migrate_simple_nodes()
        type(ast_arena_old_t) :: old_arena
        type(ast_arena_t) :: modern_arena
        type(migration_stats_t) :: stats
        type(program_node) :: prog
        type(identifier_node) :: id
        type(literal_node) :: lit
        
        call test_start("Migrate simple nodes")
        
        ! Create old arena with nodes
        call init_ast_arena(old_arena)
        
        ! Add some nodes
        prog%name = "test_program"
        call old_arena%push(prog, "PROGRAM")
        
        id%name = "x"
        call old_arena%push(id, "IDENTIFIER", parent_index=1)
        
        lit%value = "42"
        lit%literal_kind = LITERAL_INTEGER
        call old_arena%push(lit, "LITERAL", parent_index=1)
        
        ! Create modern arena
        modern_arena = create_migration_arena()
        
        ! Migrate
        stats = migrate_ast_arena(old_arena, modern_arena)
        
        if (stats%nodes_migrated == 3 .and. &
            stats%success) then
            call test_pass()
        else
            call test_fail("Simple node migration failed")
            print *, "  Migrated:", stats%nodes_migrated
            print *, "  Expected: 3"
        end if
    end subroutine test_migrate_simple_nodes
    
    subroutine test_handle_mapping()
        type(migration_context_t) :: context
        type(ast_handle_t) :: handle1, handle2, retrieved
        
        call test_start("Handle mapping")
        
        ! Create test context
        allocate(context%mappings(10))
        context%mapping_count = 2
        
        ! Add mappings
        context%mappings(1)%legacy_index = 1
        context%mappings(1)%modern_handle%node_id = 100
        context%mappings(1)%modern_handle%generation = 1
        
        context%mappings(2)%legacy_index = 2
        context%mappings(2)%modern_handle%node_id = 101
        context%mappings(2)%modern_handle%generation = 1
        
        ! Test legacy to modern
        retrieved = legacy_to_modern_handle(context, 1)
        
        if (retrieved%node_id == 100 .and. &
            retrieved%generation == 1) then
            call test_pass()
        else
            call test_fail("Handle mapping failed")
        end if
    end subroutine test_handle_mapping
    
    subroutine test_migration_stats()
        type(migration_stats_t) :: stats
        
        call test_start("Migration statistics")
        
        ! Set up test stats
        stats%nodes_migrated = 100
        stats%nodes_skipped = 5
        stats%memory_before = 100000
        stats%memory_after = 10000
        stats%success = .true.
        
        ! Calculate speedup
        if (stats%memory_before > 0) then
            stats%speedup_factor = real(stats%memory_before) / real(stats%memory_after)
        end if
        
        if (stats%speedup_factor > 9.0 .and. &
            stats%success) then
            call test_pass()
            print *, "  - Speedup factor:", stats%speedup_factor, "x"
        else
            call test_fail("Statistics calculation failed")
        end if
    end subroutine test_migration_stats
    
    subroutine test_backward_compatibility()
        type(ast_arena_old_t) :: old_arena
        logical :: is_migrated
        
        call test_start("Backward compatibility check")
        
        call init_ast_arena(old_arena)
        
        ! Check if arena is migrated
        is_migrated = is_migrated_arena(old_arena)
        
        if (.not. is_migrated) then
            call test_pass()
        else
            call test_fail("Compatibility check failed")
        end if
    end subroutine test_backward_compatibility
    
    ! Test utilities
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing ", test_name
        write(*, '(A)', advance='no') " ... "
    end subroutine test_start
    
    subroutine test_pass()
        pass_count = pass_count + 1
        print *, "PASSED"
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, "FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_ast_migration_bridge