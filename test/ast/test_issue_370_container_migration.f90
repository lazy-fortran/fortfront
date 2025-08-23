program test_issue_370_container_migration
    ! Simple test for Issue #370: Migrate ast_arena to container API
    ! Validates that ast_arena_t properly extends base_arena_t
    
    use ast_arena_modern
    use arena_memory, only: base_arena_t, arena_handle_t
    implicit none
    
    print *, "Testing Issue #370: ast_arena container API migration"
    
    call test_base_interface()
    call test_existing_api()
    
    print *, "Issue #370 implementation: COMPLETE"
    print *, "ast_arena_t successfully extends base_arena_t with type-bound procedures"
    stop 0
    
contains
    
    subroutine test_base_interface()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        type(ast_node_arena_t) :: node
        
        print *, "  Testing base_arena_t interface..."
        
        ! Create ast_arena_t through base interface
        allocate(ast_arena_t :: arena)
        select type (arena)
        type is (ast_arena_t)
            arena = create_ast_arena()
        end select
        
        ! Test insert through base interface
        node%node_type_name = "TEST"
        handle = arena%insert(node)
        
        if (.not. arena%valid(handle)) then
            print *, "    ERROR: Handle not valid after insert"
            stop 1
        end if
        
        ! Test free through base interface
        call arena%free(handle)
        
        ! Test reset through base interface
        call arena%reset()
        
        print *, "    Base interface: PASS"
        
        deallocate(arena)
    end subroutine test_base_interface
    
    subroutine test_existing_api()
        type(ast_arena_t) :: arena
        type(ast_node_arena_t) :: node
        type(ast_handle_t) :: handle
        
        print *, "  Testing backward compatibility..."
        
        arena = create_ast_arena()
        
        ! Test existing API
        node%node_type_name = "COMPAT"
        handle = store_ast_node(arena, node)
        
        if (.not. is_valid_ast_handle(handle)) then
            print *, "    ERROR: Existing API broken"
            stop 1
        end if
        
        print *, "    Backward compatibility: PASS"
        
        call destroy_ast_arena(arena)
    end subroutine test_existing_api
    
end program test_issue_370_container_migration