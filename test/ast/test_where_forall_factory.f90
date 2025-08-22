module test_where_forall_factory
    use ast_core
    use ast_factory
    use ast_arena
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing WHERE/FORALL Factory Functions ==="
        print *, ""
        
        call test_push_where_simple()
        call test_push_where_with_elsewhere()
        call test_push_forall_simple()
        call test_push_forall_with_mask()
        call test_push_forall_with_stride()
        
        print *, ""
        print *, "All WHERE/FORALL factory tests passed!"
    end subroutine run_tests

    subroutine test_push_where_simple()
        type(ast_arena_t) :: arena
        integer :: mask_idx, body1_idx, body2_idx, where_idx
        integer, allocatable :: body_indices(:)
        
        print *, "Testing push_where (simple)..."
        
        arena = create_ast_arena()
        
        ! Create components
        mask_idx = push_identifier(arena, "mask")
        body1_idx = push_identifier(arena, "stmt1")
        body2_idx = push_identifier(arena, "stmt2")
        
        allocate(body_indices(2))
        body_indices = [body1_idx, body2_idx]
        
        ! Push WHERE
        where_idx = push_where(arena, mask_idx, body_indices)
        
        if (where_idx <= 0) then
            print *, "Failed to push WHERE node"
            stop 1
        end if
        
        ! Verify the node
        select type(node => arena%entries(where_idx)%node)
        type is (where_node)
            if (node%mask_expr_index /= mask_idx) then
                print *, "Wrong mask index"
                stop 1
            end if
            if (.not. allocated(node%where_body_indices)) then
                print *, "Body indices not allocated"
                stop 1
            end if
            if (size(node%where_body_indices) /= 2) then
                print *, "Wrong number of body indices"
                stop 1
            end if
        class default
            print *, "Wrong node type"
            stop 1
        end select
        
        print *, "  ✓ push_where (simple) successful"
    end subroutine test_push_where_simple

    subroutine test_push_where_with_elsewhere()
        type(ast_arena_t) :: arena
        integer :: mask_idx, else_mask_idx
        integer :: where_body(1), else_body(1), final_body(1)
        integer :: where_idx
        
        print *, "Testing push_where with ELSEWHERE..."
        
        arena = create_ast_arena()
        
        ! Create components
        mask_idx = push_identifier(arena, "x > 0")
        else_mask_idx = push_identifier(arena, "x < 0")
        where_body = [push_identifier(arena, "y = sqrt(x)")]
        else_body = [push_identifier(arena, "y = -sqrt(-x)")]
        final_body = [push_identifier(arena, "y = 0")]
        
        ! Push WHERE with simple elsewhere (backward compatibility)
        where_idx = push_where(arena, mask_idx, where_body, final_body)
        
        if (where_idx <= 0) then
            print *, "Failed to push WHERE with ELSEWHERE"
            stop 1
        end if
        
        ! Verify the node
        select type(node => arena%entries(where_idx)%node)
        type is (where_node)
            if (.not. allocated(node%elsewhere_clauses)) then
                print *, "ELSEWHERE clauses not allocated"
                stop 1
            end if
            if (size(node%elsewhere_clauses) /= 1) then
                print *, "Wrong number of ELSEWHERE clauses"
                stop 1
            end if
            if (node%elsewhere_clauses(1)%mask_index /= 0) then
                print *, "Simple ELSEWHERE should have mask_index = 0"
                stop 1
            end if
        class default
            print *, "Wrong node type"
            stop 1
        end select
        
        print *, "  ✓ push_where with ELSEWHERE successful"
    end subroutine test_push_where_with_elsewhere

    subroutine test_push_forall_simple()
        type(ast_arena_t) :: arena
        integer :: start_idx, end_idx, body_idx, forall_idx
        
        print *, "Testing push_forall (simple)..."
        
        arena = create_ast_arena()
        
        ! Create components
        start_idx = push_literal(arena, "1", 1)
        end_idx = push_identifier(arena, "n")
        body_idx = push_identifier(arena, "a(i) = 0")
        
        ! Push FORALL (step_index=0 for default stride)
        forall_idx = push_forall(arena, "i", start_idx, end_idx, 0, 0, [body_idx])
        
        if (forall_idx <= 0) then
            print *, "Failed to push FORALL node"
            stop 1
        end if
        
        ! Verify the node
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (node%num_indices /= 1) then
                print *, "Wrong number of indices"
                stop 1
            end if
            if (node%index_names(1) /= "i") then
                print *, "Wrong index name"
                stop 1
            end if
            if (node%lower_bound_indices(1) /= start_idx) then
                print *, "Wrong lower bound"
                stop 1
            end if
            if (node%upper_bound_indices(1) /= end_idx) then
                print *, "Wrong upper bound"
                stop 1
            end if
            if (node%stride_indices(1) /= 0) then
                print *, "Default stride should be 0"
                stop 1
            end if
            if (node%has_mask) then
                print *, "Should not have mask"
                stop 1
            end if
        class default
            print *, "Wrong node type"
            stop 1
        end select
        
        print *, "  ✓ push_forall (simple) successful"
    end subroutine test_push_forall_simple

    subroutine test_push_forall_with_mask()
        type(ast_arena_t) :: arena
        integer :: start_idx, end_idx, mask_idx, body_idx, forall_idx
        
        print *, "Testing push_forall with mask..."
        
        arena = create_ast_arena()
        
        ! Create components
        start_idx = push_literal(arena, "1", 1)
        end_idx = push_literal(arena, "10", 1)
        mask_idx = push_identifier(arena, "valid(i)")
        body_idx = push_identifier(arena, "process(i)")
        
        ! Push FORALL with mask (step_index=0 for default stride)
        forall_idx = push_forall(arena, "i", start_idx, end_idx, 0, mask_idx, [body_idx])
        
        if (forall_idx <= 0) then
            print *, "Failed to push FORALL with mask"
            stop 1
        end if
        
        ! Verify the node
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (.not. node%has_mask) then
                print *, "Should have mask"
                stop 1
            end if
            if (node%mask_expr_index /= mask_idx) then
                print *, "Wrong mask index"
                stop 1
            end if
        class default
            print *, "Wrong node type"
            stop 1
        end select
        
        print *, "  ✓ push_forall with mask successful"
    end subroutine test_push_forall_with_mask

    subroutine test_push_forall_with_stride()
        type(ast_arena_t) :: arena
        integer :: start_idx, end_idx, step_idx, body_idx, forall_idx
        
        print *, "Testing push_forall with stride..."
        
        arena = create_ast_arena()
        
        ! Create components
        start_idx = push_literal(arena, "1", 1)
        end_idx = push_literal(arena, "100", 1)
        step_idx = push_literal(arena, "2", 1)
        body_idx = push_identifier(arena, "even(i) = true")
        
        ! Push FORALL with stride (mask_index=0 for no mask)
        forall_idx = push_forall(arena, "i", start_idx, end_idx, step_idx, 0, [body_idx])
        
        if (forall_idx <= 0) then
            print *, "Failed to push FORALL with stride"
            stop 1
        end if
        
        ! Verify the node
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (node%stride_indices(1) /= step_idx) then
                print *, "Wrong stride index"
                stop 1
            end if
        class default
            print *, "Wrong node type"
            stop 1
        end select
        
        print *, "  ✓ push_forall with stride successful"
    end subroutine test_push_forall_with_stride

end module test_where_forall_factory

program test_driver
    use test_where_forall_factory
    implicit none
    
    call run_tests()
end program test_driver
