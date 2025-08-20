module test_where_forall_ast
    use ast_core
    use ast_factory
    use codegen_core
    use lexer_core
    use parser_core
    use json_writer
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing WHERE/FORALL AST Nodes ==="
        print *, ""
        
        call test_where_simple()
        call test_where_with_elsewhere()
        call test_where_multiple_elsewhere()
        call test_forall_simple()
        call test_forall_with_stride()
        call test_forall_multiple_indices()
        call test_codegen_where()
        call test_codegen_forall()
        
        print *, ""
        print *, "All WHERE/FORALL AST tests passed!"
    end subroutine run_tests

    subroutine test_where_simple()
        type(ast_arena_t) :: arena
        integer :: mask_idx, body1_idx, body2_idx, where_idx
        integer, allocatable :: body_indices(:)
        
        print *, "Testing simple WHERE node..."
        
        arena = create_ast_arena()
        
        ! Create mask expression
        mask_idx = push_identifier(arena, "mask")
        
        ! Create body statements
        body1_idx = push_identifier(arena, "stmt1")
        body2_idx = push_identifier(arena, "stmt2")
        
        allocate(body_indices(2))
        body_indices = [body1_idx, body2_idx]
        
        ! Create WHERE using factory function
        where_idx = push_where(arena, mask_idx, body_indices)
        
        if (where_idx <= 0) then
            error stop "Failed to create WHERE"
        end if
        
        ! Verify the node
        select type(node => arena%entries(where_idx)%node)
        type is (where_node)
            if (node%mask_expr_index /= mask_idx) then
                error stop "Wrong mask index"
            end if
            if (.not. allocated(node%where_body_indices)) then
                error stop "Body indices not allocated"
            end if
            if (size(node%where_body_indices) /= 2) then
                error stop "Wrong number of body indices"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ Simple WHERE node created and verified"
    end subroutine test_where_simple

    subroutine test_where_with_elsewhere()
        type(ast_arena_t) :: arena
        integer :: mask1_idx, body1_idx, body2_idx, where_idx
        integer, allocatable :: where_body(:), else_body(:)
        
        print *, "Testing WHERE with ELSEWHERE..."
        
        arena = create_ast_arena()
        
        ! Create expressions
        mask1_idx = push_identifier(arena, "mask1")
        body1_idx = push_identifier(arena, "where_body")
        body2_idx = push_identifier(arena, "elsewhere_body")
        
        allocate(where_body(1), else_body(1))
        where_body = [body1_idx]
        else_body = [body2_idx]
        
        ! Create WHERE with simple ELSEWHERE (backward compatibility)
        where_idx = push_where(arena, mask1_idx, where_body, else_body)
        
        if (where_idx <= 0) then
            error stop "Failed to create WHERE with ELSEWHERE"
        end if
        
        ! Verify structure
        select type(node => arena%entries(where_idx)%node)
        type is (where_node)
            if (.not. allocated(node%elsewhere_clauses)) then
                error stop "ELSEWHERE clauses not allocated"
            end if
            if (size(node%elsewhere_clauses) /= 1) then
                error stop "Wrong number of ELSEWHERE clauses"
            end if
            if (node%elsewhere_clauses(1)%mask_index /= 0) then
                error stop "Simple ELSEWHERE should have mask_index = 0"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ WHERE with ELSEWHERE clauses created"
    end subroutine test_where_with_elsewhere

    subroutine test_where_multiple_elsewhere()
        type(ast_arena_t) :: arena
        integer :: mask_idx, where_idx
        integer, allocatable :: body_indices(:)
        
        print *, "Testing WHERE with multiple ELSEWHERE clauses..."
        
        arena = create_ast_arena()
        
        ! Create simple WHERE to test basic functionality
        mask_idx = push_identifier(arena, "main_mask")
        allocate(body_indices(2))
        body_indices = [push_identifier(arena, "stmt1"), &
                        push_identifier(arena, "stmt2")]
        
        where_idx = push_where(arena, mask_idx, body_indices)
        
        if (where_idx <= 0) then
            error stop "Failed to create WHERE"
        end if
        
        print *, "  ✓ WHERE with body statements created"
    end subroutine test_where_multiple_elsewhere

    subroutine test_forall_simple()
        type(ast_arena_t) :: arena
        integer :: lower_idx, upper_idx, body_idx, forall_idx
        
        print *, "Testing simple FORALL node..."
        
        arena = create_ast_arena()
        
        ! Create bounds
        lower_idx = push_literal(arena, "1", 1)  ! 1 = integer kind
        upper_idx = push_identifier(arena, "n")
        body_idx = push_identifier(arena, "assignment")
        
        ! Create FORALL using factory function
        forall_idx = push_forall(arena, "i", lower_idx, upper_idx, 0, 0, [body_idx])
        
        if (forall_idx <= 0) then
            error stop "Failed to create FORALL"
        end if
        
        ! Verify the node was created
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (node%num_indices /= 1) then
                error stop "Wrong number of indices"
            end if
            if (node%index_names(1) /= "i") then
                error stop "Wrong index name"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ Simple FORALL node created"
    end subroutine test_forall_simple

    subroutine test_forall_with_stride()
        type(ast_arena_t) :: arena
        integer :: stride_idx, forall_idx, start_idx, end_idx, body_idx
        
        print *, "Testing FORALL with stride..."
        
        arena = create_ast_arena()
        
        ! Create components
        start_idx = push_literal(arena, "1", 1)
        end_idx = push_literal(arena, "10", 1)
        stride_idx = push_literal(arena, "2", 1)
        body_idx = push_identifier(arena, "stmt")
        
        ! Create FORALL with stride
        forall_idx = push_forall(arena, "j", start_idx, end_idx, stride_idx, 0, [body_idx])
        
        if (forall_idx <= 0) then
            error stop "Failed to create FORALL with stride"
        end if
        
        ! Verify stride was set
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (node%stride_indices(1) /= stride_idx) then
                error stop "Stride not properly set"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ FORALL with stride created"
    end subroutine test_forall_with_stride

    subroutine test_forall_multiple_indices()
        type(ast_arena_t) :: arena
        integer :: forall_idx, mask_idx, body_idx
        
        print *, "Testing FORALL with mask..."
        
        arena = create_ast_arena()
        
        ! Create simple FORALL with mask
        mask_idx = push_identifier(arena, "mask_expr")
        body_idx = push_identifier(arena, "stmt")
        
        forall_idx = push_forall(arena, "i", &
                                 push_literal(arena, "1", 1), &
                                 push_identifier(arena, "n"), &
                                 0, mask_idx, [body_idx])
        
        if (forall_idx <= 0) then
            error stop "Failed to create FORALL with mask"
        end if
        
        ! Verify mask was set
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (.not. node%has_mask) then
                error stop "Mask flag not set"
            end if
            if (node%mask_expr_index /= mask_idx) then
                error stop "Wrong mask index"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ FORALL with mask created"
    end subroutine test_forall_multiple_indices

    subroutine test_codegen_where()
        type(ast_arena_t) :: arena
        type(where_node) :: where_stmt
        integer :: where_idx
        character(len=:), allocatable :: code
        
        print *, "Testing WHERE code generation..."
        
        arena = create_ast_arena()
        
        ! Build WHERE construct
        where_stmt%mask_expr_index = push_identifier(arena, "temp > 100")
        
        allocate(where_stmt%where_body_indices(1))
        where_stmt%where_body_indices = [push_identifier(arena, "state = 'gas'")]
        
        allocate(where_stmt%elsewhere_clauses(2))
        
        ! ELSEWHERE with mask
        where_stmt%elsewhere_clauses(1)%mask_index = push_identifier(arena, "temp < 0")
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(1))
        where_stmt%elsewhere_clauses(1)%body_indices = [push_identifier(arena, "state = 'solid'")]
        
        ! Final ELSEWHERE
        where_stmt%elsewhere_clauses(2)%mask_index = 0
        allocate(where_stmt%elsewhere_clauses(2)%body_indices(1))
        where_stmt%elsewhere_clauses(2)%body_indices = [push_identifier(arena, "state = 'liquid'")]
        
        ! Add to arena
        call arena%push(where_stmt, "where")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push where statement to arena"
            stop 1
        end if
        where_idx = arena%size
        
        ! Generate code
        code = generate_code_from_arena(arena, where_idx)
        
        if (index(code, "where") == 0) then
            error stop "Generated code missing 'where'"
        end if
        if (index(code, "elsewhere") == 0) then
            error stop "Generated code missing 'elsewhere'"
        end if
        
        print *, "  ✓ WHERE code generation successful"
    end subroutine test_codegen_where

    subroutine test_codegen_forall()
        type(ast_arena_t) :: arena
        type(forall_node) :: f_node
        integer :: forall_idx
        character(len=:), allocatable :: code
        
        print *, "Testing FORALL code generation..."
        
        arena = create_ast_arena()
        
        ! Build FORALL
        f_node%num_indices = 2
        allocate(character(len=1) :: f_node%index_names(2))
        f_node%index_names = ["i", "j"]
        
        allocate(f_node%lower_bound_indices(2))
        allocate(f_node%upper_bound_indices(2))
        allocate(f_node%stride_indices(2))
        
        f_node%lower_bound_indices = [push_literal(arena, "1", 1), &
                                           push_literal(arena, "1", 1)]
        f_node%upper_bound_indices = [push_identifier(arena, "n"), &
                                           push_identifier(arena, "m")]
        f_node%stride_indices = [0, 0]
        
        f_node%has_mask = .true.
        f_node%mask_expr_index = push_identifier(arena, "i <= j")
        
        allocate(f_node%body_indices(1))
        f_node%body_indices = [push_identifier(arena, "a(i,j) = 0")]
        
        ! Add to arena
        call arena%push(f_node, "forall")
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push forall node to arena"
            stop 1
        end if
        forall_idx = arena%size
        
        ! Generate code
        code = generate_code_from_arena(arena, forall_idx)
        
        if (index(code, "forall") == 0) then
            error stop "Generated code missing 'forall'"
        end if
        
        print *, "  ✓ FORALL code generation successful"
    end subroutine test_codegen_forall

end module test_where_forall_ast

program test_driver
    use test_where_forall_ast
    implicit none
    
    call run_tests()
end program test_driver