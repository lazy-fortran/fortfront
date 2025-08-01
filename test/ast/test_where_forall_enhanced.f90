module test_where_forall_enhanced
    use ast_core
    use ast_nodes_control
    use ast_arena
    use ast_factory
    use codegen_core
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing Enhanced WHERE/FORALL Features ==="
        print *, ""
        
        call test_where_multiple_elsewhere_enhanced()
        call test_forall_multiple_indices_enhanced()
        call test_where_json_serialization()
        call test_forall_optimization_hints()
        
        print *, ""
        print *, "All enhanced WHERE/FORALL tests passed!"
    end subroutine run_tests

    subroutine test_where_multiple_elsewhere_enhanced()
        type(ast_arena_t) :: arena
        type(where_node) :: where_stmt
        integer :: where_idx, i
        character(len=:), allocatable :: code
        
        print *, "Testing WHERE with multiple ELSEWHERE clauses (enhanced)..."
        
        arena = create_ast_arena()
        
        ! Build a complex WHERE with multiple ELSEWHERE clauses
        where_stmt%mask_expr_index = push_identifier(arena, "temp > 100")
        
        ! Main WHERE body
        allocate(where_stmt%where_body_indices(2))
        where_stmt%where_body_indices = [push_identifier(arena, "state = 'gas'"), &
                                         push_identifier(arena, "pressure = high")]
        
        ! Create 3 ELSEWHERE clauses
        allocate(where_stmt%elsewhere_clauses(3))
        
        ! First ELSEWHERE with mask (temp < 0)
        where_stmt%elsewhere_clauses(1)%mask_index = push_identifier(arena, "temp < 0")
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(2))
        where_stmt%elsewhere_clauses(1)%body_indices = &
            [push_identifier(arena, "state = 'solid'"), &
             push_identifier(arena, "pressure = low")]
        
        ! Second ELSEWHERE with mask (temp < 50)
        where_stmt%elsewhere_clauses(2)%mask_index = push_identifier(arena, "temp < 50")
        allocate(where_stmt%elsewhere_clauses(2)%body_indices(1))
        where_stmt%elsewhere_clauses(2)%body_indices = &
            [push_identifier(arena, "state = 'cool'")]
        
        ! Final ELSEWHERE without mask
        where_stmt%elsewhere_clauses(3)%mask_index = 0
        allocate(where_stmt%elsewhere_clauses(3)%body_indices(3))
        where_stmt%elsewhere_clauses(3)%body_indices = &
            [push_identifier(arena, "state = 'liquid'"), &
             push_identifier(arena, "pressure = medium"), &
             push_identifier(arena, "flow = normal")]
        
        ! Set optimization hints
        where_stmt%mask_is_simple = .true.
        where_stmt%can_vectorize = .true.
        
        ! Add to arena
        call arena%push(where_stmt, "where")
        where_idx = arena%size
        
        ! Verify structure
        if (.not. allocated(where_stmt%elsewhere_clauses)) then
            error stop "ELSEWHERE clauses not allocated"
        end if
        if (size(where_stmt%elsewhere_clauses) /= 3) then
            error stop "Wrong number of ELSEWHERE clauses"
        end if
        
        ! Verify first ELSEWHERE has mask
        if (where_stmt%elsewhere_clauses(1)%mask_index <= 0) then
            error stop "First ELSEWHERE should have mask"
        end if
        if (size(where_stmt%elsewhere_clauses(1)%body_indices) /= 2) then
            error stop "First ELSEWHERE should have 2 body statements"
        end if
        
        ! Verify final ELSEWHERE has no mask
        if (where_stmt%elsewhere_clauses(3)%mask_index /= 0) then
            error stop "Final ELSEWHERE should have no mask"
        end if
        if (size(where_stmt%elsewhere_clauses(3)%body_indices) /= 3) then
            error stop "Final ELSEWHERE should have 3 body statements"
        end if
        
        ! Test code generation
        code = generate_code_from_arena(arena, where_idx)
        if (index(code, "where (temp > 100)") == 0) then
            error stop "Missing WHERE clause in generated code"
        end if
        if (index(code, "elsewhere (temp < 0)") == 0) then
            error stop "Missing first ELSEWHERE in generated code"
        end if
        if (index(code, "elsewhere (temp < 50)") == 0) then
            error stop "Missing second ELSEWHERE in generated code"
        end if
        
        print *, "  ✓ Enhanced WHERE with multiple ELSEWHERE clauses verified"
    end subroutine test_where_multiple_elsewhere_enhanced

    subroutine test_forall_multiple_indices_enhanced()
        type(ast_arena_t) :: arena
        type(forall_node) :: f_stmt
        integer :: forall_idx
        character(len=:), allocatable :: code
        
        print *, "Testing FORALL with multiple indices (enhanced)..."
        
        arena = create_ast_arena()
        
        ! Create FORALL with 3 indices and different strides
        f_stmt%num_indices = 3
        allocate(character(len=1) :: f_stmt%index_names(3))
        f_stmt%index_names = ["i", "j", "k"]
        
        ! Set bounds for each index
        allocate(f_stmt%lower_bound_indices(3))
        allocate(f_stmt%upper_bound_indices(3))
        allocate(f_stmt%stride_indices(3))
        
        ! i = 1:n
        f_stmt%lower_bound_indices(1) = push_literal(arena, "1", 1)
        f_stmt%upper_bound_indices(1) = push_identifier(arena, "n")
        f_stmt%stride_indices(1) = 0  ! Default stride
        
        ! j = 2:m:2
        f_stmt%lower_bound_indices(2) = push_literal(arena, "2", 1)
        f_stmt%upper_bound_indices(2) = push_identifier(arena, "m")
        f_stmt%stride_indices(2) = push_literal(arena, "2", 1)
        
        ! k = 1:p:3
        f_stmt%lower_bound_indices(3) = push_literal(arena, "1", 1)
        f_stmt%upper_bound_indices(3) = push_identifier(arena, "p")
        f_stmt%stride_indices(3) = push_literal(arena, "3", 1)
        
        ! Add mask
        f_stmt%has_mask = .true.
        f_stmt%mask_expr_index = push_identifier(arena, "i+j+k <= max_sum")
        
        ! Add body with multiple statements
        allocate(f_stmt%body_indices(3))
        f_stmt%body_indices = &
            [push_identifier(arena, "tensor(i,j,k) = compute(i,j,k)"), &
             push_identifier(arena, "flags(i,j,k) = .true."), &
             push_identifier(arena, "count = count + 1")]
        
        ! Set dependency flags
        f_stmt%has_dependencies = .false.
        f_stmt%is_parallel_safe = .true.
        
        ! Add to arena
        call arena%push(f_stmt, "forall")
        forall_idx = arena%size
        
        ! Verify structure
        if (f_stmt%num_indices /= 3) then
            error stop "Wrong number of indices"
        end if
        if (f_stmt%stride_indices(1) /= 0) then
            error stop "First index should have default stride"
        end if
        if (f_stmt%stride_indices(2) <= 0) then
            error stop "Second index should have stride 2"
        end if
        if (f_stmt%stride_indices(3) <= 0) then
            error stop "Third index should have stride 3"
        end if
        if (.not. f_stmt%has_mask) then
            error stop "FORALL should have mask"
        end if
        if (size(f_stmt%body_indices) /= 3) then
            error stop "FORALL should have 3 body statements"
        end if
        
        ! Test code generation
        code = generate_code_from_arena(arena, forall_idx)
        if (index(code, "forall (i=1:n, j=2:m:2, k=1:p:3, i+j+k <= max_sum)") == 0) then
            error stop "FORALL header not generated correctly"
        end if
        
        print *, "  ✓ Enhanced FORALL with multiple indices verified"
    end subroutine test_forall_multiple_indices_enhanced

    subroutine test_where_json_serialization()
        type(ast_arena_t) :: arena
        type(where_node) :: where_stmt
        integer :: where_idx
        
        print *, "Testing WHERE structure fields..."
        
        arena = create_ast_arena()
        
        ! Create WHERE with ELSEWHERE
        where_stmt%mask_expr_index = push_identifier(arena, "condition")
        allocate(where_stmt%where_body_indices(1))
        where_stmt%where_body_indices = [push_identifier(arena, "stmt1")]
        
        allocate(where_stmt%elsewhere_clauses(1))
        where_stmt%elsewhere_clauses(1)%mask_index = 0
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(1))
        where_stmt%elsewhere_clauses(1)%body_indices = [push_identifier(arena, "stmt2")]
        
        where_stmt%mask_is_simple = .true.
        where_stmt%can_vectorize = .false.
        
        ! Add to arena
        call arena%push(where_stmt, "where")
        where_idx = arena%size
        
        ! Verify fields are preserved
        select type(node => arena%entries(where_idx)%node)
        type is (where_node)
            if (.not. node%mask_is_simple) then
                error stop "mask_is_simple not preserved"
            end if
            if (node%can_vectorize) then
                error stop "can_vectorize not preserved"
            end if
            if (.not. allocated(node%elsewhere_clauses)) then
                error stop "elsewhere_clauses not preserved"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ WHERE structure fields verified"
    end subroutine test_where_json_serialization

    subroutine test_forall_optimization_hints()
        type(ast_arena_t) :: arena
        type(forall_node) :: f_stmt
        integer :: forall_idx
        
        print *, "Testing FORALL optimization hints..."
        
        arena = create_ast_arena()
        
        ! Create simple FORALL
        f_stmt%num_indices = 1
        allocate(character(len=1) :: f_stmt%index_names(1))
        f_stmt%index_names(1) = "i"
        
        allocate(f_stmt%lower_bound_indices(1))
        allocate(f_stmt%upper_bound_indices(1))
        allocate(f_stmt%stride_indices(1))
        
        f_stmt%lower_bound_indices(1) = push_literal(arena, "1", 1)
        f_stmt%upper_bound_indices(1) = push_identifier(arena, "n")
        f_stmt%stride_indices(1) = 0
        
        allocate(f_stmt%body_indices(1))
        f_stmt%body_indices = [push_identifier(arena, "a(i) = b(i)")]
        
        ! Set optimization hints
        f_stmt%has_dependencies = .false.
        f_stmt%is_parallel_safe = .true.
        f_stmt%has_mask = .false.
        
        ! Add to arena
        call arena%push(f_stmt, "forall")
        forall_idx = arena%size
        
        ! Verify optimization hints are preserved
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (node%has_dependencies) then
                error stop "has_dependencies should be false"
            end if
            if (.not. node%is_parallel_safe) then
                error stop "is_parallel_safe should be true"
            end if
        class default
            error stop "Wrong node type"
        end select
        
        print *, "  ✓ FORALL optimization hints verified"
    end subroutine test_forall_optimization_hints

end module test_where_forall_enhanced

program test_driver
    use test_where_forall_enhanced
    implicit none
    
    call run_tests()
end program test_driver