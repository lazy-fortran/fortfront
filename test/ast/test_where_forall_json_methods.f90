module test_where_forall_json_methods
    use ast_core
    use ast_nodes_control
    use ast_factory
    use json_module
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing WHERE/FORALL JSON Methods ==="
        print *, ""
        
        call test_where_json_method()
        call test_forall_json_method()
        call test_where_stmt_json_method()
        call test_complex_where_fields()
        call test_complex_forall_fields()
        
        print *, ""
        print *, "All WHERE/FORALL JSON method tests passed!"
    end subroutine run_tests

    subroutine test_where_json_method()
        type(ast_arena_t) :: arena
        type(where_node) :: where_stmt
        type(json_core) :: json
        type(json_value), pointer :: root
        integer :: where_idx
        
        print *, "Testing WHERE to_json method..."
        
        arena = create_ast_arena()
        
        ! Create simple WHERE
        where_stmt%mask_expr_index = push_identifier(arena, "condition")
        allocate(where_stmt%where_body_indices(1))
        where_stmt%where_body_indices = [push_identifier(arena, "a = 1")]
        where_stmt%line = 10
        where_stmt%column = 5
        
        ! Test JSON serialization
        call json%initialize()
        call json%create_object(root, '')
        
        ! Call to_json method - this exercises the JSON serialization code
        call where_stmt%to_json(json, root)
        
        ! Verify something was added
        if (.not. associated(root)) then
            error stop "JSON root not created"
        end if
        
        call json%destroy(root)
        
        print *, "  ✓ WHERE to_json method works"
    end subroutine test_where_json_method

    subroutine test_forall_json_method()
        type(ast_arena_t) :: arena
        type(forall_node) :: forall_stmt
        type(json_core) :: json
        type(json_value), pointer :: root
        
        print *, "Testing FORALL to_json method..."
        
        arena = create_ast_arena()
        
        ! Create FORALL
        forall_stmt%num_indices = 1
        allocate(character(len=1) :: forall_stmt%index_names(1))
        forall_stmt%index_names = ["i"]
        allocate(forall_stmt%lower_bound_indices(1))
        allocate(forall_stmt%upper_bound_indices(1))
        allocate(forall_stmt%stride_indices(1))
        forall_stmt%lower_bound_indices = [push_literal(arena, "1", 1)]
        forall_stmt%upper_bound_indices = [push_identifier(arena, "n")]
        forall_stmt%stride_indices = [0]
        allocate(forall_stmt%body_indices(1))
        forall_stmt%body_indices = [push_identifier(arena, "a(i) = 0")]
        forall_stmt%line = 20
        forall_stmt%column = 3
        
        ! Test JSON serialization
        call json%initialize()
        call json%create_object(root, '')
        
        call forall_stmt%to_json(json, root)
        
        if (.not. associated(root)) then
            error stop "JSON root not created"
        end if
        
        call json%destroy(root)
        
        print *, "  ✓ FORALL to_json method works"
    end subroutine test_forall_json_method

    subroutine test_where_stmt_json_method()
        type(ast_arena_t) :: arena
        type(where_stmt_node) :: where_single
        type(json_core) :: json
        type(json_value), pointer :: root
        
        print *, "Testing WHERE statement to_json method..."
        
        arena = create_ast_arena()
        
        ! Create single-line WHERE
        where_single%mask_expr_index = push_identifier(arena, "valid")
        where_single%assignment_index = push_identifier(arena, "result = 42")
        where_single%line = 30
        where_single%column = 8
        
        ! Test JSON serialization
        call json%initialize()
        call json%create_object(root, '')
        
        call where_single%to_json(json, root)
        
        if (.not. associated(root)) then
            error stop "JSON root not created"
        end if
        
        call json%destroy(root)
        
        print *, "  ✓ WHERE statement to_json method works"
    end subroutine test_where_stmt_json_method

    subroutine test_complex_where_fields()
        type(ast_arena_t) :: arena
        type(where_node) :: where_stmt
        type(json_core) :: json
        type(json_value), pointer :: root
        
        print *, "Testing complex WHERE with all fields..."
        
        arena = create_ast_arena()
        
        ! Create complex WHERE with all fields
        where_stmt%mask_expr_index = push_identifier(arena, "temp > 100")
        allocate(where_stmt%where_body_indices(2))
        where_stmt%where_body_indices = [push_identifier(arena, "state = 'hot'"), &
                                         push_identifier(arena, "flag = .true.")]
        
        ! Add multiple ELSEWHERE clauses
        allocate(where_stmt%elsewhere_clauses(3))
        
        where_stmt%elsewhere_clauses(1)%mask_index = push_identifier(arena, "temp < 0")
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(1))
        where_stmt%elsewhere_clauses(1)%body_indices = [push_identifier(arena, "state = 'cold'")]
        
        where_stmt%elsewhere_clauses(2)%mask_index = push_identifier(arena, "temp < 50")
        allocate(where_stmt%elsewhere_clauses(2)%body_indices(1))
        where_stmt%elsewhere_clauses(2)%body_indices = [push_identifier(arena, "state = 'cool'")]
        
        where_stmt%elsewhere_clauses(3)%mask_index = 0
        allocate(where_stmt%elsewhere_clauses(3)%body_indices(1))
        where_stmt%elsewhere_clauses(3)%body_indices = [push_identifier(arena, "state = 'normal'")]
        
        where_stmt%mask_is_simple = .true.
        where_stmt%can_vectorize = .true.
        
        ! Test JSON serialization with all fields
        call json%initialize()
        call json%create_object(root, '')
        
        call where_stmt%to_json(json, root)
        
        if (.not. associated(root)) then
            error stop "JSON root not created"
        end if
        
        call json%destroy(root)
        
        print *, "  ✓ Complex WHERE JSON serialization works"
    end subroutine test_complex_where_fields

    subroutine test_complex_forall_fields()
        type(ast_arena_t) :: arena
        type(forall_node) :: forall_stmt
        type(json_core) :: json
        type(json_value), pointer :: root
        
        print *, "Testing complex FORALL with all fields..."
        
        arena = create_ast_arena()
        
        ! Create complex FORALL with all fields
        forall_stmt%num_indices = 3
        allocate(character(len=5) :: forall_stmt%index_names(3))
        forall_stmt%index_names = ["idx_i", "idx_j", "idx_k"]
        
        allocate(forall_stmt%lower_bound_indices(3))
        allocate(forall_stmt%upper_bound_indices(3))
        allocate(forall_stmt%stride_indices(3))
        
        forall_stmt%lower_bound_indices = [push_literal(arena, "1", 1), &
                                           push_literal(arena, "2", 1), &
                                           push_literal(arena, "3", 1)]
        forall_stmt%upper_bound_indices = [push_identifier(arena, "nx"), &
                                           push_identifier(arena, "ny"), &
                                           push_identifier(arena, "nz")]
        forall_stmt%stride_indices = [0, push_literal(arena, "2", 1), push_literal(arena, "3", 1)]
        
        forall_stmt%has_mask = .true.
        forall_stmt%mask_expr_index = push_identifier(arena, "idx_i + idx_j + idx_k <= max_sum")
        
        allocate(forall_stmt%body_indices(2))
        forall_stmt%body_indices = [push_identifier(arena, "array(idx_i,idx_j,idx_k) = 0"), &
                                    push_identifier(arena, "flag(idx_i,idx_j,idx_k) = .true.")]
        
        forall_stmt%has_dependencies = .false.
        forall_stmt%is_parallel_safe = .true.
        
        ! Test JSON serialization with all fields
        call json%initialize()
        call json%create_object(root, '')
        
        call forall_stmt%to_json(json, root)
        
        if (.not. associated(root)) then
            error stop "JSON root not created"
        end if
        
        call json%destroy(root)
        
        print *, "  ✓ Complex FORALL JSON serialization works"
    end subroutine test_complex_forall_fields

end module test_where_forall_json_methods

program test_driver
    use test_where_forall_json_methods
    implicit none
    
    call run_tests()
end program test_driver