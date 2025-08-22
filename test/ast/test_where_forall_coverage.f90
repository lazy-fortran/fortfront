module test_where_forall_coverage
    use ast_core
    use ast_nodes_control
    use ast_factory
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing WHERE/FORALL Coverage ==="
        print *, ""
        
        call test_where_assignment_operator()
        call test_forall_assignment_operator()
        call test_where_stmt_assignment()
        call test_where_structure_preservation()
        call test_forall_structure_preservation()
        
        print *, ""
        print *, "All WHERE/FORALL coverage tests passed!"
    end subroutine run_tests

    subroutine test_where_assignment_operator()
        type(where_node) :: where1, where2
        
        print *, "Testing WHERE assignment operator..."
        
        ! Create a complex WHERE node
        where1%mask_expr_index = 42
        allocate(where1%where_body_indices(2))
        where1%where_body_indices = [1, 2]
        
        ! Add ELSEWHERE clauses
        allocate(where1%elsewhere_clauses(2))
        
        ! First ELSEWHERE with mask
        where1%elsewhere_clauses(1)%mask_index = 10
        allocate(where1%elsewhere_clauses(1)%body_indices(1))
        where1%elsewhere_clauses(1)%body_indices = [3]
        
        ! Second ELSEWHERE without mask
        where1%elsewhere_clauses(2)%mask_index = 0
        allocate(where1%elsewhere_clauses(2)%body_indices(2))
        where1%elsewhere_clauses(2)%body_indices = [4, 5]
        
        where1%mask_is_simple = .true.
        where1%can_vectorize = .false.
        where1%line = 100
        where1%column = 20
        
        ! Test assignment
        where2 = where1
        
        ! Verify all fields are copied correctly
        if (where2%mask_expr_index /= 42) then
            print *, "WHERE assignment: mask_expr_index not copied"
            stop 1
        end if
        if (.not. allocated(where2%where_body_indices)) then
            print *, "WHERE assignment: where_body_indices not allocated"
            stop 1
        end if
        if (size(where2%where_body_indices) /= 2) then
            print *, "WHERE assignment: where_body_indices size incorrect"
            stop 1
        end if
        if (where2%where_body_indices(1) /= 1 .or. where2%where_body_indices(2) /= 2) then
            print *, "WHERE assignment: where_body_indices values incorrect"
            stop 1
        end if
        if (.not. allocated(where2%elsewhere_clauses)) then
            print *, "WHERE assignment: elsewhere_clauses not allocated"
            stop 1
        end if
        if (size(where2%elsewhere_clauses) /= 2) then
            print *, "WHERE assignment: elsewhere_clauses size incorrect"
            stop 1
        end if
        if (where2%elsewhere_clauses(1)%mask_index /= 10) then
            print *, "WHERE assignment: elsewhere mask_index incorrect"
            stop 1
        end if
        if (.not. allocated(where2%elsewhere_clauses(2)%body_indices)) then
            print *, "WHERE assignment: elsewhere body_indices not allocated"
            stop 1
        end if
        if (size(where2%elsewhere_clauses(2)%body_indices) /= 2) then
            print *, "WHERE assignment: elsewhere body_indices size incorrect"
            stop 1
        end if
        if (.not. where2%mask_is_simple) then
            print *, "WHERE assignment: mask_is_simple not copied"
            stop 1
        end if
        if (where2%can_vectorize) then
            print *, "WHERE assignment: can_vectorize not copied"
            stop 1
        end if
        if (where2%line /= 100 .or. where2%column /= 20) then
            print *, "WHERE assignment: line/column not copied"
            stop 1
        end if
        
        print *, "  ✓ WHERE assignment operator successful"
    end subroutine test_where_assignment_operator

    subroutine test_forall_assignment_operator()
        type(forall_node) :: forall1, forall2
        
        print *, "Testing FORALL assignment operator..."
        
        ! Create a complex FORALL node
        forall1%num_indices = 3
        allocate(character(len=1) :: forall1%index_names(3))
        forall1%index_names = ["i", "j", "k"]
        
        allocate(forall1%lower_bound_indices(3))
        allocate(forall1%upper_bound_indices(3))
        allocate(forall1%stride_indices(3))
        
        forall1%lower_bound_indices = [10, 20, 30]
        forall1%upper_bound_indices = [100, 200, 300]
        forall1%stride_indices = [1, 2, 3]
        
        forall1%has_mask = .true.
        forall1%mask_expr_index = 50
        
        allocate(forall1%body_indices(2))
        forall1%body_indices = [60, 70]
        
        forall1%has_dependencies = .true.
        forall1%is_parallel_safe = .false.
        forall1%line = 50
        forall1%column = 10
        
        ! Test assignment
        forall2 = forall1
        
        ! Verify all fields are copied
        if (forall2%num_indices /= 3) then
            print *, "FORALL assignment: num_indices not copied"
            stop 1
        end if
        if (.not. allocated(forall2%index_names)) then
            print *, "FORALL assignment: index_names not allocated"
            stop 1
        end if
        if (any(forall2%index_names /= ["i", "j", "k"])) then
            print *, "FORALL assignment: index_names values incorrect"
            stop 1
        end if
        if (.not. allocated(forall2%lower_bound_indices)) then
            print *, "FORALL assignment: lower_bound_indices not allocated"
            stop 1
        end if
        if (any(forall2%lower_bound_indices /= [10, 20, 30])) then
            print *, "FORALL assignment: lower_bound_indices values incorrect"
            stop 1
        end if
        if (.not. allocated(forall2%upper_bound_indices)) then
            print *, "FORALL assignment: upper_bound_indices not allocated"
            stop 1
        end if
        if (any(forall2%upper_bound_indices /= [100, 200, 300])) then
            print *, "FORALL assignment: upper_bound_indices values incorrect"
            stop 1
        end if
        if (.not. allocated(forall2%stride_indices)) then
            print *, "FORALL assignment: stride_indices not allocated"
            stop 1
        end if
        if (any(forall2%stride_indices /= [1, 2, 3])) then
            print *, "FORALL assignment: stride_indices values incorrect"
            stop 1
        end if
        if (.not. forall2%has_mask) then
            print *, "FORALL assignment: has_mask not copied"
            stop 1
        end if
        if (forall2%mask_expr_index /= 50) then
            print *, "FORALL assignment: mask_expr_index not copied"
            stop 1
        end if
        if (.not. allocated(forall2%body_indices)) then
            print *, "FORALL assignment: body_indices not allocated"
            stop 1
        end if
        if (size(forall2%body_indices) /= 2) then
            print *, "FORALL assignment: body_indices size incorrect"
            stop 1
        end if
        if (.not. forall2%has_dependencies) then
            print *, "FORALL assignment: has_dependencies not copied"
            stop 1
        end if
        if (forall2%is_parallel_safe) then
            print *, "FORALL assignment: is_parallel_safe not copied"
            stop 1
        end if
        
        print *, "  ✓ FORALL assignment operator successful"
    end subroutine test_forall_assignment_operator

    subroutine test_where_stmt_assignment()
        type(where_stmt_node) :: ws1, ws2
        
        print *, "Testing WHERE statement assignment..."
        
        ws1%mask_expr_index = 100
        ws1%assignment_index = 200
        ws1%line = 10
        ws1%column = 20
        
        ws2 = ws1
        
        if (ws2%mask_expr_index /= 100) then
            print *, "WHERE stmt assignment: mask_expr_index not copied"
            stop 1
        end if
        if (ws2%assignment_index /= 200) then
            print *, "WHERE stmt assignment: assignment_index not copied"
            stop 1
        end if
        if (ws2%line /= 10 .or. ws2%column /= 20) then
            print *, "WHERE stmt assignment: line/column not copied"
            stop 1
        end if
        
        print *, "  ✓ WHERE statement assignment successful"
    end subroutine test_where_stmt_assignment

    subroutine test_where_structure_preservation()
        type(ast_arena_t) :: arena
        type(where_node) :: where_stmt
        integer :: where_idx
        
        print *, "Testing WHERE structure preservation in arena..."
        
        arena = create_ast_arena()
        
        ! Create WHERE with all features
        where_stmt%mask_expr_index = push_identifier(arena, "mask")
        allocate(where_stmt%where_body_indices(1))
        where_stmt%where_body_indices = [push_identifier(arena, "stmt1")]
        
        allocate(where_stmt%elsewhere_clauses(1))
        where_stmt%elsewhere_clauses(1)%mask_index = 0
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(1))
        where_stmt%elsewhere_clauses(1)%body_indices = [push_identifier(arena, "stmt2")]
        
        where_stmt%mask_is_simple = .true.
        where_stmt%can_vectorize = .true.
        
        ! Push to arena
        call arena%push(where_stmt, "where")
        where_idx = arena%size
        
        ! Verify preservation
        select type(node => arena%entries(where_idx)%node)
        type is (where_node)
            if (.not. node%mask_is_simple) then
                print *, "mask_is_simple not preserved in arena"
                stop 1
            end if
            if (.not. node%can_vectorize) then
                print *, "can_vectorize not preserved in arena"
                stop 1
            end if
        class default
            print *, "Wrong node type in arena"
            stop 1
        end select
        
        print *, "  ✓ WHERE structure preservation successful"
    end subroutine test_where_structure_preservation

    subroutine test_forall_structure_preservation()
        type(ast_arena_t) :: arena
        type(forall_node) :: forall_stmt
        integer :: forall_idx
        
        print *, "Testing FORALL structure preservation in arena..."
        
        arena = create_ast_arena()
        
        ! Create FORALL with optimization hints
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
        
        forall_stmt%has_dependencies = .false.
        forall_stmt%is_parallel_safe = .true.
        
        ! Push to arena
        call arena%push(forall_stmt, "forall")
        forall_idx = arena%size
        
        ! Verify preservation
        select type(node => arena%entries(forall_idx)%node)
        type is (forall_node)
            if (node%has_dependencies) then
                print *, "has_dependencies not preserved in arena"
                stop 1
            end if
            if (.not. node%is_parallel_safe) then
                print *, "is_parallel_safe not preserved in arena"
                stop 1
            end if
        class default
            print *, "Wrong node type in arena"
            stop 1
        end select
        
        print *, "  ✓ FORALL structure preservation successful"
    end subroutine test_forall_structure_preservation

end module test_where_forall_coverage

program test_driver
    use test_where_forall_coverage
    implicit none
    
    call run_tests()
end program test_driver
