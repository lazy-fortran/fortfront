module test_semantic_where_forall
    use semantic_analyzer_with_checks
    use ast_core
    use ast_factory
    use ast_nodes_control
    use ast_nodes_data, only: INTENT_IN, INTENT_OUT, INTENT_INOUT
    implicit none

contains

    subroutine run_tests()
        print *, "=== Testing Semantic Analysis of WHERE/FORALL ===="
        print *, ""
        
        call test_semantic_where_with_intent()
        call test_semantic_forall_with_intent()
        call test_semantic_where_multiple_elsewhere()
        call test_semantic_forall_nested()
        
        print *, ""
        print *, "All semantic WHERE/FORALL tests passed!"
    end subroutine run_tests

    subroutine test_semantic_where_with_intent()
        type(ast_arena_t) :: arena
        type(program_node) :: prog
        type(subroutine_def_node) :: sub_node
        type(parameter_declaration_node) :: param1, param2
        type(where_node) :: where_stmt
        type(assignment_node) :: assign1, assign2
        integer :: prog_idx, sub_idx, param1_idx, param2_idx
        integer :: where_idx, assign1_idx, assign2_idx
        
        print *, "Testing semantic analysis of WHERE with INTENT..."
        
        arena = create_ast_arena()
        
        ! Create parameters with different intents
        param1%name = "input_array"
        param1%type_name = "real"
        param1%intent_type = INTENT_IN
        param1%is_array = .true.
        call arena%push(param1, "parameter_declaration")
        param1_idx = arena%size
        if (param1_idx <= 0) then
            print *, "ERROR: Failed to push param1 to arena"
            stop 1
        end if
        
        param2%name = "output_array"
        param2%type_name = "real"
        param2%intent_type = INTENT_OUT
        param2%is_array = .true.
        call arena%push(param2, "parameter_declaration")
        param2_idx = arena%size
        if (param2_idx <= 0) then
            print *, "ERROR: Failed to push param2 to arena"
            stop 1
        end if
        
        ! Create assignments inside WHERE
        assign1%target_index = push_identifier(arena, "output_array")
        assign1%value_index = push_identifier(arena, "input_array * 2")
        assign1%operator = "="
        call arena%push(assign1, "assignment")
        assign1_idx = arena%size
        if (assign1_idx <= 0) then
            print *, "ERROR: Failed to push assign1 to arena"
            stop 1
        end if
        
        assign2%target_index = push_identifier(arena, "output_array")
        assign2%value_index = push_identifier(arena, "0.0")
        assign2%operator = "="
        call arena%push(assign2, "assignment")
        assign2_idx = arena%size
        if (assign2_idx <= 0) then
            print *, "ERROR: Failed to push assign2 to arena"
            stop 1
        end if
        
        ! Create WHERE construct
        where_stmt%mask_expr_index = push_identifier(arena, "input_array > 0")
        allocate(where_stmt%where_body_indices(1))
        where_stmt%where_body_indices = [assign1_idx]
        
        ! Add ELSEWHERE clause
        allocate(where_stmt%elsewhere_clauses(1))
        where_stmt%elsewhere_clauses(1)%mask_index = 0  ! No mask
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(1))
        where_stmt%elsewhere_clauses(1)%body_indices = [assign2_idx]
        
        call arena%push(where_stmt, "where")
        where_idx = arena%size
        if (where_idx <= 0) then
            print *, "ERROR: Failed to push where_stmt to arena"
            stop 1
        end if
        
        ! Create subroutine
        sub_node%name = "process_arrays"
        allocate(sub_node%param_indices(2))
        sub_node%param_indices = [param1_idx, param2_idx]
        allocate(sub_node%body_indices(1))
        sub_node%body_indices = [where_idx]
        
        call arena%push(sub_node, "subroutine_def")
        sub_idx = arena%size
        if (sub_idx <= 0) then
            print *, "ERROR: Failed to push sub_node to arena"
            stop 1
        end if
        
        ! Create program
        allocate(prog%body_indices(1))
        prog%body_indices = [sub_idx]
        
        call arena%push(prog, "program")
        prog_idx = arena%size
        if (prog_idx <= 0) then
            print *, "ERROR: Failed to push prog to arena"
            stop 1
        end if
        
        ! Analyze - should pass without errors
        call analyze_program_with_checks(arena, prog_idx)
        
        print *, "  ✓ WHERE with INTENT analysis successful"
    end subroutine test_semantic_where_with_intent

    subroutine test_semantic_forall_with_intent()
        type(ast_arena_t) :: arena
        type(program_node) :: prog
        type(subroutine_def_node) :: sub_node
        type(parameter_declaration_node) :: param
        type(forall_node) :: forall_stmt
        type(assignment_node) :: assign
        integer :: prog_idx, sub_idx, param_idx
        integer :: forall_idx, assign_idx
        
        print *, "Testing semantic analysis of FORALL with INTENT..."
        
        arena = create_ast_arena()
        
        ! Create parameter
        param%name = "matrix"
        param%type_name = "real"
        param%intent_type = INTENT_INOUT
        param%is_array = .true.
        call arena%push(param, "parameter_declaration")
        param_idx = arena%size
        if (param_idx <= 0) then
            print *, "ERROR: Failed to push param to arena"
            stop 1
        end if
        
        ! Create assignment inside FORALL
        assign%target_index = push_identifier(arena, "matrix(i,j)")
        assign%value_index = push_identifier(arena, "matrix(j,i)")
        assign%operator = "="
        call arena%push(assign, "assignment")
        assign_idx = arena%size
        if (assign_idx <= 0) then
            print *, "ERROR: Failed to push assign to arena"
            stop 1
        end if
        
        ! Create FORALL construct
        forall_stmt%num_indices = 2
        allocate(character(len=1) :: forall_stmt%index_names(2))
        forall_stmt%index_names = ["i", "j"]
        
        allocate(forall_stmt%lower_bound_indices(2))
        allocate(forall_stmt%upper_bound_indices(2))
        allocate(forall_stmt%stride_indices(2))
        
        forall_stmt%lower_bound_indices = [push_literal(arena, "1", 1), &
                                           push_literal(arena, "1", 1)]
        forall_stmt%upper_bound_indices = [push_identifier(arena, "n"), &
                                           push_identifier(arena, "n")]
        forall_stmt%stride_indices = [0, 0]
        
        forall_stmt%has_mask = .true.
        forall_stmt%mask_expr_index = push_identifier(arena, "i < j")
        
        allocate(forall_stmt%body_indices(1))
        forall_stmt%body_indices = [assign_idx]
        
        call arena%push(forall_stmt, "forall")
        forall_idx = arena%size
        if (forall_idx <= 0) then
            print *, "ERROR: Failed to push forall_stmt to arena"
            stop 1
        end if
        
        ! Create subroutine
        sub_node%name = "transpose_upper"
        allocate(sub_node%param_indices(1))
        sub_node%param_indices = [param_idx]
        allocate(sub_node%body_indices(1))
        sub_node%body_indices = [forall_idx]
        
        call arena%push(sub_node, "subroutine_def")
        sub_idx = arena%size
        if (sub_idx <= 0) then
            print *, "ERROR: Failed to push sub_node to arena"
            stop 1
        end if
        
        ! Create program
        allocate(prog%body_indices(1))
        prog%body_indices = [sub_idx]
        
        call arena%push(prog, "program")
        prog_idx = arena%size
        if (prog_idx <= 0) then
            print *, "ERROR: Failed to push prog to arena"
            stop 1
        end if
        
        ! Analyze
        call analyze_program_with_checks(arena, prog_idx)
        
        print *, "  ✓ FORALL with INTENT analysis successful"
    end subroutine test_semantic_forall_with_intent

    subroutine test_semantic_where_multiple_elsewhere()
        type(ast_arena_t) :: arena
        type(program_node) :: prog
        type(where_node) :: where_stmt
        type(assignment_node) :: assign
        integer :: prog_idx, where_idx, i
        integer, allocatable :: assign_indices(:)
        
        print *, "Testing semantic analysis of WHERE with multiple ELSEWHERE..."
        
        arena = create_ast_arena()
        
        ! Create assignments for each branch
        allocate(assign_indices(5))
        do i = 1, 5
            assign%target_index = push_identifier(arena, "result")
            select case(i)
            case(1)
                assign%value_index = push_identifier(arena, "'hot'")
            case(2)
                assign%value_index = push_identifier(arena, "'cold'")
            case(3)
                assign%value_index = push_identifier(arena, "'warm'")
            case(4)
                assign%value_index = push_identifier(arena, "'cool'")
            case(5)
                assign%value_index = push_identifier(arena, "'moderate'")
            end select
            assign%operator = "="
            call arena%push(assign, "assignment")
            assign_indices(i) = arena%size
            if (assign_indices(i) <= 0) then
                print *, "ERROR: Failed to push assign to arena in loop"
                stop 1
            end if
        end do
        
        ! Create complex WHERE
        where_stmt%mask_expr_index = push_identifier(arena, "temp > 35")
        allocate(where_stmt%where_body_indices(1))
        where_stmt%where_body_indices = [assign_indices(1)]
        
        ! Create 4 ELSEWHERE clauses
        allocate(where_stmt%elsewhere_clauses(4))
        
        ! ELSEWHERE (temp < 0)
        where_stmt%elsewhere_clauses(1)%mask_index = push_identifier(arena, "temp < 0")
        allocate(where_stmt%elsewhere_clauses(1)%body_indices(1))
        where_stmt%elsewhere_clauses(1)%body_indices = [assign_indices(2)]
        
        ! ELSEWHERE (temp > 25)
        where_stmt%elsewhere_clauses(2)%mask_index = push_identifier(arena, "temp > 25")
        allocate(where_stmt%elsewhere_clauses(2)%body_indices(1))
        where_stmt%elsewhere_clauses(2)%body_indices = [assign_indices(3)]
        
        ! ELSEWHERE (temp < 15)
        where_stmt%elsewhere_clauses(3)%mask_index = push_identifier(arena, "temp < 15")
        allocate(where_stmt%elsewhere_clauses(3)%body_indices(1))
        where_stmt%elsewhere_clauses(3)%body_indices = [assign_indices(4)]
        
        ! Final ELSEWHERE (no mask)
        where_stmt%elsewhere_clauses(4)%mask_index = 0
        allocate(where_stmt%elsewhere_clauses(4)%body_indices(1))
        where_stmt%elsewhere_clauses(4)%body_indices = [assign_indices(5)]
        
        call arena%push(where_stmt, "where")
        where_idx = arena%size
        if (where_idx <= 0) then
            print *, "ERROR: Failed to push where_stmt to arena"
            stop 1
        end if
        
        ! Create program
        allocate(prog%body_indices(1))
        prog%body_indices = [where_idx]
        
        call arena%push(prog, "program")
        prog_idx = arena%size
        if (prog_idx <= 0) then
            print *, "ERROR: Failed to push prog to arena"
            stop 1
        end if
        
        ! Analyze
        call analyze_program_with_checks(arena, prog_idx)
        
        print *, "  ✓ WHERE with multiple ELSEWHERE analysis successful"
    end subroutine test_semantic_where_multiple_elsewhere

    subroutine test_semantic_forall_nested()
        type(ast_arena_t) :: arena
        type(program_node) :: prog
        type(forall_node) :: outer_forall, inner_forall
        type(assignment_node) :: assign
        integer :: prog_idx, outer_idx, inner_idx, assign_idx
        
        print *, "Testing semantic analysis of nested FORALL..."
        
        arena = create_ast_arena()
        
        ! Create assignment
        assign%target_index = push_identifier(arena, "tensor(i,j,k)")
        assign%value_index = push_identifier(arena, "i * j * k")
        assign%operator = "="
        call arena%push(assign, "assignment")
        assign_idx = arena%size
        if (assign_idx <= 0) then
            print *, "ERROR: Failed to push assign to arena"
            stop 1
        end if
        
        ! Create inner FORALL
        inner_forall%num_indices = 1
        allocate(character(len=1) :: inner_forall%index_names(1))
        inner_forall%index_names = ["k"]
        
        allocate(inner_forall%lower_bound_indices(1))
        allocate(inner_forall%upper_bound_indices(1))
        allocate(inner_forall%stride_indices(1))
        
        inner_forall%lower_bound_indices = [push_literal(arena, "1", 1)]
        inner_forall%upper_bound_indices = [push_identifier(arena, "nz")]
        inner_forall%stride_indices = [0]
        
        allocate(inner_forall%body_indices(1))
        inner_forall%body_indices = [assign_idx]
        
        call arena%push(inner_forall, "forall")
        inner_idx = arena%size
        if (inner_idx <= 0) then
            print *, "ERROR: Failed to push inner_forall to arena"
            stop 1
        end if
        
        ! Create outer FORALL
        outer_forall%num_indices = 2
        allocate(character(len=1) :: outer_forall%index_names(2))
        outer_forall%index_names = ["i", "j"]
        
        allocate(outer_forall%lower_bound_indices(2))
        allocate(outer_forall%upper_bound_indices(2))
        allocate(outer_forall%stride_indices(2))
        
        outer_forall%lower_bound_indices = [push_literal(arena, "1", 1), &
                                            push_literal(arena, "1", 1)]
        outer_forall%upper_bound_indices = [push_identifier(arena, "nx"), &
                                            push_identifier(arena, "ny")]
        outer_forall%stride_indices = [0, 0]
        
        allocate(outer_forall%body_indices(1))
        outer_forall%body_indices = [inner_idx]
        
        call arena%push(outer_forall, "forall")
        outer_idx = arena%size
        if (outer_idx <= 0) then
            print *, "ERROR: Failed to push outer_forall to arena"
            stop 1
        end if
        
        ! Create program
        allocate(prog%body_indices(1))
        prog%body_indices = [outer_idx]
        
        call arena%push(prog, "program")
        prog_idx = arena%size
        if (prog_idx <= 0) then
            print *, "ERROR: Failed to push prog to arena"
            stop 1
        end if
        
        ! Analyze
        call analyze_program_with_checks(arena, prog_idx)
        
        print *, "  ✓ Nested FORALL analysis successful"
    end subroutine test_semantic_forall_nested

end module test_semantic_where_forall

program test_driver
    use test_semantic_where_forall
    implicit none
    
    call run_tests()
end program test_driver