program test_frontend_analysis_cycle_guard
    use ast_arena_modern, only: ast_arena_t, create_ast_arena, destroy_ast_arena
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
        identifier_node
    use frontend_analysis_helpers, only: collect_assignment_from_node, &
        record_identifier_name
    implicit none

    call test_collect_assignment_uses_child_count()
    call test_record_identifier_cycle_guard()

    print *, 'PASS: frontend analysis guards cyclic child traversal'

contains

    subroutine test_collect_assignment_uses_child_count()
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        type(identifier_node) :: carrier_node
        type(assignment_node) :: assign_node
        character(len=64), allocatable :: names(:)
        integer :: id_idx, assign_idx, carrier_idx

        arena = create_ast_arena(4)

        id_node%name = 'x'
        call arena%push(id_node, 'identifier', 0)
        id_idx = arena%size

        assign_node%target_index = id_idx
        call arena%push(assign_node, 'assignment', 0)
        assign_idx = arena%size

        carrier_node%name = 'carrier'
        call arena%push(carrier_node, 'identifier', 0)
        carrier_idx = arena%size

        allocate (arena%entries(carrier_idx)%child_indices(2))
        arena%entries(carrier_idx)%child_indices = [assign_idx, carrier_idx]
        arena%entries(carrier_idx)%child_count = 1

        call collect_assignment_from_node(arena, carrier_idx, names, &
            skip_procedures=.false.)

        if (.not. allocated(names)) error stop 'assignment name not collected'
        if (size(names) /= 1) error stop 'unexpected assignment name count'
        if (trim(names(1)) /= 'x') error stop 'unexpected assignment name'

        call destroy_ast_arena(arena)
    end subroutine test_collect_assignment_uses_child_count

    subroutine test_record_identifier_cycle_guard()
        type(ast_arena_t) :: arena
        type(call_or_subscript_node) :: call_node
        character(len=64), allocatable :: names(:)
        integer :: call_idx

        arena = create_ast_arena(2)

        call_node%name = 'self'
        call arena%push(call_node, 'call_or_subscript', 0)
        call_idx = arena%size

        select type (node => arena%entries(call_idx)%node)
            type is (call_or_subscript_node)
            node%base_expr_index = call_idx
        end select

        call record_identifier_name(arena, call_idx, names)

        if (allocated(names)) error stop 'cycle should not collect names'

        call destroy_ast_arena(arena)
    end subroutine test_record_identifier_cycle_guard

end program test_frontend_analysis_cycle_guard
