module ast_traversal_gather
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_control, only: if_node, select_case_node
    use ast_nodes_core, only: assignment_node, binary_op_node, &
                              call_or_subscript_node, program_node
    use ast_nodes_data, only: derived_type_node, module_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
                                   subroutine_def_node
    implicit none
    private

    public :: gather_child_indices

contains

    subroutine gather_child_indices(arena, node_index, children)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: children(:)

        integer, allocatable :: buffer(:)
        integer :: count

        count = 0
        if (.not. arena%has_node_at(node_index)) then
            allocate (children(0))
            return
        end if

        select type (node => arena%entries(node_index)%node)
        class is (ast_node)
            call append_children_for_node(node, buffer, count)
        end select

        if (count == 0) then
            if (allocated(buffer)) deallocate (buffer)
            allocate (children(0))
            return
        end if

        allocate (children(count))
        children = buffer(1:count)
        if (allocated(buffer)) deallocate (buffer)
    end subroutine gather_child_indices

    subroutine ensure_child_buffer_capacity(buffer, count, required)
        integer, allocatable, intent(inout) :: buffer(:)
        integer, intent(in) :: count
        integer, intent(in) :: required
        integer, allocatable :: tmp(:)
        integer :: current_size

        if (.not. allocated(buffer)) then
            allocate (buffer(max(32, required)))
            return
        end if

        current_size = size(buffer)
        if (required <= current_size) return

        allocate (tmp(max(current_size * 2, required)))
        if (count > 0) tmp(1:count) = buffer(1:count)
        call move_alloc(tmp, buffer)
    end subroutine ensure_child_buffer_capacity

    subroutine append_child_index(buffer, count, idx)
        integer, allocatable, intent(inout) :: buffer(:)
        integer, intent(inout) :: count
        integer, intent(in) :: idx

        if (idx <= 0) return
        call ensure_child_buffer_capacity(buffer, count, count + 1)
        count = count + 1
        buffer(count) = idx
    end subroutine append_child_index

    subroutine append_child_array(buffer, count, values)
        integer, allocatable, intent(inout) :: buffer(:)
        integer, intent(inout) :: count
        integer, intent(in) :: values(:)
        integer :: k

        if (size(values) <= 0) return
        call ensure_child_buffer_capacity(buffer, count, count + size(values))
        do k = 1, size(values)
            if (values(k) > 0) then
                count = count + 1
                buffer(count) = values(k)
            end if
        end do
    end subroutine append_child_array

    subroutine append_if_node_children(node, buffer, count)
        type(if_node), intent(in) :: node
        integer, allocatable, intent(inout) :: buffer(:)
        integer, intent(inout) :: count
        integer :: i

        call append_child_index(buffer, count, node%condition_index)
        if (allocated(node%then_body_indices)) then
            call append_child_array(buffer, count, node%then_body_indices)
        end if

        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                call append_child_index(buffer, count, &
                                        node%elseif_blocks(i)%condition_index)
                if (allocated(node%elseif_blocks(i)%body_indices)) then
                    call append_child_array(buffer, count, &
                                            node%elseif_blocks(i)%body_indices)
                end if
            end do
        end if

        if (allocated(node%else_body_indices)) then
            call append_child_array(buffer, count, node%else_body_indices)
        end if
    end subroutine append_if_node_children

    subroutine append_children_for_node(node, buffer, count)
        class(ast_node), intent(in) :: node
        integer, allocatable, intent(inout) :: buffer(:)
        integer, intent(inout) :: count

        select type (node)
        type is (program_node)
            if (allocated(node%body_indices)) then
                call append_child_array(buffer, count, node%body_indices)
            end if

        type is (assignment_node)
            call append_child_index(buffer, count, node%target_index)
            call append_child_index(buffer, count, node%value_index)

        type is (binary_op_node)
            call append_child_index(buffer, count, node%left_index)
            call append_child_index(buffer, count, node%right_index)

        type is (function_def_node)
            if (allocated(node%param_indices)) then
                call append_child_array(buffer, count, node%param_indices)
            end if
            if (allocated(node%body_indices)) then
                call append_child_array(buffer, count, node%body_indices)
            end if

        type is (subroutine_def_node)
            if (allocated(node%param_indices)) then
                call append_child_array(buffer, count, node%param_indices)
            end if
            if (allocated(node%body_indices)) then
                call append_child_array(buffer, count, node%body_indices)
            end if

        type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) then
                call append_child_array(buffer, count, node%arg_indices)
            end if

        type is (subroutine_call_node)
            if (allocated(node%arg_indices)) then
                call append_child_array(buffer, count, node%arg_indices)
            end if

        type is (if_node)
            call append_if_node_children(node, buffer, count)

        type is (do_loop_node)
            call append_child_index(buffer, count, node%start_expr_index)
            call append_child_index(buffer, count, node%end_expr_index)
            call append_child_index(buffer, count, node%step_expr_index)
            if (allocated(node%body_indices)) then
                call append_child_array(buffer, count, node%body_indices)
            end if

        type is (do_while_node)
            call append_child_index(buffer, count, node%condition_index)
            if (allocated(node%body_indices)) then
                call append_child_array(buffer, count, node%body_indices)
            end if

        type is (select_case_node)
            call append_child_index(buffer, count, node%selector_index)
            if (allocated(node%case_indices)) then
                call append_child_array(buffer, count, node%case_indices)
            end if
            call append_child_index(buffer, count, node%default_index)

        type is (module_node)
            if (allocated(node%declaration_indices)) then
                call append_child_array(buffer, count, node%declaration_indices)
            end if
            if (allocated(node%procedure_indices)) then
                call append_child_array(buffer, count, node%procedure_indices)
            end if

        type is (derived_type_node)
            if (allocated(node%component_indices)) then
                call append_child_array(buffer, count, node%component_indices)
            end if

        type is (interface_block_node)
            if (allocated(node%procedure_indices)) then
                call append_child_array(buffer, count, node%procedure_indices)
            end if

        type is (print_statement_node)
            if (allocated(node%expression_indices)) then
                call append_child_array(buffer, count, node%expression_indices)
            end if

        class default
            ! Other node types intentionally yield no children here
        end select
    end subroutine append_children_for_node

end module ast_traversal_gather
