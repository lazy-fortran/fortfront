! @slow-path
module variable_usage_expr_handlers_module
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_control_handlers_module, only: traversal_context_t, &
        push_node, validate_node_index
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
        identifier_node, component_access_node, &
        assignment_node, program_node, literal_node
    use ast_nodes_bounds, only: array_slice_node
    implicit none
    private

    ! Public procedures
    public :: process_binary_op_children, process_call_or_subscript_children
    public :: process_array_slice_children, process_component_access_children
    public :: process_assignment_node_children, process_program_node_children
    public :: process_literal_node_children

contains

    subroutine process_binary_op_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
            type is (binary_op_node)
            call push_node(ctx, node%left_index)
            call push_node(ctx, node%right_index)
        end select
    end subroutine process_binary_op_children

    subroutine process_call_or_subscript_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if

            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) <= 0) cycle
                    if (.not. allocated(arena%entries(node%arg_indices(i))%node)) cycle
                    select type (arg => arena%entries(node%arg_indices(i))%node)
                        type is (assignment_node)
                        call push_node(ctx, arg%value_index)
                    class default
                        call push_node(ctx, node%arg_indices(i))
                    end select
                end do
            end if
        end select
    end subroutine process_call_or_subscript_children

    subroutine process_array_slice_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
            type is (array_slice_node)
            call push_node(ctx, node%array_index)

            block
                integer :: i
                do i = 1, node%num_dimensions
                    call push_node(ctx, node%bounds_indices(i))
                end do
            end block
        end select
    end subroutine process_array_slice_children

    subroutine process_component_access_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
            type is (component_access_node)
            call push_node(ctx, node%base_expr_index)

            if (allocated(node%component_name)) then
                call add_string_to_info(node%component_name, node_index, info)
            end if
        end select
    end subroutine process_component_access_children

    subroutine process_assignment_node_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        if (.not. validate_node_index(arena, node_index)) return

        if (arena%entries(node_index)%node_type /= "assignment") then
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (assignment_node)
            if (node%is_keyword_argument) then
                call push_node(ctx, node%value_index)
                return
            end if

            call push_node(ctx, node%target_index)
            call push_node(ctx, node%value_index)
        end select
    end subroutine process_assignment_node_children

    subroutine process_program_node_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (program_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_program_node_children

    subroutine process_literal_node_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        ! Literals typically do not have child nodes with expressions
        ! They contain constant values, not variable references
    end subroutine process_literal_node_children

end module variable_usage_expr_handlers_module
