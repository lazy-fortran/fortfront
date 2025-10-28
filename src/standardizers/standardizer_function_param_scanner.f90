module standardizer_function_param_scanner
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_bounds, only: array_bounds_node, array_slice_node
    use ast_nodes_core, only: call_or_subscript_node, component_access_node, &
                              identifier_node
    use ast_nodes_procedure, only: function_def_node
    use standardizer_parameter, only: metadata_find_param, node_exists, &
                                      param_metadata_t
    implicit none
    private
    public :: analyze_parameter_usage
contains

    subroutine analyze_parameter_usage(arena, func_def, metadata)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_def
        type(param_metadata_t), intent(inout) :: metadata
        integer :: body_idx

        if (.not. allocated(func_def%body_indices)) return
        do body_idx = 1, size(func_def%body_indices)
            call scan_node(arena, func_def%body_indices(body_idx), metadata)
        end do
    end subroutine analyze_parameter_usage

    recursive subroutine scan_node(arena, node_index, metadata)
        use ast_nodes_core, only: assignment_node, binary_op_node
        use ast_nodes_core, only: call_or_subscript_node, component_access_node
        use ast_nodes_bounds, only: array_slice_node, array_bounds_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(param_metadata_t), intent(inout) :: metadata

        if (.not. node_exists(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            call scan_assignment_children(arena, node, metadata)
        type is (binary_op_node)
            call scan_binary_children(arena, node, metadata)
        type is (call_or_subscript_node)
            call handle_call_or_subscript(arena, node, metadata)
            call scan_optional_child(arena, node%base_expr_index, metadata)
            call scan_argument_list(arena, node, metadata)
        type is (component_access_node)
            call scan_optional_child(arena, node%base_expr_index, metadata)
        type is (array_slice_node)
            call scan_array_slice_children(arena, node, metadata)
        type is (array_bounds_node)
            call scan_array_bounds_children(arena, node, metadata)
        class default
            call scan_generic_children(arena, node_index, metadata)
        end select
    end subroutine scan_node

    subroutine scan_assignment_children(arena, node, metadata)
        use ast_nodes_core, only: assignment_node
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata

        call scan_optional_child(arena, node%target_index, metadata)
        call scan_optional_child(arena, node%value_index, metadata)
    end subroutine scan_assignment_children

    subroutine scan_binary_children(arena, node, metadata)
        use ast_nodes_core, only: binary_op_node
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata

        call scan_optional_child(arena, node%left_index, metadata)
        call scan_optional_child(arena, node%right_index, metadata)
    end subroutine scan_binary_children

    subroutine scan_argument_list(arena, node, metadata)
        use ast_nodes_core, only: call_or_subscript_node
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        integer :: j

        if (.not. allocated(node%arg_indices)) return
        do j = 1, size(node%arg_indices)
            call scan_optional_child(arena, node%arg_indices(j), metadata)
        end do
    end subroutine scan_argument_list

    subroutine scan_array_slice_children(arena, node, metadata)
        use ast_nodes_bounds, only: array_slice_node
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        integer :: j

        call scan_optional_child(arena, node%array_index, metadata)
        do j = 1, node%num_dimensions
            call scan_optional_child(arena, node%bounds_indices(j), metadata)
        end do
    end subroutine scan_array_slice_children

    subroutine scan_array_bounds_children(arena, node, metadata)
        use ast_nodes_bounds, only: array_bounds_node
        type(ast_arena_t), intent(in) :: arena
        type(array_bounds_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata

        call scan_optional_child(arena, node%lower_bound_index, metadata)
        call scan_optional_child(arena, node%upper_bound_index, metadata)
        call scan_optional_child(arena, node%stride_index, metadata)
    end subroutine scan_array_bounds_children

    subroutine scan_generic_children(arena, node_index, metadata)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(param_metadata_t), intent(inout) :: metadata
        integer, allocatable :: child_indices(:)
        integer :: j

        child_indices = get_child_list(arena, node_index)
        if (.not. allocated(child_indices)) return
        do j = 1, size(child_indices)
            call scan_optional_child(arena, child_indices(j), metadata)
        end do
    end subroutine scan_generic_children

    subroutine scan_optional_child(arena, child_index, metadata)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: child_index
        type(param_metadata_t), intent(inout) :: metadata

        if (.not. node_exists(arena, child_index)) return
        call scan_node(arena, child_index, metadata)
    end subroutine scan_optional_child

    subroutine handle_call_or_subscript(arena, node, metadata)
        use ast_nodes_core, only: call_or_subscript_node
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        character(len=:), allocatable :: target_name
        integer :: idx
        integer :: rank_size

        target_name = ""
        if (allocated(node%name)) target_name = trim(node%name)
        if (len_trim(target_name) == 0) then
            if (node%base_expr_index > 0) then
                target_name = resolve_name_from_index(arena, node%base_expr_index)
            end if
        end if
        if (len_trim(target_name) == 0) return
        idx = metadata_find_param(metadata, target_name)
        if (idx <= 0) return

        metadata%is_array(idx) = .true.
        if (allocated(node%arg_indices)) then
            rank_size = count(node%arg_indices > 0)
        else
            rank_size = 0
        end if
        if (rank_size <= 0) rank_size = 1
        if (rank_size > metadata%rank(idx)) metadata%rank(idx) = rank_size
    end subroutine handle_call_or_subscript

    recursive function resolve_name_from_index(arena, idx) result(name)
        use ast_nodes_core, only: call_or_subscript_node, component_access_node
        use ast_nodes_core, only: identifier_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable :: name

        name = ""
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (base => arena%entries(idx)%node)
        type is (identifier_node)
            if (allocated(base%name)) name = trim(base%name)
        type is (call_or_subscript_node)
            if (allocated(base%name)) then
                name = trim(base%name)
            else if (base%base_expr_index > 0) then
                name = resolve_name_from_index(arena, base%base_expr_index)
            end if
        type is (component_access_node)
            name = resolve_name_from_index(arena, base%base_expr_index)
        class default
            name = ""
        end select
    end function resolve_name_from_index

    function get_child_list(arena, node_index) result(indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: indices(:)
        integer :: count_children

        allocate (indices(0))
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        count_children = arena%entries(node_index)%child_count
        if (count_children <= 0) return
        if (allocated(indices)) deallocate (indices)
        allocate (indices(count_children))
        indices = arena%entries(node_index)%child_indices(1:count_children)
    end function get_child_list

end module standardizer_function_param_scanner
