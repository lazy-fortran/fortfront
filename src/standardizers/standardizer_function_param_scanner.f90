module standardizer_function_param_scanner
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_bounds, only: array_bounds_node, array_slice_node
    use ast_nodes_core, only: call_or_subscript_node, component_access_node, &
                              identifier_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use standardizer_parameter, only: metadata_find_param, node_exists, &
                                      param_metadata_t
    implicit none
    private
    public :: analyze_parameter_usage
    public :: analyze_subroutine_parameter_usage
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

    subroutine analyze_subroutine_parameter_usage(arena, sub_def, metadata)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub_def
        type(param_metadata_t), intent(inout) :: metadata
        integer :: body_idx

        if (.not. allocated(sub_def%body_indices)) return
        do body_idx = 1, size(sub_def%body_indices)
            call scan_node(arena, sub_def%body_indices(body_idx), metadata)
        end do
    end subroutine analyze_subroutine_parameter_usage

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
        integer, allocatable :: target_params(:)
        integer, allocatable :: value_params(:)
        logical :: target_has_array

        call scan_optional_child(arena, node%target_index, metadata)
        call scan_optional_child(arena, node%value_index, metadata)

        call collect_param_indices(arena, node%target_index, metadata, &
                                   target_params)
        call collect_param_indices(arena, node%value_index, metadata, &
                                   value_params)

        target_has_array = has_array_parameter(metadata, target_params)
        if (target_has_array) then
            call mark_params_as_array(metadata, value_params)
        end if
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

    recursive subroutine scan_optional_child(arena, child_index, metadata)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: child_index
        type(param_metadata_t), intent(inout) :: metadata

        if (.not. node_exists(arena, child_index)) return
        call scan_node(arena, child_index, metadata)
    end subroutine scan_optional_child

    logical function is_array_intrinsic(name) result(is_array_fn)
        use lexer_core, only: to_lower
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(name))
        select case (lowered)
        case ("size", "lbound", "ubound", "shape", "allocated", &
              "maxloc", "minloc", "sum", "product", "maxval", "minval", &
              "any", "all", "count", "matmul", "transpose", "pack", &
              "unpack", "reshape", "spread", "merge", "cshift", "eoshift")
            is_array_fn = .true.
        case default
            is_array_fn = .false.
        end select
    end function is_array_intrinsic

    subroutine mark_intrinsic_array_args(arena, node, metadata)
        use ast_nodes_core, only: call_or_subscript_node, identifier_node
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        type(param_metadata_t), intent(inout) :: metadata
        integer :: first_arg_idx, param_idx
        character(len=:), allocatable :: arg_name

        if (.not. allocated(node%arg_indices)) return
        if (size(node%arg_indices) < 1) return

        first_arg_idx = node%arg_indices(1)
        if (.not. node_exists(arena, first_arg_idx)) return

        select type (arg => arena%entries(first_arg_idx)%node)
        type is (identifier_node)
            if (.not. allocated(arg%name)) return
            arg_name = trim(arg%name)
            param_idx = metadata_find_param(metadata, arg_name)
            if (param_idx > 0) then
                metadata%is_array(param_idx) = .true.
                if (metadata%rank(param_idx) < 1) metadata%rank(param_idx) = 1
            end if
        end select
    end subroutine mark_intrinsic_array_args

    subroutine handle_call_or_subscript(arena, node, metadata)
        use ast_nodes_core, only: call_or_subscript_node
        use lexer_core, only: to_lower
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

        ! Check if this is an array intrinsic call (fixes #2062)
        if (is_array_intrinsic(target_name)) then
            call mark_intrinsic_array_args(arena, node, metadata)
            return
        end if

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
        if (.not. arena%has_node_at(idx)) return

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
        if (.not. arena%has_node_at(node_index)) return
        count_children = arena%entries(node_index)%child_count
        if (count_children <= 0) return
        if (allocated(indices)) deallocate (indices)
        allocate (indices(count_children))
        indices = arena%entries(node_index)%child_indices(1:count_children)
    end function get_child_list

    subroutine collect_param_indices(arena, node_index, metadata, params)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(param_metadata_t), intent(in) :: metadata
        integer, allocatable, intent(out) :: params(:)

        allocate (params(0))
        call collect_recursive(node_index)

    contains

        recursive subroutine collect_recursive(current_index)
            use ast_nodes_core, only: identifier_node, call_or_subscript_node
            use ast_nodes_core, only: binary_op_node
            integer, intent(in) :: current_index
            integer :: param_idx
            integer :: j
            integer, allocatable :: children(:)

            if (.not. node_exists(arena, current_index)) return

            select type (node => arena%entries(current_index)%node)
            type is (identifier_node)
                if (allocated(node%name)) then
                    param_idx = metadata_find_param(metadata, trim(node%name))
                    if (param_idx > 0) call append_param(param_idx)
                end if
            type is (call_or_subscript_node)
                if (allocated(node%name)) then
                    param_idx = metadata_find_param(metadata, trim(node%name))
                    if (param_idx > 0) call append_param(param_idx)
                end if
                if (node%base_expr_index > 0) call collect_recursive( &
                    node%base_expr_index)
                if (allocated(node%arg_indices)) then
                    do j = 1, size(node%arg_indices)
                        call collect_recursive(node%arg_indices(j))
                    end do
                end if
            type is (binary_op_node)
                if (node%left_index > 0) call collect_recursive(node%left_index)
                if (node%right_index > 0) call collect_recursive(node%right_index)
            class default
                children = get_child_list(arena, current_index)
                if (allocated(children)) then
                    do j = 1, size(children)
                        call collect_recursive(children(j))
                    end do
                end if
            end select
        end subroutine collect_recursive

        subroutine append_param(param_idx)
            integer, intent(in) :: param_idx
            if (param_idx <= 0) return
            if (any(params == param_idx)) return
            call append_value(params, param_idx)
        end subroutine append_param

        subroutine append_value(values, new_value)
            integer, allocatable, intent(inout) :: values(:)
            integer, intent(in) :: new_value
            integer, allocatable :: tmp(:)
            integer :: current_size

            current_size = size(values)
            allocate (tmp(current_size + 1))
            if (current_size > 0) tmp(1:current_size) = values
            tmp(current_size + 1) = new_value
            call move_alloc(tmp, values)
        end subroutine append_value
    end subroutine collect_param_indices

    logical function has_array_parameter(metadata, param_indices)
        type(param_metadata_t), intent(in) :: metadata
        integer, intent(in) :: param_indices(:)
        integer :: i, idx

        has_array_parameter = .false.
        do i = 1, size(param_indices)
            idx = param_indices(i)
            if (idx <= 0) cycle
            if (metadata%is_array(idx)) then
                has_array_parameter = .true.
                return
            end if
            if (metadata%rank(idx) > 0) then
                has_array_parameter = .true.
                return
            end if
        end do
    end function has_array_parameter

    subroutine mark_params_as_array(metadata, param_indices)
        type(param_metadata_t), intent(inout) :: metadata
        integer, intent(in) :: param_indices(:)
        integer :: i, idx

        do i = 1, size(param_indices)
            idx = param_indices(i)
            if (idx <= 0) cycle
            metadata%is_array(idx) = .true.
            if (metadata%rank(idx) <= 0) metadata%rank(idx) = 1
        end do
    end subroutine mark_params_as_array

end module standardizer_function_param_scanner
