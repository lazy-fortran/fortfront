module semantic_array_slice
    use type_system_unified, only: mono_type_t, create_mono_type, TARRAY, TCHAR
    use type_array_safe, only: safe_extract_array_rank
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_bounds, only: array_slice_node, range_expression_node, &
        array_bounds_node
    use semantic_function_helpers, only: get_type_lookup
    use semantic_constant_values, only: get_constant_integer_value
    implicit none
    private

    public :: infer_array_slice_type

contains

    function infer_array_slice_type(arena, slice_node, get_type_fn) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t) :: typ
        type(mono_type_t) :: source_type
        type(mono_type_t) :: base_type
        type(mono_type_t), allocatable :: args(:)
        logical, allocatable :: keep_dim(:)
        integer, allocatable :: dim_lengths(:)
        integer :: max_dims
        integer :: dims_to_process
        integer :: i
        integer :: bounds_idx
        logical :: is_range
        integer :: substring_len
        integer :: slice_extent

        source_type = get_type_fn(arena, slice_node%array_index)
        if (source_type%kind == TCHAR) then
            typ = source_type
            substring_len = infer_character_substring_length(arena, slice_node, &
                source_type%size)
            if (substring_len > 0) typ%size = substring_len
            return
        end if

        if (source_type%kind /= TARRAY) then
            typ = source_type
            return
        end if

        call safe_extract_array_rank(source_type, max_dims, base_type)

        if (max_dims <= 0) then
            typ = source_type
            return
        end if

        allocate (keep_dim(max_dims))
        allocate (dim_lengths(max_dims))
        keep_dim = .false.
        dim_lengths = -1
        dims_to_process = min(max_dims, slice_node%num_dimensions)
        do i = 1, dims_to_process
            bounds_idx = slice_node%bounds_indices(i)
            is_range = .false.
            if (bounds_idx > 0 .and. bounds_idx <= arena%size) then
                if (allocated(arena%entries(bounds_idx)%node)) then
                    select type (bounds => arena%entries(bounds_idx)%node)
                        type is (range_expression_node)
                        is_range = .true.
                        type is (array_bounds_node)
                        is_range = .true.
                    end select
                end if
            end if
            keep_dim(i) = is_range
            if (is_range) then
                slice_extent = infer_slice_extent(arena, bounds_idx)
                dim_lengths(i) = slice_extent
            end if
        end do

        if (slice_node%num_dimensions < max_dims) then
            keep_dim(slice_node%num_dimensions + 1:max_dims) = .true.
        end if

        if (.not. any(keep_dim)) then
            typ = base_type
            return
        end if

        typ = base_type
        do i = max_dims, 1, -1
            if (.not. keep_dim(i)) cycle
            allocate (args(1))
            args(1) = typ
            if (dim_lengths(i) > 0) then
                typ = create_mono_type(TARRAY, args=args, &
                    array_size=dim_lengths(i))
                typ%alloc_info%is_allocatable = .false.
                typ%alloc_info%needs_allocation_check = .false.
            else
                typ = create_mono_type(TARRAY, args=args)
                typ%size = 0
                typ%alloc_info%is_allocatable = .true.
                typ%alloc_info%needs_allocation_check = .true.
            end if
            typ%alloc_info%is_pointer = .false.
            typ%alloc_info%needs_allocatable_string = .false.
            deallocate (args)
        end do

        if (allocated(dim_lengths)) deallocate (dim_lengths)
        if (allocated(keep_dim)) deallocate (keep_dim)
    end function infer_array_slice_type

    integer function infer_slice_extent(arena, bounds_index) result(extent)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: bounds_index
        integer :: start_idx
        integer :: end_idx
        integer :: stride_idx
        integer :: start_value
        integer :: end_value
        integer :: stride_value
        integer :: delta
        integer :: abs_delta
        integer :: abs_stride
        logical :: has_start
        logical :: has_end
        logical :: has_stride

        extent = -1
        if (bounds_index <= 0) return
        if (bounds_index > arena%size) return
        if (.not. allocated(arena%entries(bounds_index)%node)) return

        select type (bounds => arena%entries(bounds_index)%node)
            type is (range_expression_node)
            start_idx = bounds%start_index
            end_idx = bounds%end_index
            stride_idx = bounds%stride_index
            type is (array_bounds_node)
            start_idx = bounds%lower_bound_index
            end_idx = bounds%upper_bound_index
            stride_idx = bounds%stride_index
        class default
            return
        end select

        if (start_idx <= 0) return
        if (end_idx <= 0) return

        has_start = get_constant_integer_value(arena, start_idx, start_value)
        has_end = get_constant_integer_value(arena, end_idx, end_value)
        if (.not. has_start) return
        if (.not. has_end) return

        if (stride_idx > 0) then
            has_stride = get_constant_integer_value(arena, stride_idx, &
                stride_value)
            if (.not. has_stride) return
        else
            stride_value = 1
        end if

        if (stride_value == 0) return

        delta = end_value - start_value
        if ((delta >= 0 .and. stride_value < 0) .or. &
            (delta <= 0 .and. stride_value > 0)) return

        abs_delta = abs(delta)
        abs_stride = abs(stride_value)
        extent = abs_delta / abs_stride + 1
        if (extent <= 0) extent = -1
    end function infer_slice_extent

    integer function infer_character_substring_length(arena, slice_node, &
            base_length) result(len)
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        integer, intent(in) :: base_length
        integer :: bounds_idx
        integer :: start_expr_idx
        integer :: end_expr_idx
        integer :: start_value
        integer :: end_value
        logical :: has_start
        logical :: has_end

        len = -1
        if (slice_node%num_dimensions <= 0) return

        bounds_idx = slice_node%bounds_indices(1)
        if (.not. arena%has_node_at(bounds_idx)) return

        select type (bounds => arena%entries(bounds_idx)%node)
            type is (range_expression_node)
            start_expr_idx = bounds%start_index
            end_expr_idx = bounds%end_index
            type is (array_bounds_node)
            start_expr_idx = bounds%lower_bound_index
            end_expr_idx = bounds%upper_bound_index
        class default
            return
        end select

        if (start_expr_idx <= 0) then
            start_value = 1
            has_start = .true.
        else
            has_start = get_constant_integer_value(arena, start_expr_idx, &
                start_value)
        end if

        if (end_expr_idx <= 0) then
            if (base_length > 0) then
                end_value = base_length
                has_end = .true.
            else
                has_end = .false.
            end if
        else
            has_end = get_constant_integer_value(arena, end_expr_idx, end_value)
        end if

        if (.not. (has_start .and. has_end)) then
            len = -1
            return
        end if

        len = end_value - start_value + 1
        if (len <= 0) len = -1
    end function infer_character_substring_length

end module semantic_array_slice
