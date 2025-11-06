module semantic_array_slice
    use type_system_unified, only: mono_type_t, create_mono_type, TARRAY
    use type_array_safe, only: safe_extract_array_rank
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_bounds, only: array_slice_node, range_expression_node, &
                                array_bounds_node
    use semantic_function_helpers, only: get_type_lookup
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
        integer :: max_dims
        integer :: dims_to_process
        integer :: i
        integer :: bounds_idx
        logical :: is_range

        source_type = get_type_fn(arena, slice_node%array_index)
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
        keep_dim = .false.
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
            typ = create_mono_type(TARRAY, args=args)
            typ%size = 0
            typ%alloc_info%is_allocatable = .true.
            typ%alloc_info%needs_allocation_check = .true.
            typ%alloc_info%is_pointer = .false.
            typ%alloc_info%needs_allocatable_string = .false.
            deallocate (args)
        end do
    end function infer_array_slice_type

end module semantic_array_slice
