module ast_factory_arrays
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_nodes_core, only: call_or_subscript_node
    use ast_nodes_bounds, only: array_bounds_node, array_slice_node, &
                                range_expression_node
    use ast_factory_core, only: push_literal
    use ast_base, only: LITERAL_INTEGER
    use uid_generator, only: generate_uid
    use string_utils_mod, only: int_to_string
    implicit none
    private

    ! Public array node creation functions
    public :: push_array_section, push_array_bounds, push_array_slice
    public :: push_range_expression, push_assumed_size_bounds, push_assumed_rank_bounds

contains

    function push_array_section(arena, array_name, start_idx, end_idx, &
                                line, column, parent_index) result(section_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: array_name
        integer, intent(in) :: start_idx, end_idx
        integer, intent(in), optional :: line, column, parent_index
        integer :: section_index
        type(call_or_subscript_node) :: section
        integer :: start_literal_idx, end_literal_idx
        character(len=20) :: start_str, end_str

        ! Convert indices to strings
        start_str = int_to_string(start_idx)
        end_str = int_to_string(end_idx)

        ! Create start and end index literals
        start_literal_idx = push_literal(arena, trim(start_str), LITERAL_INTEGER, &
                                         line, column)
        end_literal_idx = push_literal(arena, trim(end_str), LITERAL_INTEGER, &
                                       line, column)

        ! Create subscript node with array section range
        section%uid = generate_uid()
        section%name = array_name
        allocate (section%arg_indices(2))
        section%arg_indices(1) = start_literal_idx
        section%arg_indices(2) = end_literal_idx

        if (present(line)) section%line = line
        if (present(column)) section%column = column

        call arena%push(section, "call_or_subscript", parent_index)
        section_index = arena%size

        ! Link children to this parent for AST traversal
        call link_children_to_parent(arena, section_index, &
                                     [start_literal_idx, end_literal_idx])
    end function push_array_section

    ! Create array bounds node and add to stack
    function push_array_bounds(arena, lower_index, upper_index, stride_index, &
                               line, column, parent_index) result(bounds_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: lower_index, upper_index
        integer, intent(in), optional :: stride_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: bounds_index
        type(array_bounds_node) :: bounds

        bounds%uid = generate_uid()
        bounds%lower_bound_index = lower_index
        bounds%upper_bound_index = upper_index
        if (present(stride_index)) bounds%stride_index = stride_index
        if (present(line)) bounds%line = line
        if (present(column)) bounds%column = column

        call arena%push(bounds, "array_bounds", parent_index)
        bounds_index = arena%size

        ! Link children to this parent for AST traversal
        if (lower_index > 0) then
            call link_children_to_parent(arena, bounds_index, [lower_index])
        end if
        if (upper_index > 0) then
            call link_children_to_parent(arena, bounds_index, [upper_index])
        end if
        if (present(stride_index)) then
            if (stride_index > 0) then
                call link_children_to_parent(arena, bounds_index, [stride_index])
            end if
        end if
    end function push_array_bounds

    ! Create array slice node and add to stack
    function push_array_slice(arena, array_index, bounds_indices, num_dims, &
                              line, column, parent_index) result(slice_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: array_index
        integer, intent(in) :: bounds_indices(:)
        integer, intent(in) :: num_dims
        integer, intent(in), optional :: line, column, parent_index
        integer :: slice_index
        type(array_slice_node) :: slice
        integer :: slice_dims, i

        slice%uid = generate_uid()
        slice%array_index = array_index

        if (num_dims < 0) then
            slice%num_dimensions = 0
        else
            slice_dims = size(slice%bounds_indices)
            slice%num_dimensions = min(num_dims, slice_dims)
        end if

        do i = 1, slice%num_dimensions
            if (i <= size(bounds_indices)) then
                slice%bounds_indices(i) = bounds_indices(i)
            else
                slice%bounds_indices(i) = -1
            end if
        end do

        if (slice%num_dimensions < size(slice%bounds_indices)) then
            slice%bounds_indices(slice%num_dimensions + 1:) = -1
        end if
        if (present(line)) slice%line = line
        if (present(column)) slice%column = column

        call arena%push(slice, "array_slice", parent_index)
        slice_index = arena%size

        ! Link children to this parent for AST traversal
        if (array_index > 0) then
            call link_children_to_parent(arena, slice_index, [array_index])
        end if
        if (size(bounds_indices) > 0) then
            call link_children_to_parent(arena, slice_index, bounds_indices)
        end if
    end function push_array_slice

    ! Create range expression node and add to stack
    function push_range_expression(arena, start_index, end_index, stride_index, &
                                   line, column, parent_index) result(range_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: stride_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: range_index
        type(range_expression_node) :: range

        range%uid = generate_uid()
        range%start_index = start_index
        range%end_index = end_index
        if (present(stride_index)) range%stride_index = stride_index
        if (present(line)) range%line = line
        if (present(column)) range%column = column

        call arena%push(range, "range_expression", parent_index)
        range_index = arena%size

        ! Link children to this parent for AST traversal
        if (start_index > 0) then
            call link_children_to_parent(arena, range_index, [start_index])
        end if
        if (end_index > 0) then
            call link_children_to_parent(arena, range_index, [end_index])
        end if
        if (present(stride_index)) then
            if (stride_index > 0) then
                call link_children_to_parent(arena, range_index, [stride_index])
            end if
        end if
    end function push_range_expression

    ! Create assumed-size array bounds node and add to stack
    function push_assumed_size_bounds(arena, line, column, parent_index) &
        result(bounds_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: bounds_index
        type(array_bounds_node) :: bounds

        bounds%uid = generate_uid()
        bounds%lower_bound_index = 0
        bounds%upper_bound_index = 0
        bounds%stride_index = 0
        bounds%is_assumed_size = .true.
        if (present(line)) bounds%line = line
        if (present(column)) bounds%column = column

        call arena%push(bounds, "array_bounds", parent_index)
        bounds_index = arena%size
    end function push_assumed_size_bounds

    ! Create assumed-rank array bounds node (..) and add to stack
    ! Fortran 2018 feature for SELECT RANK construct
    function push_assumed_rank_bounds(arena, line, column, parent_index) &
        result(bounds_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: bounds_index
        type(array_bounds_node) :: bounds

        bounds%uid = generate_uid()
        bounds%lower_bound_index = 0
        bounds%upper_bound_index = 0
        bounds%stride_index = 0
        bounds%is_assumed_rank = .true.
        if (present(line)) bounds%line = line
        if (present(column)) bounds%column = column

        call arena%push(bounds, "array_bounds", parent_index)
        bounds_index = arena%size
    end function push_assumed_rank_bounds

end module ast_factory_arrays
