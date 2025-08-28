module ast_factory_arrays
    use ast_core
    use ast_factory_core, only: push_literal
    implicit none
    private

    ! Public array node creation functions
    public :: push_array_section, push_array_bounds, push_array_slice
    public :: push_range_expression

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
        write (start_str, '(I0)') start_idx
        write (end_str, '(I0)') end_idx

        ! Create start and end index literals
        start_literal_idx = push_literal(arena, trim(start_str), LITERAL_INTEGER, line, column)
        end_literal_idx = push_literal(arena, trim(end_str), LITERAL_INTEGER, line, column)

        ! Create subscript node with array section range
        section%name = array_name
        allocate (section%arg_indices(2))
        section%arg_indices(1) = start_literal_idx
        section%arg_indices(2) = end_literal_idx

        if (present(line)) section%line = line
        if (present(column)) section%column = column

        call arena%push(section, "call_or_subscript", parent_index)
        section_index = arena%size
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
        
        bounds = create_array_bounds(lower_index, upper_index, stride_index)
        if (present(line)) bounds%line = line
        if (present(column)) bounds%column = column
        
        call arena%push(bounds, "array_bounds", parent_index)
        bounds_index = arena%size
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
        
        slice = create_array_slice(array_index, bounds_indices, num_dims)
        if (present(line)) slice%line = line
        if (present(column)) slice%column = column
        
        call arena%push(slice, "array_slice", parent_index)
        slice_index = arena%size
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
        
        range = create_range_expression(start_index, end_index, stride_index)
        if (present(line)) range%line = line
        if (present(column)) range%column = column
        
        call arena%push(range, "range_expression", parent_index)
        range_index = arena%size
    end function push_range_expression

end module ast_factory_arrays