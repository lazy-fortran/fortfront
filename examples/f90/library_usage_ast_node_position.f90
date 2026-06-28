program ast_node_position
    use fortfront, only: ast_arena_t, create_ast_arena, get_node_column, &
        get_node_line, get_node_location, &
        tooling_load_ast_from_string
    implicit none

    type(ast_arena_t) :: arena
    integer :: root_index
    integer :: line
    integer :: col
    character(len=:), allocatable :: error_msg

    arena = create_ast_arena()
    call tooling_load_ast_from_string('x = 5', arena, root_index, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        error stop error_msg
    end if

    line = get_node_line(arena, root_index)
    col = get_node_column(arena, root_index)
    print '(a,i0,a,i0)', 'Position: line ', line, ', column ', col

    call get_node_location(arena, root_index, line, col)
    print '(a,i0,a,i0)', 'Location:  line ', line, ', column ', col

    line = arena%get_node_line(root_index)
    col = arena%get_node_column(root_index)
    print '(a,i0,a,i0)', 'Bound:     line ', line, ', column ', col
end program ast_node_position
