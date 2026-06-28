module ast_node_counter_callbacks
    use fortfront, only: ast_arena_t, get_node_type_at, node_exists
    implicit none
contains
    subroutine count_callback(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: node_type

        if (.not. node_exists(arena, node_index)) return
        node_type = get_node_type_at(arena, node_index)
        print '(a,i0,a,a)', 'Node ', node_index, ': ', node_type
    end subroutine count_callback
end module ast_node_counter_callbacks

program ast_node_counter
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_node_counter_callbacks, only: count_callback
    use fortfront, only: ast_arena_t, create_ast_arena, &
        tooling_load_ast_from_string, traverse_ast
    implicit none

    type(ast_arena_t) :: arena
    integer :: root_index
    character(len=:), allocatable :: error_msg

    arena = create_ast_arena()
    call tooling_load_ast_from_string('x = 5 + 3', arena, root_index, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(a)') trim(error_msg)
        stop 1
    end if

    call traverse_ast(arena, root_index, count_callback)
end program ast_node_counter
