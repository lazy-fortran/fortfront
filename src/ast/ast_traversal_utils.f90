module ast_traversal_utils
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    implicit none
    private

    public :: find_nodes_by_type
    public :: get_ancestor_of_type
    public :: has_child_of_type
    public :: get_children
    public :: traverse_ast

    abstract interface
        subroutine traverse_callback(arena, node_index, user_data)
            import :: ast_arena_t
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
            class(*), intent(inout), optional :: user_data
        end subroutine traverse_callback
    end interface

    public :: traverse_callback

    interface find_nodes_by_type
        module procedure find_nodes_by_type_impl
    end interface find_nodes_by_type

    interface get_children
        module procedure get_children_impl
    end interface get_children

contains

    function find_nodes_by_type_impl(arena, root_index, node_type_name) &
        result(node_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=*), intent(in) :: node_type_name
        integer, allocatable :: node_indices(:)
        integer, allocatable :: temp_indices(:)
        integer :: count, i
        integer, allocatable :: children(:)

        count = 0
        allocate (temp_indices(arena%compat_size))

        call collect_matching_nodes(arena, root_index, node_type_name, &
                                    temp_indices, count)

        allocate (node_indices(count))
        do i = 1, count
            node_indices(i) = temp_indices(i)
        end do
    end function find_nodes_by_type_impl

    recursive subroutine collect_matching_nodes(arena, node_index, &
                                                node_type_name, indices, count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: node_type_name
        integer, intent(inout) :: indices(:)
        integer, intent(inout) :: count
        integer :: i
        integer, allocatable :: children(:)

        if (node_index <= 0 .or. node_index > arena%compat_size) return

        if (allocated(arena%entries(node_index)%node_type)) then
            if (arena%entries(node_index)%node_type == node_type_name) then
                count = count + 1
                indices(count) = node_index
            end if
        end if

        children = arena%get_children(node_index)
        do i = 1, size(children)
            call collect_matching_nodes(arena, children(i), node_type_name, &
                                        indices, count)
        end do
    end subroutine collect_matching_nodes

    function get_ancestor_of_type(arena, node_index, node_type_name) &
        result(ancestor_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: node_type_name
        integer :: ancestor_index
        integer :: current_index

        ancestor_index = 0
        current_index = node_index

        if (current_index <= 0 .or. current_index > arena%compat_size) return

        current_index = arena%entries(current_index)%parent_index

        do while (current_index > 0 .and. current_index <= arena%compat_size)
            if (allocated(arena%entries(current_index)%node_type)) then
                if (arena%entries(current_index)%node_type == node_type_name) then
                    ancestor_index = current_index
                    return
                end if
            end if
            current_index = arena%entries(current_index)%parent_index
        end do
    end function get_ancestor_of_type

    function has_child_of_type(arena, node_index, node_type_name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: node_type_name
        logical :: found
        integer, allocatable :: children(:)
        integer :: i

        found = .false.

        if (node_index <= 0 .or. node_index > arena%compat_size) return

        children = arena%get_children(node_index)

        do i = 1, size(children)
            if (allocated(arena%entries(children(i))%node_type)) then
                if (arena%entries(children(i))%node_type == node_type_name) then
                    found = .true.
                    return
                end if
            end if
        end do
    end function has_child_of_type

    function get_children_impl(arena, node_index) result(child_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: child_indices(:)

        child_indices = arena%get_children(node_index)
    end function get_children_impl

    subroutine traverse_ast(arena, root_index, callback, user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        procedure(traverse_callback) :: callback
        class(*), intent(inout), optional :: user_data

        call traverse_recursive(arena, root_index, callback, user_data)
    end subroutine traverse_ast

    recursive subroutine traverse_recursive(arena, node_index, callback, &
                                            user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        procedure(traverse_callback) :: callback
        class(*), intent(inout), optional :: user_data
        integer, allocatable :: children(:)
        integer :: i

        if (node_index <= 0 .or. node_index > arena%compat_size) return

        if (present(user_data)) then
            call callback(arena, node_index, user_data)
        else
            call callback(arena, node_index)
        end if

        children = arena%get_children(node_index)
        do i = 1, size(children)
            call traverse_recursive(arena, children(i), callback, user_data)
        end do
    end subroutine traverse_recursive

end module ast_traversal_utils
