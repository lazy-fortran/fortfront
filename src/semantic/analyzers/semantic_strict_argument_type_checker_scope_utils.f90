module semantic_strict_argument_type_checker_scope_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: module_node, submodule_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: split_rename
    public :: find_module_node_index
    public :: node_name_matches

contains

    subroutine split_rename(mapping, local_name, remote_name)
        character(len=*), intent(in) :: mapping
        character(len=:), allocatable, intent(out) :: local_name
        character(len=:), allocatable, intent(out) :: remote_name

        integer :: arrow

        arrow = index(mapping, "=>")
        if (arrow <= 0) return
        if (arrow == 1) return
        if (arrow + 1 >= len(mapping)) return

        local_name = adjustl(mapping(1:arrow - 1))
        remote_name = adjustl(mapping(arrow + 2:))
        local_name = trim(local_name)
        remote_name = trim(remote_name)
    end subroutine split_rename

    subroutine find_module_node_index(arena, module_lowered, module_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: module_lowered
        integer, intent(out) :: module_index

        integer :: i

        module_index = 0
        if (len_trim(module_lowered) == 0) return

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) == module_lowered) then
                    module_index = i
                    return
                end if
            type is (submodule_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) == module_lowered) then
                    module_index = i
                    return
                end if
            class default
                cycle
            end select
        end do
    end subroutine find_module_node_index

    logical function node_name_matches(node_name, lowered_name) result(matches)
        character(len=:), allocatable, intent(in) :: node_name
        character(len=*), intent(in) :: lowered_name

        matches = .false.
        if (.not. allocated(node_name)) return
        if (len_trim(lowered_name) == 0) return
        matches = to_lower(trim(node_name)) == lowered_name
    end function node_name_matches

end module semantic_strict_argument_type_checker_scope_utils

