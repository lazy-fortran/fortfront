module standardizer_interface_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: interface_block_node
    implicit none
    private
    public :: function_in_interface_block
contains

    logical function function_in_interface_block(arena, func_index) &
            result(in_iface)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: func_index
        integer :: parent_index
        integer :: iface_index

        in_iface = .false.
        if (.not. arena%has_node_at(func_index)) return

        parent_index = arena%entries(func_index)%parent_index
        do while (parent_index > 0 .and. parent_index <= arena%size)
            if (.not. allocated(arena%entries(parent_index)%node)) then
                parent_index = arena%entries(parent_index)%parent_index
                cycle
            end if
            select type (parent => arena%entries(parent_index)%node)
                type is (interface_block_node)
                in_iface = .true.
                return
            class default
                parent_index = arena%entries(parent_index)%parent_index
            end select
        end do

        do iface_index = 1, arena%size
            if (.not. arena%has_node_at(iface_index)) cycle
            select type (iface => arena%entries(iface_index)%node)
                type is (interface_block_node)
                if (.not. allocated(iface%procedure_indices)) cycle
                if (any(iface%procedure_indices == func_index)) then
                    in_iface = .true.
                    return
                end if
            end select
        end do
    end function function_in_interface_block

end module standardizer_interface_utils
