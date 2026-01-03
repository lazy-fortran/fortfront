module ast_traversal_gather
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: gather_child_indices

contains

    subroutine gather_child_indices(arena, node_index, children)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: children(:)

        integer :: count

        if (.not. arena%has_node_at(node_index)) then
            allocate (children(0))
            return
        end if

        if (.not. allocated(arena%entries(node_index)%child_indices)) then
            allocate (children(0))
            return
        end if

        count = arena%entries(node_index)%child_count
        if (count <= 0) then
            allocate (children(0))
            return
        end if

        count = min(count, size(arena%entries(node_index)%child_indices))
        if (count <= 0) then
            allocate (children(0))
            return
        end if

        allocate (children(count))
        children = arena%entries(node_index)%child_indices(1:count)
    end subroutine gather_child_indices

end module ast_traversal_gather
