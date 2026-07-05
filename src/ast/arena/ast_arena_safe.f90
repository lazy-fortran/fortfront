module ast_arena_safe
    ! SAFE version of ast_arena that handles all node types via generic
    ! allocate(source=) deep copy. Previously handled only ~13 types;
    ! now covers all node kinds (issue #2842).
   use ast_base, only: ast_node
    implicit none
    private

    public :: safe_arena_push

contains

    function safe_arena_push(arena, node, node_type) result(index)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        character(*), intent(in), optional :: node_type
        integer :: index

        ! Allocate entry and deep-copy the node via allocate(source=).
        ! This invokes the type-bound assignment operator which performs
        ! a recursive deep copy of all allocatable components.
        call arena%ensure_capacity()
        arena%size = arena%size + 1
        index = arena%size

        if (allocated(arena%entries(index)%node)) then
            deallocate (arena%entries(index)%node)
        end if
        allocate (arena%entries(index)%node, source=node)

        ! Set metadata
        if (present(node_type)) then
            arena%entries(index)%node_type = node_type
        else
            arena%entries(index)%node_type = "unknown"
        end if

        ! Update arena tracking
        arena%entries(index)%parent_index = arena%current_index
        if (arena%current_index > 0) then
            arena%entries(index)%depth = arena%entries(arena%current_index)%depth + 1
        else
            arena%entries(index)%depth = 0
        end if

        arena%current_index = index
        arena%max_depth = max(arena%max_depth, arena%entries(index)%depth)

    end function safe_arena_push

end module ast_arena_safe
