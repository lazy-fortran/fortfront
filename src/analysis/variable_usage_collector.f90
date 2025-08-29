module variable_usage_collector_module
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_dispatcher_module
    implicit none
    private

    ! Public procedures
    public :: collect_identifiers_recursive

contains

    ! Recursively collect all identifier nodes from expression subtree
    recursive subroutine collect_identifiers_recursive(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        character(len=:), allocatable :: node_type
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        ! If this is an identifier, add it to the list
        if (node_type == "identifier") then
            call add_identifier_to_info(arena, node_index, info)
        end if
        
        ! Forward declaration for dispatcher function
        call dispatch_node_processing(arena, node_index, info, node_type)
    end subroutine collect_identifiers_recursive

end module variable_usage_collector_module