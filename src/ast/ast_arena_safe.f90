module ast_arena_safe
    ! SAFE version of ast_arena that avoids source= allocations
    use ast_base, only: ast_node
    use ast_nodes_core, only: program_node, assignment_node, binary_op_node, &
                              identifier_node, literal_node, array_literal_node, &
                              call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                    subroutine_call_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                               module_node
    use ast_nodes_control, only: if_node, do_loop_node, do_while_node, &
                                  select_case_node, case_block_node
    implicit none
    private

    public :: safe_arena_push

contains

    function safe_arena_push(arena, node, node_type) result(index)
        use ast_arena, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        character(*), intent(in), optional :: node_type
        integer :: index
        
        ! First allocate the entry
        call arena%ensure_capacity()
        arena%size = arena%size + 1
        index = arena%size
        
        ! Type-specific allocation WITHOUT source=
        select type(n => node)
        type is (program_node)
            allocate(program_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (program_node)
                dest = n  ! Use assignment operator
            end select
            
        type is (assignment_node)
            allocate(assignment_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (assignment_node)
                dest = n
            end select
            
        type is (binary_op_node)
            allocate(binary_op_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (binary_op_node)
                dest = n
            end select
            
        type is (identifier_node)
            allocate(identifier_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (identifier_node)
                dest = n
            end select
            
        type is (literal_node)
            allocate(literal_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (literal_node)
                dest = n
            end select
            
        type is (function_def_node)
            allocate(function_def_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (function_def_node)
                dest = n
            end select
            
        type is (subroutine_def_node)
            allocate(subroutine_def_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (subroutine_def_node)
                dest = n
            end select
            
        type is (declaration_node)
            allocate(declaration_node :: arena%entries(index)%node)
            select type(dest => arena%entries(index)%node)
            type is (declaration_node)
                dest = n
            end select
            
        ! ... Would need to add ALL node types here ...
        
        class default
            ! Fallback - still unsafe but at least we know when it happens
            print *, "WARNING: Using unsafe source= for unknown node type"
            allocate(arena%entries(index)%node, source=node)
        end select
        
        ! Set metadata
        if (present(node_type)) then
            arena%entries(index)%node_type = node_type
        else
            arena%entries(index)%node_type = "unknown"
        end if
        
        ! Update arena metadata
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