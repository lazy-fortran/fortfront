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
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        character(*), intent(in), optional :: node_type
        integer :: index
        logical :: handled
        
        ! First allocate the entry
        call arena%ensure_capacity()
        arena%size = arena%size + 1
        index = arena%size
        
        ! Try each category of nodes
        handled = .false.
        if (.not. handled) call store_core_nodes(arena, node, index, handled)
        if (.not. handled) call store_procedure_nodes(arena, node, index, handled)
        if (.not. handled) call store_data_nodes(arena, node, index, handled)
        
        ! Handle unknown types
        if (.not. handled) then
            call store_error_placeholder(arena, node, index)
        end if
        
        ! Set metadata
        call set_node_metadata(arena, index, node_type)
        
        ! Update arena tracking
        call update_arena_tracking(arena, index)
        
    end function safe_arena_push
    
    subroutine store_core_nodes(arena, node, index, handled)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        integer, intent(in) :: index
        logical, intent(out) :: handled
        
        handled = .true.
        
        select type(n => node)
        type is (program_node)
            allocate(program_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (assignment_node)
            allocate(assignment_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (binary_op_node)
            allocate(binary_op_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (identifier_node)
            allocate(identifier_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (literal_node)
            allocate(literal_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (array_literal_node)
            allocate(array_literal_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (call_or_subscript_node)
            allocate(call_or_subscript_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        class default
            handled = .false.
        end select
    end subroutine store_core_nodes
    
    subroutine store_procedure_nodes(arena, node, index, handled)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        integer, intent(in) :: index
        logical, intent(out) :: handled
        
        handled = .true.
        
        select type(n => node)
        type is (function_def_node)
            allocate(function_def_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (subroutine_def_node)
            allocate(subroutine_def_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (subroutine_call_node)
            allocate(subroutine_call_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        class default
            handled = .false.
        end select
    end subroutine store_procedure_nodes
    
    subroutine store_data_nodes(arena, node, index, handled)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        integer, intent(in) :: index
        logical, intent(out) :: handled
        
        handled = .true.
        
        select type(n => node)
        type is (declaration_node)
            allocate(declaration_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (parameter_declaration_node)
            allocate(parameter_declaration_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        type is (module_node)
            allocate(module_node :: arena%entries(index)%node)
            call copy_node(arena%entries(index)%node, n)
            
        class default
            handled = .false.
        end select
    end subroutine store_data_nodes
    
    subroutine copy_node(dest, src)
        class(ast_node), intent(inout) :: dest
        class(ast_node), intent(in) :: src
        
        select type(dest_typed => dest)
        type is (program_node)
            select type(src_typed => src)
            type is (program_node)
                dest_typed = src_typed
            end select
        type is (assignment_node)
            select type(src_typed => src)
            type is (assignment_node)
                dest_typed = src_typed
            end select
        type is (binary_op_node)
            select type(src_typed => src)
            type is (binary_op_node)
                dest_typed = src_typed
            end select
        type is (identifier_node)
            select type(src_typed => src)
            type is (identifier_node)
                dest_typed = src_typed
            end select
        type is (literal_node)
            select type(src_typed => src)
            type is (literal_node)
                dest_typed = src_typed
            end select
        type is (array_literal_node)
            select type(src_typed => src)
            type is (array_literal_node)
                dest_typed = src_typed
            end select
        type is (call_or_subscript_node)
            select type(src_typed => src)
            type is (call_or_subscript_node)
                dest_typed = src_typed
            end select
        type is (function_def_node)
            select type(src_typed => src)
            type is (function_def_node)
                dest_typed = src_typed
            end select
        type is (subroutine_def_node)
            select type(src_typed => src)
            type is (subroutine_def_node)
                dest_typed = src_typed
            end select
        type is (subroutine_call_node)
            select type(src_typed => src)
            type is (subroutine_call_node)
                dest_typed = src_typed
            end select
        type is (declaration_node)
            select type(src_typed => src)
            type is (declaration_node)
                dest_typed = src_typed
            end select
        type is (parameter_declaration_node)
            select type(src_typed => src)
            type is (parameter_declaration_node)
                dest_typed = src_typed
            end select
        type is (module_node)
            select type(src_typed => src)
            type is (module_node)
                dest_typed = src_typed
            end select
        end select
    end subroutine copy_node
    
    subroutine store_error_placeholder(arena, node, index)
        use ast_arena_modern, only: ast_arena_t
        use ast_error_nodes, only: error_node_t, create_error_node
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        integer, intent(in) :: index
        
        print *, "ERROR: Unknown AST node type - creating error placeholder"
        allocate(error_node_t :: arena%entries(index)%node)
        select type(error_node => arena%entries(index)%node)
        type is (error_node_t)
            error_node = create_error_node("Unknown node type in arena storage", &
                                           "Could not safely store node")
        end select
    end subroutine store_error_placeholder
    
    subroutine set_node_metadata(arena, index, node_type)
        use ast_arena_modern, only: ast_arena_t
        use ast_error_nodes, only: error_node_t
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index
        character(*), intent(in), optional :: node_type
        
        if (present(node_type)) then
            arena%entries(index)%node_type = node_type
        else
            ! Check if we created an error node
            select type(node_ref => arena%entries(index)%node)
            type is (error_node_t)
                arena%entries(index)%node_type = "error_node"
            class default
                arena%entries(index)%node_type = "unknown"
            end select
        end if
    end subroutine set_node_metadata
    
    subroutine update_arena_tracking(arena, index)
        use ast_arena_modern, only: ast_arena_t
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index
        
        arena%entries(index)%parent_index = arena%current_index
        if (arena%current_index > 0) then
            arena%entries(index)%depth = arena%entries(arena%current_index)%depth + 1
        else
            arena%entries(index)%depth = 0
        end if
        
        arena%current_index = index
        arena%max_depth = max(arena%max_depth, arena%entries(index)%depth)
    end subroutine update_arena_tracking

end module ast_arena_safe