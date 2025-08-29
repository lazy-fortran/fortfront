module variable_usage_node_processors_module
    use iso_fortran_env, only: error_unit
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_collector_module
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
                              identifier_node, component_access_node, &
                              assignment_node, program_node, literal_node
    use ast_nodes_bounds, only: array_slice_node
    implicit none
    private

    ! Public procedures
    public :: process_binary_op_children, process_call_or_subscript_children
    public :: process_array_slice_children, process_component_access_children
    public :: process_assignment_node_children, process_program_node_children
    public :: process_literal_node_children, process_procedure_def_body

contains

    ! Process binary operation children
    subroutine process_binary_op_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            if (node%left_index > 0) then
                call collect_identifiers_recursive(arena, node%left_index, info)
            end if
            if (node%right_index > 0) then
                call collect_identifiers_recursive(arena, node%right_index, info)
            end if
        end select
    end subroutine process_binary_op_children

    ! Process call or subscript children
    subroutine process_call_or_subscript_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        select type (node => arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            ! The function/array name is stored as a string, not an index
            ! We need to add it manually to the info if it's a variable reference
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if
            
            ! Process all arguments/subscripts
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%arg_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_call_or_subscript_children

    ! Process array slice children
    subroutine process_array_slice_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (array_slice_node)
            ! Process array name
            if (node%array_index > 0) then
                call collect_identifiers_recursive(arena, node%array_index, info)
            end if
            
            ! Process slice bounds
            block
                integer :: i
                do i = 1, node%num_dimensions
                    if (node%bounds_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%bounds_indices(i), info)
                    end if
                end do
            end block
        end select
    end subroutine process_array_slice_children

    ! Process component access children
    subroutine process_component_access_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        select type (node => arena%entries(node_index)%node)
        type is (component_access_node)
            ! Process the base expression (structure/derived type)
            if (node%base_expr_index > 0) then
                call collect_identifiers_recursive(arena, node%base_expr_index, info)
            end if
            
            ! Component name is stored as a string
            if (allocated(node%component_name)) then
                call add_string_to_info(node%component_name, node_index, info)
            end if
        end select
    end subroutine process_component_access_children

    ! Process assignment node children
    subroutine process_assignment_node_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "assignment") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if
        
        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Process target (LHS) - might have array subscripts
            if (node%target_index > 0) then
                call collect_identifiers_recursive(arena, node%target_index, info)
            end if
            
            ! Process value (RHS)
            if (node%value_index > 0) then
                call collect_identifiers_recursive(arena, node%value_index, info)
            end if
        end select
    end subroutine process_assignment_node_children

    ! Process program node children
    subroutine process_program_node_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        integer :: i
        
        ! Validate node index bounds
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            ! Process all body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0) then
                        call collect_identifiers_recursive(arena, node%body_indices(i), info)
                    end if
                end do
            end if
        end select
    end subroutine process_program_node_children

    ! Process literal node children (might contain parsed expressions)
    subroutine process_literal_node_children(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        
        ! Literals typically don't have child nodes with expressions
        ! But we should check if this literal represents a parsed statement
        ! that might contain identifiers (like an if statement)
        
        ! For now, we don't traverse literal nodes as they usually contain
        ! constant values, not variable references
    end subroutine process_literal_node_children

    ! Process procedure definition children (shared by subroutine and function)
    subroutine process_procedure_def_body(arena, node_index, info, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        integer, intent(in), optional :: body_indices(:)
        
        integer :: i
        
        ! Process all body statements - this will capture all identifiers used
        ! within the procedure, including dummy arguments when they're used
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                if (body_indices(i) > 0) then
                    call collect_identifiers_recursive(arena, body_indices(i), info)
                end if
            end do
        end if
    end subroutine process_procedure_def_body

end module variable_usage_node_processors_module