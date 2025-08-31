module semantic_undefined_variable_checker
    ! Undefined variable detection and checking utilities
    ! Extracted from semantic_analyzer.f90 for architectural compliance (Issue #1067)
    use semantic_context_types, only: semantic_context_base_t
    use ast_core, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node, &
                               call_or_subscript_node, array_literal_node, program_node
    use ast_nodes_control, only: if_node
    use type_system_unified, only: poly_type_t
    use error_handling, only: create_error_result, ERROR_SEMANTIC, result_t
    implicit none
    private

    public :: check_undefined_variables_internal

    ! Forward declaration for semantic_context_t (to avoid circular dependency)
    type, abstract, extends(semantic_context_base_t) :: semantic_context_base_extended_t
        logical :: strict_mode = .false.
    end type

contains

    subroutine check_undefined_variables_internal(ctx, arena, prog_index)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        logical :: strict_mode_value
        
        ! Get strict mode from context (using select type since we can't access directly)
        select type (ctx)
        class is (semantic_context_base_extended_t)
            strict_mode_value = ctx%strict_mode
        class default
            strict_mode_value = .false.  ! Default to non-strict mode
        end select
        
        ! Only perform undefined variable checking in strict mode
        if (.not. strict_mode_value) return
        
        ! Recursively traverse the AST to find identifier nodes and check if they're defined
        call traverse_for_undefined_variables(ctx, arena, prog_index)
    end subroutine check_undefined_variables_internal
    
    ! Recursive helper to traverse AST and detect undefined variables
    recursive subroutine traverse_for_undefined_variables(ctx, arena, node_index)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(poly_type_t), allocatable :: scheme
        type(result_t) :: error_result
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            ! Skip empty/unallocated identifiers
            if (.not. allocated(node%name) .or. len_trim(node%name) == 0) return
            
            ! Check if identifier is defined in scope  
            ! Note: This requires access to scope lookup which is context-specific
            ! For now, we'll create a simplified check that can be enhanced later
            call check_identifier_in_scope(ctx, node%name, scheme)
            if (.not. allocated(scheme)) then
                ! Undefined variable found - create semantic error
                error_result = create_error_result( &
                    "Undefined variable '" // node%name // "'", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="check_undefined_variables", &
                    suggestion="Declare the variable before using it or remove 'implicit none'" &
                )
                call add_error_to_context(ctx, error_result)
            end if
            
        type is (program_node)
            ! Traverse program body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call traverse_for_undefined_variables(ctx, arena, node%body_indices(i))
                end do
            end if
            
        type is (binary_op_node)
            ! Traverse both operands
            call traverse_for_undefined_variables(ctx, arena, node%left_index)
            call traverse_for_undefined_variables(ctx, arena, node%right_index)
            
        type is (assignment_node)
            ! Traverse both sides of assignment
            call traverse_for_undefined_variables(ctx, arena, node%target_index)
            call traverse_for_undefined_variables(ctx, arena, node%value_index)
            
        type is (call_or_subscript_node)
            ! Traverse function arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call traverse_for_undefined_variables(ctx, arena, node%arg_indices(i))
                end do
            end if
            
        type is (array_literal_node)
            ! Traverse array elements
            if (allocated(node%element_indices)) then
                do i = 1, size(node%element_indices)
                    call traverse_for_undefined_variables(ctx, arena, node%element_indices(i))
                end do
            end if
            
        type is (if_node)
            ! Traverse if statement condition and branches
            if (node%condition_index > 0) then
                call traverse_for_undefined_variables(ctx, arena, node%condition_index)
            end if
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call traverse_for_undefined_variables(ctx, arena, node%then_body_indices(i))
                end do
            end if
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call traverse_for_undefined_variables(ctx, arena, node%else_body_indices(i))
                end do
            end if
            
        class default
            ! For other node types, no traversal needed or implement as needed
            continue
        end select
    end subroutine traverse_for_undefined_variables

    ! Helper to check identifier in scope (simplified interface)
    subroutine check_identifier_in_scope(ctx, name, scheme)
        class(semantic_context_base_t), intent(inout) :: ctx
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable, intent(out) :: scheme
        
        ! This is a simplified interface that needs to be implemented
        ! by the specific context type. For now, mark as not found.
        if (allocated(scheme)) deallocate(scheme)
        ! The actual implementation would call ctx%scopes%lookup(name, scheme)
    end subroutine check_identifier_in_scope

    ! Helper to add error to context (simplified interface)
    subroutine add_error_to_context(ctx, error_result)
        class(semantic_context_base_t), intent(inout) :: ctx
        type(result_t), intent(in) :: error_result
        
        ! This is a simplified interface that needs to be implemented
        ! by the specific context type. For now, it's a no-op.
        ! The actual implementation would call ctx%errors%add_result(error_result)
    end subroutine add_error_to_context

end module semantic_undefined_variable_checker