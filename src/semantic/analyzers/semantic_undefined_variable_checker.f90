module semantic_undefined_variable_checker
    ! Undefined variable detection and checking utilities
    ! Extracted from semantic_analyzer.f90 for architectural compliance (Issue #1067)
    use ast_core, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node, &
                               call_or_subscript_node, array_literal_node, program_node
    use ast_nodes_control, only: if_node
    use type_system_unified, only: poly_type_t
    use scope_manager, only: scope_stack_t
    use error_handling, only: create_error_result, ERROR_SEMANTIC, result_t, error_collection_t
    implicit none
    private

    public :: check_undefined_variables_generic

    ! Generic interface to work with any context type that has scopes and errors
    abstract interface
        subroutine check_undefined_vars_interface(scopes, errors, strict_mode, arena, prog_index)
            import :: scope_stack_t, error_collection_t, ast_arena_t
            type(scope_stack_t), intent(inout) :: scopes
            type(error_collection_t), intent(inout) :: errors  
            logical, intent(in) :: strict_mode
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: prog_index
        end subroutine
    end interface

contains

    ! Generic implementation that works with any context type
    subroutine check_undefined_variables_generic(scopes, errors, strict_mode, arena, prog_index)
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        
        ! Only perform undefined variable checking in strict mode
        if (.not. strict_mode) return
        
        ! Recursively traverse the AST to find identifier nodes and check if they're defined
        call traverse_for_undefined_variables(scopes, errors, arena, prog_index)
    end subroutine check_undefined_variables_generic
    
    ! Recursive helper to traverse AST and detect undefined variables
    recursive subroutine traverse_for_undefined_variables(scopes, errors, arena, node_index)
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
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
            call scopes%lookup(node%name, scheme)
            if (.not. allocated(scheme)) then
                ! Undefined variable found - create semantic error
                error_result = create_error_result( &
                    "Undefined variable '" // node%name // "'", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="check_undefined_variables", &
                    suggestion="Declare the variable before using it or remove 'implicit none'" &
                )
                call errors%add_result(error_result)
            end if
            
        type is (program_node)
            ! Traverse program body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call traverse_for_undefined_variables(scopes, errors, arena, node%body_indices(i))
                end do
            end if
            
        type is (binary_op_node)
            ! Traverse both operands
            call traverse_for_undefined_variables(scopes, errors, arena, node%left_index)
            call traverse_for_undefined_variables(scopes, errors, arena, node%right_index)
            
        type is (assignment_node)
            ! Traverse both sides of assignment
            call traverse_for_undefined_variables(scopes, errors, arena, node%target_index)
            call traverse_for_undefined_variables(scopes, errors, arena, node%value_index)
            
        type is (call_or_subscript_node)
            ! Traverse function arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call traverse_for_undefined_variables(scopes, errors, arena, node%arg_indices(i))
                end do
            end if
            
        type is (array_literal_node)
            ! Traverse array elements
            if (allocated(node%element_indices)) then
                do i = 1, size(node%element_indices)
                    call traverse_for_undefined_variables(scopes, errors, arena, node%element_indices(i))
                end do
            end if
            
        type is (if_node)
            ! Traverse if statement condition and branches
            if (node%condition_index > 0) then
                call traverse_for_undefined_variables(scopes, errors, arena, node%condition_index)
            end if
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call traverse_for_undefined_variables(scopes, errors, arena, node%then_body_indices(i))
                end do
            end if
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call traverse_for_undefined_variables(scopes, errors, arena, node%else_body_indices(i))
                end do
            end if
            
        class default
            ! For other node types, no traversal needed or implement as needed
            continue
        end select
    end subroutine traverse_for_undefined_variables



end module semantic_undefined_variable_checker