module semantic_assignment_inference
    ! Assignment inference logic extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
                                   create_mono_type, create_poly_type, &
                                   TCHAR, TARRAY, TINT
    use ast_core
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node
    use semantic_validation_utils, only: update_identifier_type_in_arena
    use error_handling, only: result_t, create_error_result, ERROR_SEMANTIC
    use scope_manager, only: scope_stack_t
    ! No direct dependency on function analysis here
    use error_handling, only: error_collection_t
    implicit none
    private

    public :: process_assignment_inference

contains

    ! Process assignment inference with scope and error handling
    recursive subroutine process_assignment_inference(arena, assignment, assignment_index, &
                                           lhs_index, expr_typ, &
                                           scopes, errors, strict_mode, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index, lhs_index
        type(mono_type_t), intent(inout) :: expr_typ
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        integer, intent(inout) :: next_var_id
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: existing_scheme
        type(result_t) :: error_result

        if (lhs_index > 0 .and. lhs_index <= arena%size) then
            if (allocated(arena%entries(lhs_index)%node)) then
                select type (lhs_node => arena%entries(lhs_index)%node)
                type is (identifier_node)
                    ! Check if already defined in current or parent scope
                    call scopes%lookup(lhs_node%name, existing_scheme)
                    
                    if (.not. allocated(existing_scheme)) then
                        ! Fallback: if this identifier is declared somewhere in the arena,
                        ! define it in scope before deciding it's undefined. This protects
                        ! multi-variable declarations with initializers and re-ordered nodes.
                        call ensure_declared_from_arena_local(scopes, arena, lhs_node%name)
                        call scopes%lookup(lhs_node%name, existing_scheme)
                    end if

                    if (.not. allocated(existing_scheme)) then
                        ! Do not raise an error here. A centralized undefined-variable
                        ! check runs after inference and handles strict-mode diagnostics
                        ! with proper arena-backed discovery. Keeping this path silent
                        ! avoids duplicate or premature errors for multi-declarations.
                    end if
                    
                    ! Handle allocatable character detection
                    if (expr_typ%kind == TCHAR) then
                        call handle_character_allocation(arena, assignment, expr_typ, lhs_node%name)
                    end if
                    
                    ! Update all identifier nodes in the arena with the inferred type
                    call update_identifier_type_in_arena(arena, lhs_node%name, expr_typ)
                    
                    ! Generalize the expression type and define/update in scope
                    scheme = create_poly_type(forall_vars=[type_var_t::], mono=expr_typ)
                    call scopes%define(lhs_node%name, scheme)
                end select
            end if
        end if
    end subroutine process_assignment_inference

    ! Local helper: best-effort define symbol from any declaration present in the arena
    recursive subroutine ensure_declared_from_arena_local(scopes, arena, name)
        use ast_nodes_data, only: declaration_node
        use semantic_inference_helpers, only: process_declaration_variables
        type(scope_stack_t), intent(inout) :: scopes
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer :: i, j
        type(mono_type_t) :: decl_type
        type(poly_type_t) :: scheme

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == trim(name)) then
                        call process_declaration_variables(node, decl_type)
                        scheme = create_poly_type(forall_vars=[type_var_t::], mono=decl_type)
                        call scopes%define(name, scheme)
                        return
                    end if
                end if
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (trim(node%var_names(j)) == trim(name)) then
                            call process_declaration_variables(node, decl_type)
                            scheme = create_poly_type(forall_vars=[type_var_t::], mono=decl_type)
                            call scopes%define(name, scheme)
                            return
                        end if
                    end do
                end if
            end select
        end do
    end subroutine ensure_declared_from_arena_local

    ! Handle character allocation detection for string concatenation
    recursive subroutine handle_character_allocation(arena, assignment, expr_typ, var_name)
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        type(mono_type_t), intent(inout) :: expr_typ
        character(len=*), intent(in) :: var_name

        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
            if (allocated(arena%entries(assignment%value_index)%node)) then
                select type (value_node => arena%entries(assignment%value_index)%node)
                type is (binary_op_node)
                    if (value_node%operator == "//") then
                        ! Only mark as allocatable if size was not calculated
                        if (expr_typ%size < 0) then
                            expr_typ%alloc_info%is_allocatable = .true.
                            expr_typ%alloc_info%needs_allocatable_string = .true.
                            expr_typ%size = 0  ! Deferred length
                        end if
                        
                        ! Update all existing identifier nodes with this name
                        call update_identifier_type_in_arena(arena, var_name, expr_typ)
                    end if
                end select
            end if
        end if
    end subroutine handle_character_allocation

end module semantic_assignment_inference
