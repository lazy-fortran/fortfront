module semantic_placement_validation
    ! Rejects declarations that name an attribute which the standard confines
    ! to one kind of program section (issue #2896).
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use error_handling, only: ERROR_SEMANTIC, create_error_result, &
        error_collection_t
    implicit none
    private

    public :: validate_main_program_declaration_placement

contains

    ! F2018 C858: the PROTECTED attribute is permitted only in the
    ! specification part of a module. A main program has no module semantics
    ! to protect anything from, so the attribute cannot appear there.
    subroutine validate_main_program_declaration_placement(arena, body_indices, &
            errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors

        integer :: i

        if (.not. allocated(body_indices)) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. node%is_protected) cycle
                call emit_protected_outside_module(errors, node%line, node%column)
            class default
                cycle
            end select
        end do
    end subroutine validate_main_program_declaration_placement

    subroutine emit_protected_outside_module(errors, line, column)
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: line, column

        call errors%add_result(create_error_result( &
            "PROTECTED attribute is only allowed in the specification part "// &
            "of a module", ERROR_SEMANTIC, &
            component="semantic_analyzer", &
            context="declaration_placement", &
            suggestion="Move the declaration into a module, or drop the "// &
            "PROTECTED attribute", &
            line=line, column=column, end_line=line, end_column=column + 1))
    end subroutine emit_protected_outside_module

end module semantic_placement_validation
