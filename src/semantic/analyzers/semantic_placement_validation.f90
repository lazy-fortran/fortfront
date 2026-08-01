module semantic_placement_validation
    ! Rejects declarations that name an attribute which the standard confines
    ! to one kind of program section (issue #2896).
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, module_node
    use ast_nodes_core, only: program_node
    use error_handling, only: ERROR_SEMANTIC, create_error_result, &
        error_collection_t
    implicit none
    private

    public :: validate_main_program_declaration_placement
    public :: validate_declaration_placement_in_arena

contains

    ! Whole-arena sweep: every main program in the arena is checked, which
    ! does not depend on the root kind the inference dispatch selects.
    subroutine validate_declaration_placement_in_arena(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (declaration_node)
                if (.not. node%is_protected) cycle
                if (declaration_is_in_module_spec(arena, i)) cycle
                call emit_protected_outside_module(errors, node%line, node%column)
            end select
        end do
    end subroutine validate_declaration_placement_in_arena

    logical function declaration_is_in_module_spec(arena, declaration_index) &
            result(in_module_spec)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: declaration_index
        integer :: module_index, j

        in_module_spec = .false.
        do module_index = 1, arena%size
            if (.not. arena%has_node_at(module_index)) cycle
            select type (module => arena%entries(module_index)%node)
                type is (module_node)
                if (.not. allocated(module%declaration_indices)) cycle
                do j = 1, size(module%declaration_indices)
                    if (module%declaration_indices(j) == declaration_index) then
                        in_module_spec = .true.
                        return
                    end if
                end do
            end select
        end do
    end function declaration_is_in_module_spec

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
