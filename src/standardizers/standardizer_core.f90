module standardizer_core
    ! Core standardization module - main entry points and orchestration
    ! This module provides the primary interface for AST standardization

    use ast_arena_modern, only: ast_arena_t
    use ast_factory
    use type_system_unified
    use error_handling, only: &
        ERROR_TYPE_SYSTEM
    use standardizer_program
    use standardizer_module
    use standardizer_subprograms, only: &
        wrap_subroutine_in_program
    use standardizer_parameter, only: set_standardizer_input_mode
    use standardizer_declarations_state, only: &
        set_declaration_type_standardization => set_standardizer_type_standardization, &
        get_declaration_type_standardization => get_standardizer_type_standardization
    implicit none
    private

    ! Constants
    integer, parameter :: INVALID_INTEGER = -999999

    public :: standardize_ast, standardize_multi_unit_children
    ! JSON-based API removed
    public :: set_standardizer_type_standardization, &
        get_standardizer_type_standardization
    public :: set_standardizer_input_mode

contains

    ! Wrapper that initializes cycle guards and calls the recursive implementation.
    subroutine standardize_ast(arena, root_index, in_module)
        use standardizer_driver, only: standardize_ast_iter
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        logical, intent(in), optional :: in_module
        call standardize_ast_iter(arena, root_index)
    end subroutine standardize_ast

    ! JSON interface for standardization
    ! JSON standardization entry removed

    ! Configuration setters/getters
    subroutine set_standardizer_type_standardization(enabled)
        logical, intent(in) :: enabled
        call set_declaration_type_standardization(enabled)
    end subroutine set_standardizer_type_standardization

    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        call get_declaration_type_standardization(enabled)
    end subroutine get_standardizer_type_standardization

    ! String result methods now in standardizer_types

    subroutine standardize_multi_unit_children(arena, root_index)
        use ast_nodes_core, only: program_node
        use ast_nodes_data, only: multi_unit_container_node
        use ast_nodes_procedure, only: subroutine_def_node, function_def_node
        use standardizer_subprograms, only: standardize_subroutine_def, &
            standardize_function_def
        use standardizer_program, only: standardize_program
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer :: i, child_index

        if (.not. arena%has_node_at(root_index)) return

        select type (node => arena%entries(root_index)%node)
            type is (multi_unit_container_node)
            if (.not. allocated(node%body_indices)) return

            ! CRITICAL: Copy indices before the loop to avoid a dangling selector if
            ! standardize_program triggers arena reallocation.
            block
                integer, allocatable :: local_indices(:)
                allocate (local_indices(size(node%body_indices)))
                local_indices = node%body_indices

                do i = 1, size(local_indices)
                    child_index = local_indices(i)
                    if (.not. arena%has_node_at(child_index)) cycle

                    select type (child => arena%entries(child_index)%node)
                        type is (subroutine_def_node)
                        call standardize_subroutine_def(arena, child, child_index)
                        type is (function_def_node)
                        call standardize_function_def(arena, child, child_index)
                        type is (program_node)
                        call standardize_program(arena, child, child_index)
                    end select
                end do

                deallocate (local_indices)
            end block
        end select
    end subroutine standardize_multi_unit_children

end module standardizer_core
