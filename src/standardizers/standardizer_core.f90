module standardizer_core
    ! Core standardization module - main entry points and orchestration
    ! This module provides the primary interface for AST standardization

    use ast_arena_modern, only: ast_arena_t
    use ast_factory
    use type_system_unified
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result, &
                              ERROR_TYPE_SYSTEM
    use standardizer_program
    use standardizer_module
    use standardizer_types, only: string_result_t
    use standardizer_subprograms, only: wrap_function_in_program, &
                                        wrap_subroutine_in_program
    use debug_trace, only: trace_enter, trace_leave
    use standardizer_parameter, only: set_standardizer_input_mode
    implicit none
    private

    ! Type standardization configuration for standardizer
    ! DISABLED: Converting real -> real(8) breaks generic interfaces that
    ! depend on exact type matching. Users should explicitly use real(8) or
    ! kind parameters if they want double precision.
    logical, save :: standardizer_type_standardization_enabled = .false.

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
        standardizer_type_standardization_enabled = enabled
    end subroutine set_standardizer_type_standardization

    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    ! String result methods now in standardizer_types

    subroutine standardize_multi_unit_children(arena, root_index)
        use ast_nodes_core, only: program_node
        use ast_nodes_procedure, only: subroutine_def_node, function_def_node
        use standardizer_subprograms, only: standardize_subroutine_def, &
                                            standardize_function_def
        use standardizer_program, only: standardize_program
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer :: i, child_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (node => arena%entries(root_index)%node)
        type is (program_node)
            if (node%name /= "__MULTI_UNIT__") return
            if (.not. allocated(node%body_indices)) return

            ! CRITICAL: Copy indices before loop to prevent dangling pointer access
      ! standardize_program calls split_multi_variable_declaration which modifies arena,
       ! potentially invalidating the 'node' selector. Using a local copy prevents this.
            block
                integer, allocatable :: local_indices(:)
                allocate (local_indices(size(node%body_indices)))
                local_indices = node%body_indices

                do i = 1, size(local_indices)
                    child_index = local_indices(i)
                    if (child_index <= 0 .or. child_index > arena%size) cycle
                    if (.not. allocated(arena%entries(child_index)%node)) cycle

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
