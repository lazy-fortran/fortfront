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
    use standardizer_subprograms, only: wrap_function_in_program, wrap_subroutine_in_program
    use debug_trace, only: trace_enter, trace_leave
    implicit none
    private
    
    ! Type standardization configuration for standardizer
    logical, save :: standardizer_type_standardization_enabled = .true.
    
    ! Constants
    integer, parameter :: INVALID_INTEGER = -999999

    public :: standardize_ast
    ! JSON-based API removed
    public :: set_standardizer_type_standardization, &
              get_standardizer_type_standardization
    

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

end module standardizer_core
