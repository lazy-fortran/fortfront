module standardizer_core
    ! Core standardization module - main entry points and orchestration
    ! This module provides the primary interface for AST standardization
    
    use ast_core
    use ast_factory
    use type_system_unified
    use json_module, only: json_core, json_value, json_file
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result, &
                              ERROR_TYPE_SYSTEM
    use standardizer_program
    use standardizer_module  
    use standardizer_types, only: string_result_t
    implicit none
    private
    
    ! Type standardization configuration for standardizer
    logical, save :: standardizer_type_standardization_enabled = .true.
    
    ! Constants
    integer, parameter :: INVALID_INTEGER = -999999

    public :: standardize_ast
    public :: standardize_ast_json
    public :: set_standardizer_type_standardization, &
              get_standardizer_type_standardization
    
    ! Forward declarations for procedures in submodules
    interface
        subroutine wrap_function_in_program(arena, func_index)
            import :: ast_arena_t
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(inout) :: func_index
        end subroutine wrap_function_in_program
        
        subroutine wrap_subroutine_in_program(arena, sub_index)
            import :: ast_arena_t
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(inout) :: sub_index
        end subroutine wrap_subroutine_in_program
    end interface

contains

    ! Main standardization entry point
    recursive subroutine standardize_ast(arena, root_index, in_module)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        logical, intent(in), optional :: in_module
        integer :: i
        logical :: is_in_module

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        ! Determine if we're inside a module context
        is_in_module = .false.
        if (present(in_module)) is_in_module = in_module

        select type (node => arena%entries(root_index)%node)
        type is (program_node)
            ! Skip standardization for multi-unit containers
            if (node%name == "__MULTI_UNIT__") then
                ! Standardize each unit individually
                if (allocated(node%body_indices)) then
                    block
                        integer :: j
                        do j = 1, size(node%body_indices)
                        if (node%body_indices(j) > 0 .and. &
                            node%body_indices(j) <= arena%size) then
                            call standardize_ast(arena, node%body_indices(j))
                        end if
                    end do
                    end block
                end if
            else
                call standardize_program(arena, node, root_index)
            end if
        type is (function_def_node)
            ! Only wrap standalone functions in a program, skip if inside module
            if (.not. is_in_module) then
                call wrap_function_in_program(arena, root_index)
            end if
        type is (subroutine_def_node)
            ! Only wrap standalone subroutines in a program, skip if inside module
            if (.not. is_in_module) then
                call wrap_subroutine_in_program(arena, root_index)
            end if
        type is (module_node)
            ! Modules don't need wrapping - standardize their contents
            call standardize_module(arena, node, root_index)
        class default
            ! For other node types, no standardization needed yet
        end select

    end subroutine standardize_ast

    ! JSON interface for standardization
    subroutine standardize_ast_json(json_file_path, output_file, error_msg)
        character(len=*), intent(in) :: json_file_path, output_file
        character(len=:), allocatable, intent(out) :: error_msg
        type(json_file) :: ast_json
        type(json_core) :: json
        type(json_value), pointer :: root_ptr
        type(ast_arena_t) :: arena
        integer :: root_index
        logical :: found
        
        ! Load JSON and reconstruct AST (implementation would go here)
        ! This is a stub for the JSON interface
        error_msg = "JSON standardization not yet implemented"
    end subroutine standardize_ast_json

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