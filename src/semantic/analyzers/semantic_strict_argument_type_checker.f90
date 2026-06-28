module semantic_strict_argument_type_checker
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
        subroutine_def_node
    use error_handling, only: error_collection_t
    use semantic_strict_argument_type_checker_resolution, only: &
        find_function_interface_index, find_subroutine_interface_index
    use semantic_strict_argument_type_checker_validation, only: &
        validate_call_against_interface
    implicit none
    private

    public :: validate_strict_argument_types_for_function_reference
    public :: validate_strict_argument_types_for_subroutine_call

contains

    subroutine validate_strict_argument_types_for_function_reference(arena, errors, &
            call_node, &
            call_index)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(call_or_subscript_node), intent(in) :: call_node
        integer, intent(in) :: call_index
        integer :: iface_index

        if (.not. allocated(call_node%name)) return
        if (len_trim(call_node%name) == 0) return
        if (call_node%base_expr_index /= 0) return

        call find_function_interface_index(arena, call_node%name, call_index, &
            iface_index)
        if (iface_index <= 0) return
        if (.not. allocated(arena%entries(iface_index)%node)) return
        select type (iface => arena%entries(iface_index)%node)
            type is (function_def_node)
            call validate_call_against_interface(arena, errors, call_node%name, &
                call_node%arg_indices, &
                iface%param_indices, &
                iface%body_indices)
        class default
            return
        end select
    end subroutine validate_strict_argument_types_for_function_reference

    subroutine validate_strict_argument_types_for_subroutine_call(arena, errors, &
            call_node, &
            call_index)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: call_node
        integer, intent(in) :: call_index
        integer :: iface_index

        if (.not. allocated(call_node%name)) return
        if (len_trim(call_node%name) == 0) return

        call find_subroutine_interface_index(arena, call_node%name, call_index, &
            iface_index)
        if (iface_index <= 0) return
        if (.not. allocated(arena%entries(iface_index)%node)) return
        select type (iface => arena%entries(iface_index)%node)
            type is (subroutine_def_node)
            call validate_call_against_interface(arena, errors, call_node%name, &
                call_node%arg_indices, &
                iface%param_indices, &
                iface%body_indices)
        class default
            return
        end select
    end subroutine validate_strict_argument_types_for_subroutine_call

end module semantic_strict_argument_type_checker
