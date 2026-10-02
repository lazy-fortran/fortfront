module semantic_definition_context_validation
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
        identifier_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_procedure, only: subroutine_call_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use frontend_compiler_resolution, only: declaration_binding_t, &
        resolve_name_at_node, BINDING_NAMED_CONSTANT
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: validate_definition_contexts

contains

    subroutine validate_definition_contexts(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (assignment_node)
                if (node%is_keyword_argument) cycle
                ! DATA expansion assignments are hidden implementation nodes;
                ! the DATA validator owns their initialization diagnostics.
                if (node%suppress_codegen) cycle
                if (is_actual_argument(arena, i)) cycle
                call check_assignment_target(arena, i, node%target_index, errors)
                type is (do_loop_node)
                if (.not. allocated(node%var_name)) cycle
                call check_constant_definition(arena, i, node%var_name, errors)
                type is (call_or_subscript_node)
                call check_character_inquiry_arity(arena, i, node, errors)
            end select
        end do
    end subroutine validate_definition_contexts

    logical function is_actual_argument(arena, node_index) result(is_actual)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: parent_index

        is_actual = .false.
        parent_index = arena%entries(node_index)%parent_index
        if (.not. arena%has_node_at(parent_index)) return
        select type (parent => arena%entries(parent_index)%node)
            type is (call_or_subscript_node)
            if (.not. allocated(parent%arg_indices)) return
            is_actual = any(parent%arg_indices == node_index)
            type is (subroutine_call_node)
            if (.not. allocated(parent%arg_indices)) return
            is_actual = any(parent%arg_indices == node_index)
        end select
    end function is_actual_argument

    subroutine check_assignment_target(arena, statement_index, target_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: statement_index, target_index
        type(error_collection_t), intent(inout) :: errors

        if (.not. arena%has_node_at(target_index)) return
        select type (target => arena%entries(target_index)%node)
            type is (identifier_node)
            if (.not. allocated(target%name)) return
            call check_constant_definition(arena, statement_index, target%name, errors)
            type is (call_or_subscript_node)
            if (target%base_expr_index /= 0) return
            if (.not. allocated(target%name)) return
            call check_constant_definition(arena, statement_index, target%name, errors)
        end select
    end subroutine check_assignment_target

    subroutine check_constant_definition(arena, statement_index, name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: statement_index
        character(len=*), intent(in) :: name
        type(error_collection_t), intent(inout) :: errors
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg

        call resolve_name_at_node(arena, statement_index, name, binding, error_msg)
        if (len_trim(error_msg) > 0) return
        if (.not. binding%found) return
        if (binding%binding_kind /= BINDING_NAMED_CONSTANT) return
        call errors%add_error("Named constant '"//trim(name)// &
            "' cannot appear in a variable definition context", &
            severity=ERROR_SEMANTIC, component="semantic_definition_context", &
            line=arena%entries(statement_index)%node%line, &
            column=arena%entries(statement_index)%node%column)
    end subroutine check_constant_definition

    subroutine check_character_inquiry_arity(arena, node_index, node, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(call_or_subscript_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: name, error_msg
        integer :: argument_count

        if (.not. allocated(node%name)) return
        name = to_lower(trim(node%name))
        if (name /= 'len') then
            if (name /= 'len_trim') return
        end if
        if (node%base_expr_index /= 0) return
        call resolve_name_at_node(arena, node_index, name, binding, error_msg)
        if (len_trim(error_msg) > 0) return
        ! A source-defined procedure, array or generic with this spelling
        ! owns its own argument contract.
        if (binding%found) return
        argument_count = 0
        if (allocated(node%arg_indices)) argument_count = size(node%arg_indices)
        if (argument_count >= 1 .and. argument_count <= 2) return
        call errors%add_error('Intrinsic '//name// &
            ' requires one or two arguments (STRING [, KIND])', &
            severity=ERROR_SEMANTIC, component="semantic_intrinsic_arity", &
            line=node%line, column=node%column)
    end subroutine check_character_inquiry_arity

end module semantic_definition_context_validation
