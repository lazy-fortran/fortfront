module semantic_parameter_analysis
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, TVAR, TREAL, TDOUBLE, TINT
    use semantic_type_operations, only: get_common_type
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, call_or_subscript_node, &
                              literal_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node
    use scope_manager, only: scope_stack_t
    use semantic_literal_type_helpers, only: literal_numeric_type
    use semantic_validation_utils, only: update_identifier_type_in_arena, &
                                         int_to_str
    use semantic_type_context, only: infer_type_from_usage_context, &
                                     infer_identifier_type_from_context, &
                                     infer_expression_type_static
    use semantic_procedure_utils, only: declaration_type_to_mono
    use semantic_function_array, only: find_return_type
    implicit none
    private

    public :: merge_parameter_type
    public :: analyze_function_parameters

contains

    subroutine merge_parameter_type(current_type, candidate_type)
        type(mono_type_t), intent(inout) :: current_type
        type(mono_type_t), intent(in) :: candidate_type
        type(mono_type_t) :: merged_type

        if (candidate_type%kind <= 0) return

        if (current_type%kind <= 0) then
            current_type = candidate_type
            return
        end if

        if (current_type%kind == TVAR) then
            current_type = candidate_type
            return
        end if

        if (candidate_type%kind == TVAR) return

        if (current_type%kind == candidate_type%kind) return

        if (candidate_type%kind == TDOUBLE .and. current_type%kind == TREAL) then
            current_type = candidate_type
            return
        end if

        merged_type = get_common_type(current_type, candidate_type)
        if (merged_type%kind > 0) current_type = merged_type
    end subroutine merge_parameter_type

    subroutine fetch_parameter_metadata(arena, param_index, param_name, param_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: param_index
        character(len=64), intent(out) :: param_name
        type(mono_type_t), intent(out) :: param_type

        param_name = ''
        param_type%kind = 0

        if (param_index <= 0 .or. param_index > arena%size) return
        if (.not. allocated(arena%entries(param_index)%node)) return

        select type (param_node => arena%entries(param_index)%node)
        type is (identifier_node)
            param_name = param_node%name
        type is (parameter_declaration_node)
            param_name = param_node%name
            param_type = declaration_type_to_mono(param_node%type_name)
            if (param_type%kind == 0 .and. param_node%inferred_type%kind > 0) then
                param_type = param_node%inferred_type
            end if
        type is (declaration_node)
            param_name = param_node%var_name
            param_type = declaration_type_to_mono(param_node%type_name)
            if (param_type%kind == 0 .and. param_node%inferred_type%kind > 0) then
                param_type = param_node%inferred_type
            end if
        class default
            param_name = ''
        end select
    end subroutine fetch_parameter_metadata

    subroutine ensure_parameter_seed(param_type, param_name, next_var_id)
        type(mono_type_t), intent(inout) :: param_type
        character(len=*), intent(in) :: param_name
        integer, intent(inout) :: next_var_id
        character(len=64) :: trimmed_name

        trimmed_name = trim(param_name)

        if (param_type%kind /= TVAR) return

        if (param_type%var%id == 0) then
            param_type = create_mono_type(TVAR, &
                                          var=create_type_var(next_var_id, "arg"))
            next_var_id = next_var_id + 1
        end if

        if (len_trim(trimmed_name) == 0) return

        select case (trimmed_name(1:1))
        case ('i', 'j', 'k', 'l', 'm', 'n')
            param_type = create_mono_type(TINT)
        end select
    end subroutine ensure_parameter_seed

    subroutine collect_parameter_metadata(arena, func_node, param_types, param_names, &
                                          next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        character(len=64), allocatable, intent(inout) :: param_names(:)
        integer, intent(inout) :: next_var_id
        integer :: i
        character(len=64) :: source_name
        character(len=64) :: final_name
        type(mono_type_t) :: source_type

        do i = 1, size(param_types)
            call fetch_parameter_metadata(arena, func_node%param_indices(i), &
                                          source_name, source_type)
            final_name = trim(source_name)
            if (len_trim(final_name) == 0) final_name = 'arg' // trim(int_to_str(i))
            final_name = trim(final_name)
            if (source_type%kind == 0) then
                source_type = infer_type_from_usage_context(final_name, next_var_id)
            end if
            call ensure_parameter_seed(source_type, final_name, next_var_id)
            param_names(i) = final_name
            param_types(i) = source_type
        end do
    end subroutine collect_parameter_metadata

    subroutine infer_parameter_types_from_calls(arena, func_node, param_types, &
                                                param_names, scopes, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id
        integer :: idx
        integer :: i
        integer :: arg_idx
        type(mono_type_t) :: inferred_arg_type
        type(mono_type_t) :: literal_type
        character(len=:), allocatable :: func_name

        if (.not. allocated(func_node%name)) return
        func_name = trim(func_node%name)
        if (len_trim(func_name) == 0) return

        do idx = 1, arena%size
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (call_node => arena%entries(idx)%node)
            type is (call_or_subscript_node)
                if (.not. allocated(call_node%name)) cycle
                if (trim(call_node%name) /= func_name) cycle
                if (.not. allocated(call_node%arg_indices)) cycle
                do i = 1, min(size(call_node%arg_indices), size(param_types))
                    arg_idx = call_node%arg_indices(i)
                    if (arg_idx <= 0 .or. arg_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(arg_idx)%node)) cycle
                    select type (arg_node => arena%entries(arg_idx)%node)
                    type is (literal_node)
                        literal_type = literal_numeric_type(arg_node)
                        call merge_parameter_type(param_types(i), literal_type)
                    type is (identifier_node)
                        call merge_parameter_type(param_types(i), &
                                                  arg_node%inferred_type)
                        inferred_arg_type = &
                            infer_identifier_type_from_context( &
                            arena, arg_node%name, param_names, param_types, &
                            scopes, arg_idx, next_var_id)
                        call merge_parameter_type(param_types(i), inferred_arg_type)
                    type is (call_or_subscript_node)
                        if (allocated(arg_node%name)) then
                            if (trim(arg_node%name) == func_name) cycle
                        end if
                        inferred_arg_type = infer_expression_type_static( &
                                            arena, arg_idx, param_names, &
                                            param_types)
                        if (inferred_arg_type%kind == 0 .or. &
                            inferred_arg_type%kind == TVAR) then
                            inferred_arg_type = resolve_call_argument_type( &
                                                arena, arg_node, func_name, &
                                                next_var_id)
                        end if
                        call merge_parameter_type(param_types(i), inferred_arg_type)
                    end select
                end do
            end select
        end do
    end subroutine infer_parameter_types_from_calls

    function resolve_call_argument_type(arena, call_node, current_name, &
                                        next_var_id) &
        result(arg_type)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        character(len=*), intent(in) :: current_name
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: arg_type
        character(len=:), allocatable :: call_name
        logical :: found_return

        arg_type%kind = 0
        if (call_node%inferred_type%kind > 0) then
            arg_type = call_node%inferred_type
            return
        end if
        if (.not. allocated(call_node%name)) return
        if (call_node%is_array_access) return

        call_name = trim(call_node%name)
        if (len_trim(call_name) == 0) return
        if (len_trim(current_name) > 0) then
            if (call_name == trim(current_name)) return
        end if

        found_return = find_return_type(arena, call_name, arg_type)
        if (found_return) return

        arg_type = infer_type_from_usage_context(call_name, next_var_id)
    end function resolve_call_argument_type

    subroutine update_parameter_nodes(arena, func_node, param_types)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer :: i

        do i = 1, size(param_types)
            select type (param_node => arena%entries(func_node%param_indices(i))%node)
            type is (identifier_node)
                param_node%inferred_type = param_types(i)
                arena%entries(func_node%param_indices(i))%node = param_node
            type is (parameter_declaration_node)
                param_node%inferred_type = param_types(i)
                arena%entries(func_node%param_indices(i))%node = param_node
            type is (declaration_node)
                param_node%inferred_type = param_types(i)
                arena%entries(func_node%param_indices(i))%node = param_node
            end select
        end do
    end subroutine update_parameter_nodes

    subroutine register_parameters_in_scope(arena, param_names, param_types, scopes)
        type(ast_arena_t), intent(inout) :: arena
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer :: i
        type(poly_type_t) :: scheme

        do i = 1, size(param_types)
            if (len_trim(param_names(i)) == 0) cycle
            scheme = create_poly_type(forall_vars=[type_var_t ::], mono=param_types(i))
            call scopes%define(trim(param_names(i)), scheme)
            call update_identifier_type_in_arena(arena, trim(param_names(i)), &
                                                 param_types(i))
        end do
    end subroutine register_parameters_in_scope

    subroutine analyze_function_parameters(arena, func_node, param_types, &
                                           param_names, scopes, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(out) :: param_types(:)
        character(len=64), allocatable, intent(out) :: param_names(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id
        integer :: param_count

        if (.not. allocated(func_node%param_indices)) then
            allocate (param_types(0))
            allocate (param_names(0))
            return
        end if

        param_count = size(func_node%param_indices)

        allocate (param_types(param_count))
        allocate (param_names(param_count))

        call collect_parameter_metadata(arena, func_node, param_types, param_names, &
                                        next_var_id)
        call infer_parameter_types_from_calls(arena, func_node, param_types, &
                                              param_names, scopes, next_var_id)
        call update_parameter_nodes(arena, func_node, param_types)
        call register_parameters_in_scope(arena, param_names, param_types, scopes)
    end subroutine analyze_function_parameters

end module semantic_parameter_analysis
