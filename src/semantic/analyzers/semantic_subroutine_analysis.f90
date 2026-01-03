module semantic_subroutine_analysis
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, TVAR
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, literal_node, call_or_subscript_node
    use ast_nodes_procedure, only: subroutine_def_node, subroutine_call_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use scope_manager, only: scope_stack_t
    use semantic_validation_utils, only: update_identifier_type_in_arena, int_to_str
    use semantic_literal_type_helpers, only: literal_numeric_type
    use semantic_function_analysis, only: infer_type_from_usage_context, &
                                          merge_parameter_type, &
                                          infer_identifier_type_from_context, &
                                          infer_expression_type_static
    use semantic_procedure_utils, only: declaration_type_to_mono
    implicit none
    private

    public :: analyze_subroutine_parameters
    public :: create_subroutine_scope

contains

    subroutine extract_subroutine_param_info(arena, sub_node, param_types, &
                                             stored_names, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(in) :: sub_node
        type(mono_type_t), allocatable, intent(out) :: param_types(:)
        character(len=64), allocatable, intent(out) :: stored_names(:)
        integer, intent(inout) :: next_var_id

        integer :: i
        type(mono_type_t) :: temp_type
        character(len=64) :: param_name
        character(len=64) :: stored_name

        allocate (param_types(size(sub_node%param_indices)))
        allocate (stored_names(size(sub_node%param_indices)))

        do i = 1, size(sub_node%param_indices)
            call gather_subroutine_param_seed(arena, sub_node%param_indices(i), &
                                              param_name, temp_type)
            stored_name = derive_stored_param_name(param_name, i)
            stored_names(i) = stored_name
            call initialize_param_type(param_types(i), stored_name, temp_type, &
                                       next_var_id)
        end do
    end subroutine extract_subroutine_param_info

    subroutine gather_subroutine_param_seed(arena, entry_index, param_name, seed_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: entry_index
        character(len=64), intent(out) :: param_name
        type(mono_type_t), intent(out) :: seed_type

        param_name = ''
        seed_type%kind = 0
        if (.not. arena%has_node_at(entry_index)) return

        select type (arg => arena%entries(entry_index)%node)
        type is (identifier_node)
            if (allocated(arg%name)) then
                param_name = arg%name
                seed_type = arg%inferred_type
            end if
        type is (parameter_declaration_node)
            if (allocated(arg%name)) then
                param_name = arg%name
                seed_type = arg%inferred_type
            end if
        type is (declaration_node)
            if (allocated(arg%var_name)) then
                param_name = arg%var_name
                if (allocated(arg%type_name) .and. len_trim(arg%type_name) > 0) then
                    seed_type = declaration_type_to_mono(arg%type_name)
                else
                    seed_type = arg%inferred_type
                end if
            end if
        end select
    end subroutine gather_subroutine_param_seed

    function derive_stored_param_name(candidate_name, index) result(resolved_name)
        character(len=*), intent(in) :: candidate_name
        integer, intent(in) :: index
        character(len=64) :: resolved_name
        character(len=:), allocatable :: fallback

        resolved_name = trim(candidate_name)
        if (len_trim(resolved_name) > 0) return
        fallback = 'arg' // trim(int_to_str(index))
        resolved_name = fallback
    end function derive_stored_param_name

    subroutine initialize_param_type(param_type, param_name, seed_type, next_var_id)
        type(mono_type_t), intent(out) :: param_type
        character(len=*), intent(in) :: param_name
        type(mono_type_t), intent(in) :: seed_type
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: inferred_arg_type

        if (seed_type%kind == 0) then
            if (len_trim(param_name) > 0) then
                inferred_arg_type = infer_type_from_usage_context(param_name, &
                                                                  next_var_id)
                param_type = inferred_arg_type
            else
                param_type = create_mono_type(TVAR, &
                                              var=create_type_var(next_var_id, "p"))
                next_var_id = next_var_id + 1
            end if
        else
            param_type = seed_type
        end if
    end subroutine initialize_param_type

    subroutine update_subroutine_param_nodes(arena, sub_node, param_types, &
                                             stored_names)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(in) :: sub_node
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        character(len=64), allocatable, intent(inout) :: stored_names(:)

        integer :: i, idx

        do i = 1, size(param_types)
            idx = sub_node%param_indices(i)
            if (.not. arena%has_node_at(idx)) cycle

            select type (arg => arena%entries(idx)%node)
            type is (parameter_declaration_node)
                if (allocated(arg%name)) then
                    stored_names(i) = trim(arg%name)
                end if
            type is (declaration_node)
                if (allocated(arg%var_name)) then
                    stored_names(i) = trim(arg%var_name)
                end if
            type is (identifier_node)
                if (allocated(arg%name)) then
                    stored_names(i) = trim(arg%name)
                end if
            end select
        end do

        do i = 1, size(param_types)
            if (sub_node%param_indices(i) <= 0 .or. sub_node%param_indices(i) > &
                & arena%size) cycle
            if (.not. allocated(arena%entries(sub_node%param_indices(i))%node)) cycle
            select type (param_node => arena%entries(sub_node%param_indices(i))%node)
            type is (identifier_node)
                param_node%inferred_type = param_types(i)
                arena%entries(sub_node%param_indices(i))%node = param_node
            type is (parameter_declaration_node)
                param_node%inferred_type = param_types(i)
                arena%entries(sub_node%param_indices(i))%node = param_node
            type is (declaration_node)
                param_node%inferred_type = param_types(i)
                arena%entries(sub_node%param_indices(i))%node = param_node
            end select
        end do
    end subroutine update_subroutine_param_nodes

    subroutine infer_subroutine_param_types_from_calls(arena, subroutine_name, &
                                                       param_names, param_types, &
                                                       scopes, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: subroutine_name
        character(len=64), allocatable, intent(inout) :: param_names(:)
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id

        integer :: call_idx

        do call_idx = 1, arena%size
            call update_param_types_from_call(arena, call_idx, subroutine_name, &
                                              param_names, param_types, scopes, &
                                              next_var_id)
        end do
    end subroutine infer_subroutine_param_types_from_calls

    subroutine update_param_types_from_call(arena, call_idx, subroutine_name, &
                                            param_names, param_types, scopes, &
                                            next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: call_idx
        character(len=*), intent(in) :: subroutine_name
        character(len=64), allocatable, intent(inout) :: param_names(:)
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id
        integer :: arg_limit
        integer :: i
        integer :: arg_idx

        if (.not. allocated(arena%entries(call_idx)%node)) return

        select type (call_node => arena%entries(call_idx)%node)
        type is (subroutine_call_node)
            if (.not. allocated(call_node%name)) return
            if (trim(call_node%name) /= subroutine_name) return
            if (.not. allocated(call_node%arg_indices)) return

            arg_limit = min(size(call_node%arg_indices), size(param_types))
            do i = 1, arg_limit
                arg_idx = call_node%arg_indices(i)
                call update_param_type_from_argument(arena, arg_idx, i, &
                                                     param_names, param_types, &
                                                     scopes, next_var_id)
            end do
        end select
    end subroutine update_param_types_from_call

    subroutine update_param_type_from_argument(arena, arg_idx, param_index, &
                                               param_names, param_types, scopes, &
                                               next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: arg_idx
        integer, intent(in) :: param_index
        character(len=64), allocatable, intent(inout) :: param_names(:)
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: inferred_arg_type
        type(mono_type_t) :: literal_type

        if (param_index <= 0) return
        if (param_index > size(param_types)) return
        if (.not. arena%has_node_at(arg_idx)) return

        select type (arg_node => arena%entries(arg_idx)%node)
        type is (literal_node)
            literal_type = literal_numeric_type(arg_node)
            call merge_parameter_type(param_types(param_index), literal_type)
        type is (identifier_node)
            call merge_parameter_type(param_types(param_index), arg_node%inferred_type)
            inferred_arg_type = infer_identifier_type_from_context(arena, &
                                                                   arg_node%name, &
                                                                   param_names, &
                                                                   param_types, &
                                                                   scopes, &
                                                                   arg_idx, &
                                                                   next_var_id)
            call merge_parameter_type(param_types(param_index), inferred_arg_type)
        type is (call_or_subscript_node)
            call merge_parameter_type(param_types(param_index), arg_node%inferred_type)
        end select

        inferred_arg_type = infer_expression_type_static(arena, arg_idx, param_names, &
                                                         param_types)
        if (inferred_arg_type%kind /= 0) then
            call merge_parameter_type(param_types(param_index), inferred_arg_type)
        end if
    end subroutine update_param_type_from_argument

    subroutine analyze_subroutine_parameters(arena, sub_node, param_types, &
                                             param_names, scopes, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(in) :: sub_node
        type(mono_type_t), allocatable, intent(out) :: param_types(:)
        character(len=64), allocatable, intent(out) :: param_names(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id

        integer :: i
        type(poly_type_t) :: scheme
        character(len=64), allocatable :: stored_names(:)
        character(:), allocatable :: subroutine_name

        if (.not. allocated(sub_node%param_indices)) then
            allocate (param_types(0))
            allocate (param_names(0))
            return
        end if

        call extract_subroutine_param_info(arena, sub_node, param_types, &
                                           stored_names, next_var_id)

        if (allocated(sub_node%name)) then
            subroutine_name = trim(sub_node%name)
        else
            subroutine_name = ''
        end if

        if (len_trim(subroutine_name) > 0) then
            call infer_subroutine_param_types_from_calls(arena, subroutine_name, &
                                                         stored_names, param_types, &
                                                         scopes, next_var_id)
        end if

        call update_subroutine_param_nodes(arena, sub_node, param_types, stored_names)

        do i = 1, size(param_types)
            if (len_trim(stored_names(i)) == 0) cycle
            scheme = create_poly_type(forall_vars=[type_var_t ::], mono=param_types(i))
            call scopes%define(trim(stored_names(i)), scheme)
            call update_identifier_type_in_arena( &
                arena, trim(stored_names(i)), param_types(i))
        end do

        param_names = stored_names

    end subroutine analyze_subroutine_parameters

    subroutine create_subroutine_scope(arena, sub_node, sub_index, scopes)
        type(ast_arena_t), intent(inout) :: arena
        type(subroutine_def_node), intent(in) :: sub_node
        integer, intent(in) :: sub_index
        type(scope_stack_t), intent(inout) :: scopes
        character(len=:), allocatable :: sub_name

        if (allocated(sub_node%name)) then
            sub_name = trim(sub_node%name)
        else
            sub_name = 'anonymous_subroutine'
        end if

        call scopes%enter_function(sub_name)
    end subroutine create_subroutine_scope

end module semantic_subroutine_analysis
