module semantic_function_inference
    use type_system_unified, only: type_var_t, mono_type_t, create_mono_type, &
                                   create_type_var, TVAR, TREAL, TCHAR
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, &
                              call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: declaration_node
    use semantic_procedure_utils, only: detect_result_name, &
                                        declaration_type_to_mono
    use semantic_type_context, only: infer_type_from_usage_context, &
                                     infer_expression_type_static
    use semantic_array_type_builders, only: build_deferred_shape_array
    implicit none
    private

    public :: determine_function_return_type

contains

    function determine_function_return_type(arena, func_node, param_names, &
                                            param_types, next_var_id) &
        result(return_type)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: return_type
        character(len=:), allocatable :: result_name

        result_name = resolve_result_variable_name(arena, func_node)
        return_type = derive_result_type_candidate(arena, func_node, result_name, &
                                                   param_names, param_types)
        if (return_type%kind == 0) then
            return_type = fallback_result_type(func_node, result_name, next_var_id)
        end if
        call ensure_return_type_seed(return_type, next_var_id)
    end function determine_function_return_type

    function derive_result_type_candidate(arena, func_node, result_name, &
                                          param_names, param_types) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate

        candidate%kind = 0
        if (len_trim(result_name) == 0) return

        candidate = find_declared_result_type(arena, func_node, result_name)
        if (candidate%kind /= 0) return

        candidate = infer_result_type_from_assignments(arena, func_node, result_name, &
                                                       param_names, param_types)
    end function derive_result_type_candidate

    subroutine ensure_return_type_seed(return_type, next_var_id)
        type(mono_type_t), intent(inout) :: return_type
        integer, intent(inout) :: next_var_id

        if (return_type%kind /= TVAR) return
        if (return_type%var%id /= 0) return
        return_type = create_mono_type(TVAR, var=create_type_var(next_var_id, "ret"))
        next_var_id = next_var_id + 1
    end subroutine ensure_return_type_seed

    function resolve_result_variable_name(arena, func_node) result(name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=:), allocatable :: name

        if (allocated(func_node%result_variable)) then
            if (len_trim(func_node%result_variable) > 0) then
                name = trim(func_node%result_variable)
                return
            end if
        end if

        name = detect_result_name(arena, func_node)
        if (len_trim(name) == 0 .and. allocated(func_node%name)) then
            name = trim(func_node%name)
        end if
        if (len_trim(name) == 0) name = ''
    end function resolve_result_variable_name

    function find_declared_result_type(arena, func_node, result_name) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        type(mono_type_t) :: candidate
        integer :: i, stmt_index

        candidate%kind = 0
        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            stmt_index = func_node%body_indices(i)
            if (stmt_index <= 0 .or. stmt_index > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_index)%node)) cycle
            select type (stmt => arena%entries(stmt_index)%node)
            type is (declaration_node)
                if (trim(stmt%var_name) == trim(result_name)) then
                    candidate = declaration_type_to_mono(stmt%type_name)
                    if (candidate%kind /= 0) return
                end if
            end select
        end do
    end function find_declared_result_type

    function infer_result_type_from_assignments(arena, func_node, result_name, &
                                                param_names, param_types) &
        result(result_type)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: result_type

        result_type%kind = 0
        if (.not. allocated(func_node%body_indices)) return

        result_type = select_best_assignment_type( &
                      arena, func_node%body_indices, result_name, param_names, &
                      param_types)
    end function infer_result_type_from_assignments

    function select_best_assignment_type(arena, body_indices, result_name, &
                                         param_names, param_types) result(selected)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: selected
        type(mono_type_t) :: candidate
        type(mono_type_t) :: best
        integer :: i
        integer :: stmt_index

        selected%kind = 0
        best%kind = 0

        do i = 1, size(body_indices)
            stmt_index = body_indices(i)
            candidate = evaluate_result_assignment(arena, stmt_index, result_name, &
                                                   param_names, param_types)
            if (candidate%kind == 0) cycle
            if (candidate%kind /= TVAR) then
                selected = candidate
                return
            end if
            if (best%kind == 0) best = candidate
        end do

        if (best%kind /= 0) selected = best
    end function select_best_assignment_type

    function evaluate_result_assignment(arena, stmt_index, result_name, &
                                        param_names, param_types) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate

        candidate%kind = 0
        if (stmt_index <= 0 .or. stmt_index > arena%size) return
        if (.not. allocated(arena%entries(stmt_index)%node)) return

        select type (stmt => arena%entries(stmt_index)%node)
        type is (assignment_node)
            candidate = infer_assignment_result_type(arena, stmt, result_name, &
                                                     param_names, param_types)
        end select
    end function evaluate_result_assignment

    function infer_assignment_result_type(arena, stmt, result_name, param_names, &
                                          param_types) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate
        integer :: target_index

        candidate%kind = 0
        target_index = stmt%target_index
        if (target_index <= 0 .or. target_index > arena%size) return
        if (.not. allocated(arena%entries(target_index)%node)) return

        select type (target => arena%entries(target_index)%node)
        type is (identifier_node)
            if (trim(target%name) /= trim(result_name)) return
            candidate = infer_expression_type_static(arena, stmt%value_index, &
                                                     param_names, param_types)
        type is (call_or_subscript_node)
            candidate = infer_array_assignment_type(arena, target, stmt%value_index, &
                                                    result_name, param_names, &
                                                    param_types)
        end select
    end function infer_assignment_result_type

    function infer_array_assignment_type(arena, target, value_index, result_name, &
                                         param_names, param_types) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: target
        integer, intent(in) :: value_index
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate
        type(mono_type_t) :: element_type
        integer :: rank

        candidate%kind = 0
        if (.not. allocated(target%name)) return
        if (trim(target%name) /= trim(result_name)) return
        if (.not. allocated(target%arg_indices)) return
        rank = size(target%arg_indices)
        if (rank <= 0) return

        element_type = infer_expression_type_static(arena, value_index, param_names, &
                                                    param_types)
        if (element_type%kind == 0) element_type = create_mono_type(TREAL)
        candidate = build_deferred_shape_array(element_type, rank)
    end function infer_array_assignment_type

    function fallback_result_type(func_node, result_name, next_var_id) &
        result(candidate)
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: candidate
        character(len=:), allocatable :: source_name

        if (len_trim(result_name) > 0) then
            source_name = trim(result_name)
        else if (allocated(func_node%name)) then
            source_name = trim(func_node%name)
        else
            source_name = ''
        end if

        if (len_trim(source_name) > 0) then
            candidate = infer_type_from_usage_context(source_name, next_var_id)
        else
            candidate = create_mono_type(TREAL)
        end if
    end function fallback_result_type

end module semantic_function_inference
