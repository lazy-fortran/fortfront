module semantic_parameter_analysis
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
        create_mono_type, create_type_var, &
        create_poly_type, empty_type_vars, TVAR, TREAL, TDOUBLE, &
        TINT, TCHAR
    use type_array_safe, only: safe_extract_array_rank, safe_peel_array_to_base
    use semantic_type_operations, only: get_common_type
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, call_or_subscript_node, &
        literal_node, binary_op_node, assignment_node, &
        program_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
        module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use scope_manager, only: scope_stack_t
    use semantic_literal_type_helpers, only: literal_numeric_type
    use semantic_validation_utils, only: update_identifier_type_in_arena, &
        int_to_str
    use semantic_type_context, only: infer_type_from_usage_context, &
        infer_identifier_type_from_context, &
        infer_expression_type_static
    use semantic_procedure_utils, only: declaration_type_to_mono
    use semantic_function_helpers, only: find_return_type
    use ast_base, only: LITERAL_STRING
    implicit none
    private

    public :: merge_parameter_type
    public :: analyze_function_parameters
    public :: refine_parameters_from_body_usage

contains

    subroutine merge_parameter_type(current_type, candidate_type)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(inout) :: current_type
        type(mono_type_t), intent(in) :: candidate_type
        type(mono_type_t) :: merged_type
        type(mono_type_t) :: base_current, base_candidate
        integer :: rank_current, rank_candidate

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

        ! Prefer array types over scalar types (fixes #2062)
        if (candidate_type%kind == TARRAY .and. current_type%kind /= TARRAY) then
            current_type = candidate_type
            return
        end if

        ! Merge arrays of different ranks (take maximum rank)
        if (current_type%kind == TARRAY .and. candidate_type%kind == TARRAY) then
            call extract_rank_and_base_type(current_type, rank_current, base_current)
            call extract_rank_and_base_type(candidate_type, rank_candidate, &
                base_candidate)
            if (rank_current == rank_candidate) then
                if (base_current%kind == base_candidate%kind) then
                    if (base_current%kind /= TCHAR .or. &
                        base_current%size == base_candidate%size) then
                        return
                    end if
                end if
            end if
            merged_type = merge_array_types(current_type, candidate_type)
            if (merged_type%kind > 0) current_type = merged_type
            return
        end if

        if (current_type%kind == candidate_type%kind) return

        if (candidate_type%kind == TDOUBLE .and. current_type%kind == TREAL) then
            current_type = candidate_type
            return
        end if

        merged_type = get_common_type(current_type, candidate_type)
        if (merged_type%kind > 0) current_type = merged_type
    end subroutine merge_parameter_type

    function merge_array_types(type1, type2) result(merged)
        use type_system_unified, only: TARRAY
        use semantic_array_type_builders, only: build_deferred_shape_array
        type(mono_type_t), intent(in) :: type1, type2
        type(mono_type_t) :: merged
        type(mono_type_t) :: base1, base2, common_base
        integer :: rank1, rank2, max_rank

        call extract_rank_and_base_type(type1, rank1, base1)
        call extract_rank_and_base_type(type2, rank2, base2)

        max_rank = max(rank1, rank2)

        common_base = get_common_type(base1, base2)
        if (common_base%kind <= 0) common_base = base1

        merged = build_deferred_shape_array(common_base, max_rank)
    end function merge_array_types

    subroutine extract_rank_and_base_type(array_type, rank, base_type)
        type(mono_type_t), intent(in) :: array_type
        integer, intent(out) :: rank
        type(mono_type_t), intent(out) :: base_type

        call safe_extract_array_rank(array_type, rank, base_type)
    end subroutine extract_rank_and_base_type

    subroutine fetch_parameter_metadata(arena, param_index, param_name, param_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: param_index
        character(len=64), intent(out) :: param_name
        type(mono_type_t), intent(out) :: param_type

        param_name = ''
        param_type%kind = 0

        if (.not. arena%has_node_at(param_index)) return

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
        case ('i', 'j', 'k', 'l', 'n')
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
            if (len_trim(final_name) == 0) final_name = 'arg'//trim(int_to_str(i))
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
                    if (.not. arena%has_node_at(arg_idx)) cycle
                    select type (arg_node => arena%entries(arg_idx)%node)
                        type is (literal_node)
                        if (arg_node%literal_kind == LITERAL_STRING) then
                            literal_type = create_mono_type(TCHAR)
                        else
                            literal_type = literal_numeric_type(arg_node)
                        end if
                        call merge_parameter_type(param_types(i), literal_type)
                        type is (identifier_node)
                        call merge_parameter_type(param_types(i), &
                            arg_node%inferred_type)
                        inferred_arg_type = &
                            infer_identifier_type_from_context( &
                            arena, arg_node%name, param_names, param_types, &
                            scopes, arg_idx, next_var_id)
                        if (inferred_arg_type%kind == 0 .or. &
                            inferred_arg_type%kind == TVAR) then
                            block
                                integer :: enclosing_func_idx
                                enclosing_func_idx = &
                                    find_function_containing_call(arena, idx)
                                if (enclosing_func_idx > 0) then
                                    inferred_arg_type = &
                                        infer_type_from_enclosing_function_param( &
                                        arena, arg_node%name, enclosing_func_idx, &
                                        next_var_id)
                                end if
                            end block
                        end if
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
                        type is (binary_op_node)
                        ! An expression argument (e.g. `twice(1.0d0/6.0d0)`)
                        ! carries the real type through to the dummy (issue #2980).
                        inferred_arg_type = infer_expression_type_static( &
                            arena, arg_idx, param_names, &
                            param_types)
                        call merge_parameter_type(param_types(i), inferred_arg_type)
                    end select
                end do
            end select
        end do
    end subroutine infer_parameter_types_from_calls

    function find_function_containing_call(arena, call_index) result(func_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_index
        integer :: func_index
        integer :: i

        func_index = 0

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (func_node => arena%entries(i)%node)
                type is (function_def_node)
                if (.not. allocated(func_node%body_indices)) cycle
                if (call_is_in_function_body(arena, call_index, func_node)) then
                    func_index = i
                    return
                end if
            end select
        end do
    end function find_function_containing_call

    recursive logical function call_is_in_function_body(arena, call_index, func_node) &
            result(is_in_body)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_index
        type(function_def_node), intent(in) :: func_node
        integer :: i, stmt_idx

        is_in_body = .false.

        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            stmt_idx = func_node%body_indices(i)
            if (stmt_idx == call_index) then
                is_in_body = .true.
                return
            end if

            if (.not. arena%has_node_at(stmt_idx)) cycle
            if (node_contains_call(arena, stmt_idx, call_index)) then
                is_in_body = .true.
                return
            end if
        end do
    end function call_is_in_function_body

    recursive logical function node_contains_call(arena, node_idx, call_index) &
            result(contains_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx, call_index
        integer :: i

        contains_call = .false.

        if (node_idx == call_index) then
            contains_call = .true.
            return
        end if

        if (.not. arena%has_node_at(node_idx)) return

        select type (node => arena%entries(node_idx)%node)
            type is (assignment_node)
            if (node%value_index == call_index .or. &
                node%target_index == call_index) then
                contains_call = .true.
                return
            end if
            if (node%value_index > 0) then
                if (node_contains_call(arena, node%value_index, call_index)) then
                    contains_call = .true.
                    return
                end if
            end if
            type is (binary_op_node)
            if (node%left_index == call_index .or. node%right_index == call_index) &
                then
                contains_call = .true.
                return
            end if
            if (node%left_index > 0) then
                if (node_contains_call(arena, node%left_index, call_index)) then
                    contains_call = .true.
                    return
                end if
            end if
            if (node%right_index > 0) then
                if (node_contains_call(arena, node%right_index, call_index)) then
                    contains_call = .true.
                    return
                end if
            end if
            type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) == call_index) then
                        contains_call = .true.
                        return
                    end if
                end do
            end if
        end select
    end function node_contains_call

    function infer_type_from_enclosing_function_param(arena, identifier_name, &
            enclosing_func_index, &
            next_var_id) &
            result(inferred_type)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: identifier_name
        integer, intent(in) :: enclosing_func_index
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: inferred_type
        type(mono_type_t), allocatable :: outer_param_types(:)
        character(len=64), allocatable :: outer_param_names(:)
        character(len=:), allocatable :: outer_func_name
        integer :: i
        integer :: idx
        integer :: arg_idx
        type(mono_type_t) :: literal_type

        inferred_type%kind = 0

        if (enclosing_func_index <= 0 .or. enclosing_func_index > arena%size) &
            return
        if (.not. allocated(arena%entries(enclosing_func_index)%node)) return

        select type (outer_func => arena%entries(enclosing_func_index)%node)
            type is (function_def_node)
            if (.not. allocated(outer_func%param_indices)) return
            if (.not. allocated(outer_func%name)) return
            outer_func_name = trim(outer_func%name)

            allocate (outer_param_types(size(outer_func%param_indices)))
            allocate (outer_param_names(size(outer_func%param_indices)))

            do i = 1, size(outer_func%param_indices)
                call fetch_parameter_metadata(arena, outer_func%param_indices(i), &
                    outer_param_names(i), &
                    outer_param_types(i))
                if (trim(outer_param_names(i)) == trim(identifier_name)) then
                    do idx = 1, arena%size
                        if (.not. allocated(arena%entries(idx)%node)) cycle
                        select type (call_node => arena%entries(idx)%node)
                            type is (call_or_subscript_node)
                            if (.not. allocated(call_node%name)) cycle
                            if (trim(call_node%name) /= outer_func_name) cycle
                            if (.not. allocated(call_node%arg_indices)) cycle
                            if (i > size(call_node%arg_indices)) cycle

                            arg_idx = call_node%arg_indices(i)
                            if (.not. arena%has_node_at(arg_idx)) cycle

                            select type (arg_node => arena%entries(arg_idx)%node)
                                type is (literal_node)
                                if (arg_node%literal_kind == LITERAL_STRING) then
                                    literal_type = create_mono_type(TCHAR)
                                else
                                    literal_type = literal_numeric_type(arg_node)
                                end if
                                if (literal_type%kind > 0) then
                                    inferred_type = literal_type
                                    if (allocated(outer_param_types)) &
                                        deallocate (outer_param_types)
                                    if (allocated(outer_param_names)) &
                                        deallocate (outer_param_names)
                                    return
                                end if
                            end select
                        end select
                    end do
                end if
            end do

            if (allocated(outer_param_types)) deallocate (outer_param_types)
            if (allocated(outer_param_names)) deallocate (outer_param_names)
        end select
    end function infer_type_from_enclosing_function_param

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
            scheme = create_poly_type(forall_vars=empty_type_vars(), mono=param_types(i))
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
        call refine_parameters_from_body_usage(arena, func_node, param_types, &
            param_names)
        call update_parameter_nodes(arena, func_node, param_types)
        call register_parameters_in_scope(arena, param_names, param_types, scopes)
    end subroutine analyze_function_parameters

    function infer_base_type_from_call_site(arena, func_node, param_position) &
            result(base_type)
        use type_system_unified, only: TARRAY, &
            type_args_size, type_args_element
        use semantic_type_context, only: infer_expression_type_static
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: param_position
        type(mono_type_t) :: base_type
        character(len=:), allocatable :: func_name
        character(len=:), allocatable :: arg_name
        integer :: idx, arg_idx, assign_idx
        integer :: call_scope_index
        type(mono_type_t) :: arg_type
        type(mono_type_t), allocatable :: empty_types(:)
        character(len=64), allocatable :: empty_names(:)

        base_type%kind = 0
        allocate (empty_types(0))
        allocate (empty_names(0))

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
                if (param_position > size(call_node%arg_indices)) cycle

                arg_idx = call_node%arg_indices(param_position)
                if (.not. arena%has_node_at(arg_idx)) cycle
                call_scope_index = find_enclosing_scope_index(arena, idx)
                if (call_scope_index <= 0) cycle

                select type (arg_node => arena%entries(arg_idx)%node)
                    type is (identifier_node)
                    arg_type = arg_node%inferred_type
                    if (arg_type%kind == TARRAY) then
                        base_type = extract_array_base_type(arg_type)
                        if (base_type%kind > 0) then
                            deallocate (empty_types)
                            deallocate (empty_names)
                            return
                        end if
                    else if (arg_type%kind > 0 .and. arg_type%kind /= TVAR) then
                        if (.not. allocated(arg_node%name)) cycle
                        arg_name = trim(arg_node%name)
                        assign_idx = find_assignment_to_variable(arena, arg_name, &
                            call_scope_index)
                        if (assign_idx > 0) then
                            arg_type = infer_expression_type_static(arena, &
                                assign_idx, &
                                empty_names, &
                                empty_types)
                            if (arg_type%kind == TARRAY) then
                                base_type = extract_array_base_type(arg_type)
                            else
                                base_type = arg_type
                            end if
                            if (base_type%kind > 0) then
                                deallocate (empty_types)
                                deallocate (empty_names)
                                return
                            end if
                        end if
                        base_type = arg_type
                        deallocate (empty_types)
                        deallocate (empty_names)
                        return
                    else
                        if (.not. allocated(arg_node%name)) cycle
                        arg_name = trim(arg_node%name)
                        assign_idx = find_assignment_to_variable(arena, arg_name, &
                            call_scope_index)
                        if (assign_idx > 0) then
                            arg_type = infer_expression_type_static(arena, &
                                assign_idx, &
                                empty_names, &
                                empty_types)
                            if (arg_type%kind == TARRAY) then
                                base_type = extract_array_base_type(arg_type)
                            else if (arg_type%kind > 0) then
                                base_type = arg_type
                            end if
                            if (base_type%kind > 0) then
                                deallocate (empty_types)
                                deallocate (empty_names)
                                return
                            end if
                        end if
                    end if
                end select
            end select
        end do

        deallocate (empty_types)
        deallocate (empty_names)
    end function infer_base_type_from_call_site

    function find_assignment_to_variable(arena, var_name, scope_index) &
            result(value_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: scope_index
        integer :: value_index
        integer :: idx

        value_index = 0
        if (scope_index <= 0) return

        do idx = 1, arena%size
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (assign_node => arena%entries(idx)%node)
                type is (assignment_node)
                if (.not. node_within_scope(arena, idx, scope_index)) cycle
                if (assign_node%target_index <= 0) cycle
                if (assign_node%target_index > arena%size) cycle
                if (.not. allocated(arena%entries(assign_node%target_index)%node)) &
                    cycle
                select type (target_node => &
                        arena%entries(assign_node%target_index)%node)
                    type is (identifier_node)
                    if (.not. allocated(target_node%name)) cycle
                    if (trim(target_node%name) == trim(var_name)) then
                        value_index = assign_node%value_index
                        return
                    end if
                end select
            end select
        end do
    end function find_assignment_to_variable

    integer function find_enclosing_scope_index(arena, node_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        scope_index = 0
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (.not. allocated(arena%entries(current)%node)) exit
            select type (scope_node => arena%entries(current)%node)
                type is (function_def_node)
                scope_index = current
                return
                type is (subroutine_def_node)
                scope_index = current
                return
                type is (program_node)
                scope_index = current
                return
            end select
            current = arena%entries(current)%parent_index
        end do
    end function find_enclosing_scope_index

    logical function node_within_scope(arena, node_index, scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_index
        integer :: current

        node_within_scope = .false.
        if (scope_index <= 0) return
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (current == scope_index) then
                node_within_scope = .true.
                return
            end if
            current = arena%entries(current)%parent_index
        end do
    end function node_within_scope

    function extract_array_base_type(array_type) result(base_type)
        type(mono_type_t), intent(in) :: array_type
        type(mono_type_t) :: base_type

        base_type = safe_peel_array_to_base(array_type)
    end function extract_array_base_type

    subroutine refine_parameters_from_body_usage(arena, func_node, param_types, &
            param_names)
        use type_system_unified, only: TARRAY
        use semantic_array_type_builders, only: build_deferred_shape_array
        use intrinsic_registry, only: is_intrinsic_function
        use string_utils_mod, only: to_lower
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(inout) :: param_types(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        integer :: i, j, arg_idx
        character(len=:), allocatable :: intrinsic_name, arg_name
        type(mono_type_t) :: array_type, element_type

        if (.not. allocated(func_node%body_indices)) return
        if (.not. allocated(param_names)) return
        if (.not. allocated(param_types)) return

        do i = 1, size(func_node%body_indices)
            call refine_from_statement(arena, func_node%body_indices(i), &
                param_types, param_names)
        end do

    contains

        recursive subroutine refine_from_statement(arena, stmt_idx, param_types, &
                param_names)
            use ast_nodes_loops, only: do_loop_node
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: stmt_idx
            type(mono_type_t), allocatable, intent(inout) :: param_types(:)
            character(len=64), allocatable, intent(in) :: param_names(:)
            integer :: k

            if (.not. arena%has_node_at(stmt_idx)) return

            select type (node => arena%entries(stmt_idx)%node)
                type is (call_or_subscript_node)
                call refine_from_intrinsic_call(node)
                call refine_from_array_indexing(node)
                type is (assignment_node)
                if (node%value_index > 0) then
                    call refine_from_statement(arena, node%value_index, &
                        param_types, param_names)
                end if
                type is (binary_op_node)
                if (node%left_index > 0) then
                    call refine_from_statement(arena, node%left_index, &
                        param_types, param_names)
                end if
                if (node%right_index > 0) then
                    call refine_from_statement(arena, node%right_index, &
                        param_types, param_names)
                end if
                type is (do_loop_node)
                if (allocated(node%body_indices)) then
                    do k = 1, size(node%body_indices)
                        call refine_from_statement(arena, node%body_indices(k), &
                            param_types, param_names)
                    end do
                end if
            end select
        end subroutine refine_from_statement

        subroutine refine_from_intrinsic_call(call_node)
            type(call_or_subscript_node), intent(in) :: call_node
            character(len=:), allocatable :: lowered_name
            integer :: arg_idx, param_idx
            character(len=:), allocatable :: arg_name
            type(mono_type_t) :: inferred_array_type, base_type
            integer :: rank

            if (.not. allocated(call_node%name)) return
            if (.not. is_intrinsic_function(call_node%name)) return

            lowered_name = to_lower(trim(call_node%name))

            select case (lowered_name)
            case ("size", "lbound", "ubound", "shape")
                if (.not. allocated(call_node%arg_indices)) return
                if (size(call_node%arg_indices) < 1) return

                arg_idx = call_node%arg_indices(1)
                if (.not. arena%has_node_at(arg_idx)) return

                select type (arg_node => arena%entries(arg_idx)%node)
                    type is (identifier_node)
                    if (.not. allocated(arg_node%name)) return
                    arg_name = trim(arg_node%name)

                    param_idx = 0
                    do j = 1, size(param_names)
                        if (trim(param_names(j)) == arg_name) then
                            param_idx = j
                            exit
                        end if
                    end do

                    if (param_idx == 0) return

                    if (param_types(param_idx)%kind /= TARRAY) then
                        rank = 1
                        if (lowered_name == "size" .and. &
                            size(call_node%arg_indices) >= 2) then
                            rank = get_dimension_from_size_call(arena, call_node)
                        end if

                        base_type = infer_base_type_from_call_site(arena, &
                            func_node, &
                            param_idx)
                        if (base_type%kind <= 0 .or. base_type%kind == TVAR) then
                            base_type = param_types(param_idx)
                            if (base_type%kind <= 0 .or. base_type%kind == TVAR) then
                                base_type = create_mono_type(TREAL)
                            end if
                        end if

                        inferred_array_type = build_deferred_shape_array(base_type, &
                            rank)
                        call merge_parameter_type(param_types(param_idx), &
                            inferred_array_type)
                    end if
                end select
            end select
        end subroutine refine_from_intrinsic_call

        subroutine refine_from_array_indexing(call_node)
            type(call_or_subscript_node), intent(in) :: call_node
            character(len=:), allocatable :: param_name
            integer :: param_idx, rank
            type(mono_type_t) :: inferred_array_type, base_type

            if (.not. allocated(call_node%name)) return
            if (is_intrinsic_function(call_node%name)) return
            if (.not. allocated(call_node%arg_indices)) return
            if (size(call_node%arg_indices) == 0) return

            param_name = trim(call_node%name)
            param_idx = 0
            do j = 1, size(param_names)
                if (trim(param_names(j)) == param_name) then
                    param_idx = j
                    exit
                end if
            end do

            if (param_idx == 0) return

            if (param_types(param_idx)%kind /= TARRAY) then
                rank = size(call_node%arg_indices)

                base_type = infer_base_type_from_call_site(arena, func_node, &
                    param_idx)
                if (base_type%kind <= 0 .or. base_type%kind == TVAR) then
                    base_type = param_types(param_idx)
                    if (base_type%kind <= 0 .or. base_type%kind == TVAR) then
                        base_type = create_mono_type(TREAL)
                    end if
                end if

                inferred_array_type = build_deferred_shape_array(base_type, rank)
                call merge_parameter_type(param_types(param_idx), &
                    inferred_array_type)
            end if
        end subroutine refine_from_array_indexing

        function get_dimension_from_size_call(arena, call_node) result(dim)
            use ast_nodes_core, only: literal_node
            type(ast_arena_t), intent(in) :: arena
            type(call_or_subscript_node), intent(in) :: call_node
            integer :: dim
            integer :: dim_arg_idx
            integer :: iostat_val

            dim = 1

            if (.not. allocated(call_node%arg_indices)) return
            if (size(call_node%arg_indices) < 2) return

            dim_arg_idx = call_node%arg_indices(2)
            if (.not. arena%has_node_at(dim_arg_idx)) return

            select type (dim_node => arena%entries(dim_arg_idx)%node)
                type is (literal_node)
                if (allocated(dim_node%value)) then
                    read (dim_node%value, *, iostat=iostat_val) dim
                    if (iostat_val /= 0) dim = 1
                end if
            end select
        end function get_dimension_from_size_call

    end subroutine refine_parameters_from_body_usage

end module semantic_parameter_analysis
