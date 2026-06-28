module semantic_function_inference
    use type_system_unified, only: mono_type_t, create_mono_type, &
        create_type_var, TVAR, TREAL, TCHAR, TARRAY
    use type_array_safe, only: safe_peel_array_to_base
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, &
        call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_control, only: if_node, do_loop_node, do_while_node, &
        select_case_node, case_block_node, &
        case_default_node, select_type_node, &
        type_guard_block_node, where_node, &
        where_stmt_node, associate_node, &
        block_construct_node, forall_node, &
        elseif_wrapper_t, elsewhere_clause_t
    use ast_nodes_data, only: declaration_node
    use semantic_procedure_utils, only: detect_result_name, &
        declaration_type_to_mono
    use semantic_type_context, only: infer_type_from_usage_context, &
        infer_expression_type_static
    use semantic_array_type_builders, only: build_deferred_shape_array
    use semantic_parameter_analysis, only: merge_parameter_type
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
            if (.not. arena%has_node_at(stmt_index)) cycle
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
            arena, func_node%body_indices, func_node, result_name, &
            param_names, param_types)
    end function infer_result_type_from_assignments

    function build_result_aliases(func_node, result_name) result(aliases)
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable :: aliases(:)

        call append_alias(result_name, aliases)
        if (allocated(func_node%name)) then
            call append_alias(func_node%name, aliases)
        end if
        if (allocated(func_node%result_variable)) then
            call append_alias(func_node%result_variable, aliases)
        end if
        if (.not. allocated(aliases)) allocate (aliases(0))
    end function build_result_aliases

    pure function normalize_alias(name) result(norm_name)
        character(len=*), intent(in) :: name
        character(len=64) :: norm_name
        integer :: nlen

        norm_name = ''
        nlen = len_trim(name)
        if (nlen <= 0) return
        if (nlen > len(norm_name)) nlen = len(norm_name)
        norm_name(1:nlen) = name(1:nlen)
    end function normalize_alias

    subroutine append_alias(name, aliases)
        character(len=*), intent(in) :: name
        character(len=64), allocatable, intent(inout) :: aliases(:)
        character(len=64) :: normalized
        character(len=64), allocatable :: temp(:)
        integer :: i, count

        normalized = normalize_alias(name)
        if (len_trim(normalized) == 0) return

        if (.not. allocated(aliases)) then
            allocate (aliases(1))
            aliases(1) = normalized
            return
        end if

        do i = 1, size(aliases)
            if (trim(aliases(i)) == trim(normalized)) return
        end do

        count = size(aliases)
        allocate (temp(count + 1))
        temp(1:count) = aliases
        temp(count + 1) = normalized
        call move_alloc(temp, aliases)
    end subroutine append_alias

    logical function matches_alias(name, aliases) result(found)
        character(len=*), intent(in) :: name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64) :: normalized
        integer :: i

        found = .false.
        if (.not. allocated(aliases)) return
        normalized = normalize_alias(name)
        if (len_trim(normalized) == 0) return

        do i = 1, size(aliases)
            if (trim(aliases(i)) == trim(normalized)) then
                found = .true.
                return
            end if
        end do
    end function matches_alias

    function select_best_assignment_type(arena, body_indices, func_node, &
            result_name, param_names, param_types) &
            result(selected)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: selected
        type(mono_type_t) :: fallback
        character(len=64), allocatable :: aliases(:)
        integer :: i

        selected%kind = 0
        fallback%kind = 0
        aliases = build_result_aliases(func_node, result_name)
        do i = 1, size(body_indices)
            call accumulate_result_assignments(arena, body_indices(i), result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
        end do
        if (selected%kind == 0) selected = fallback

        ! Elemental functions MUST return scalars, never arrays
        ! ISO Fortran standard: elemental attribute applies element-wise
        ! The return type must be scalar even though params can be arrays
        if (is_elemental_function(func_node) .and. selected%kind == TARRAY) then
            selected = safe_peel_array_to_base(selected)
        end if
    end function select_best_assignment_type

    logical function is_elemental_function(func_node) result(is_elem)
        type(function_def_node), intent(in) :: func_node
        integer :: i

        is_elem = .false.
        if (.not. allocated(func_node%prefix_keywords)) return
        do i = 1, size(func_node%prefix_keywords)
            if (trim(func_node%prefix_keywords(i)) == 'elemental') then
                is_elem = .true.
                return
            end if
        end do
    end function is_elemental_function

    recursive subroutine accumulate_result_assignments(arena, stmt_index, &
            result_name, aliases, &
            param_names, param_types, &
            selected, fallback)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback
        type(mono_type_t) :: candidate

        if (.not. arena%has_node_at(stmt_index)) return

        select type (stmt => arena%entries(stmt_index)%node)
            type is (assignment_node)
            candidate = infer_assignment_result_type(arena, stmt, result_name, &
                aliases, param_names, param_types)
            if (candidate%kind == 0) then
                ! no-op
            else if (candidate%kind == TVAR) then
                if (fallback%kind == 0) fallback = candidate
            else
                call merge_parameter_type(selected, candidate)
            end if
            type is (if_node)
            call process_if_node(arena, stmt, result_name, aliases, param_names, &
                param_types, selected, fallback)
            type is (where_node)
            call process_where_node(arena, stmt, result_name, aliases, param_names, &
                param_types, selected, fallback)
            type is (select_case_node)
            call process_select_case_node(arena, stmt, result_name, aliases, &
                param_names, param_types, selected, fallback)
            type is (select_type_node)
            call process_select_type_node(arena, stmt, result_name, aliases, &
                param_names, param_types, selected, fallback)
            type is (do_loop_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (do_while_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (forall_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (where_stmt_node)
            call accumulate_result_assignments(arena, stmt%assignment_index, &
                result_name, aliases, param_names, &
                param_types, selected, fallback)
            type is (case_block_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (case_default_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (type_guard_block_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (associate_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (block_construct_node)
            call process_simple_body_node(arena, stmt%body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
            type is (function_def_node)
            return
            type is (subroutine_def_node)
            return
        class default
            ! Unhandled node types either cannot contain statements
            ! or do not affect result type inference.
        end select
    end subroutine accumulate_result_assignments

    recursive subroutine process_simple_body_node(arena, body_indices, &
            result_name, aliases, &
            param_names, param_types, &
            selected, fallback)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback

        if (allocated(body_indices)) then
            call accumulate_body_list(arena, body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
        end if
    end subroutine process_simple_body_node

    recursive subroutine process_if_node(arena, stmt, result_name, aliases, &
            param_names, param_types, selected, fallback)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback
        integer :: i

        if (allocated(stmt%then_body_indices)) then
            call accumulate_body_list(arena, stmt%then_body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
        end if
        if (allocated(stmt%elseif_blocks)) then
            do i = 1, size(stmt%elseif_blocks)
                if (allocated(stmt%elseif_blocks(i)%body_indices)) then
                    call accumulate_body_list( &
                        arena, stmt%elseif_blocks(i)%body_indices, result_name, &
                        aliases, param_names, param_types, selected, fallback)
                end if
            end do
        end if
        if (allocated(stmt%else_body_indices)) then
            call accumulate_body_list(arena, stmt%else_body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
        end if
    end subroutine process_if_node

    recursive subroutine process_where_node(arena, stmt, result_name, aliases, &
            param_names, param_types, selected, &
            fallback)
        type(ast_arena_t), intent(in) :: arena
        type(where_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback
        integer :: i

        if (allocated(stmt%where_body_indices)) then
            call accumulate_body_list(arena, stmt%where_body_indices, result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
        end if
        if (allocated(stmt%elsewhere_clauses)) then
            do i = 1, size(stmt%elsewhere_clauses)
                if (allocated(stmt%elsewhere_clauses(i)%body_indices)) then
                    call accumulate_body_list( &
                        arena, stmt%elsewhere_clauses(i)%body_indices, &
                        result_name, aliases, param_names, param_types, &
                        selected, fallback)
                end if
            end do
        end if
    end subroutine process_where_node

    recursive subroutine process_select_case_node(arena, stmt, result_name, &
            aliases, param_names, &
            param_types, selected, fallback)
        type(ast_arena_t), intent(in) :: arena
        type(select_case_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback
        integer :: i

        if (allocated(stmt%case_indices)) then
            do i = 1, size(stmt%case_indices)
                call accumulate_result_assignments(arena, stmt%case_indices(i), &
                    result_name, aliases, &
                    param_names, param_types, &
                    selected, fallback)
            end do
        end if
        call accumulate_result_assignments(arena, stmt%default_index, result_name, &
            aliases, param_names, param_types, &
            selected, fallback)
    end subroutine process_select_case_node

    recursive subroutine process_select_type_node(arena, stmt, result_name, &
            aliases, param_names, &
            param_types, selected, fallback)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback
        integer :: i

        if (allocated(stmt%guard_indices)) then
            do i = 1, size(stmt%guard_indices)
                call accumulate_result_assignments(arena, stmt%guard_indices(i), &
                    result_name, aliases, &
                    param_names, param_types, &
                    selected, fallback)
            end do
        end if
        call accumulate_result_assignments(arena, stmt%default_index, result_name, &
            aliases, param_names, param_types, &
            selected, fallback)
    end subroutine process_select_type_node

    recursive subroutine accumulate_body_list(arena, indices, result_name, &
            aliases, param_names, param_types, &
            selected, fallback)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t), intent(inout) :: selected
        type(mono_type_t), intent(inout) :: fallback
        integer :: i

        if (size(indices) <= 0) return
        do i = 1, size(indices)
            call accumulate_result_assignments(arena, indices(i), result_name, &
                aliases, param_names, param_types, &
                selected, fallback)
        end do
    end subroutine accumulate_body_list

    function infer_assignment_result_type(arena, stmt, result_name, aliases, &
            param_names, param_types) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate
        integer :: target_index

        candidate%kind = 0
        target_index = stmt%target_index
        if (.not. arena%has_node_at(target_index)) return

        select type (target => arena%entries(target_index)%node)
            type is (identifier_node)
            if (.not. matches_alias(target%name, aliases)) return
            candidate = infer_expression_type_static(arena, stmt%value_index, &
                param_names, param_types)
            ! Issue #2066: Distinguish array operations from scalar accumulation.
            ! If RHS is array but uses subscripted accesses (e.g., arr(i)), peel to
            ! scalar. If RHS is whole-array operation (e.g., x * x), keep as array.
            if (candidate%kind == TARRAY .and. &
                .not. is_array_literal_node(arena, stmt%value_index)) then
                if (expression_uses_subscripted_params(arena, stmt%value_index, &
                    param_names)) then
                    candidate = safe_peel_array_to_base(candidate)
                end if
            end if
            if (needs_deferred_shape(candidate)) then
                candidate = convert_to_deferred_shape_array(candidate)
            end if
            type is (call_or_subscript_node)
            candidate = infer_array_assignment_type(arena, target, stmt%value_index, &
                result_name, aliases, &
                param_names, param_types)
        end select
    end function infer_assignment_result_type

    logical function is_array_literal_node(arena, node_index)
        use ast_nodes_core, only: array_literal_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_array_literal_node = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (value_node => arena%entries(node_index)%node)
            type is (array_literal_node)
            is_array_literal_node = .true.
        end select
    end function is_array_literal_node

    recursive function expression_uses_subscripted_params(arena, expr_index, &
            param_names) &
            result(uses_subscripts)
        use ast_nodes_core, only: call_or_subscript_node, binary_op_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64), allocatable, intent(in) :: param_names(:)
        logical :: uses_subscripts
        integer :: i

        uses_subscripts = .false.
        if (.not. arena%has_node_at(expr_index)) return

        select type (node => arena%entries(expr_index)%node)
            type is (call_or_subscript_node)
            ! Check if this is a subscripted parameter (e.g., arr(i))
            if (allocated(node%name) .and. allocated(node%arg_indices)) then
                do i = 1, size(param_names)
                    if (trim(param_names(i)) == trim(node%name)) then
                        ! Found parameter with subscripts - this is scalar access
                        uses_subscripts = .true.
                        return
                    end if
                end do
            end if
            type is (binary_op_node)
            ! Recursively check left and right sides
            uses_subscripts = expression_uses_subscripted_params(arena, &
                node%left_index, &
                param_names)
            if (uses_subscripts) return
            uses_subscripts = expression_uses_subscripted_params(arena, &
                node%right_index, &
                param_names)
        end select
    end function expression_uses_subscripted_params

    logical function needs_deferred_shape(typ) result(needs)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(in) :: typ

        needs = .false.
        if (typ%kind /= TARRAY) return
        if (typ%alloc_info%is_allocatable) return
        if (typ%size > 0 .and. .not. typ%alloc_info%needs_allocation_check) return
        needs = .true.
    end function needs_deferred_shape

    function infer_array_assignment_type(arena, target, value_index, result_name, &
            aliases, param_names, param_types) &
            result(candidate)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: target
        integer, intent(in) :: value_index
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: aliases(:)
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: candidate
        type(mono_type_t) :: element_type
        integer :: rank

        candidate%kind = 0
        if (.not. allocated(target%name)) return
        if (.not. matches_alias(target%name, aliases)) return
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

    recursive function convert_to_deferred_shape_array(typ) result(deferred)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: deferred
        type(mono_type_t) :: inner
        type(mono_type_t), allocatable :: args(:)

        deferred = typ
        if (typ%kind /= TARRAY) return

        deferred%size = 0
        deferred%alloc_info%is_allocatable = .true.
        deferred%alloc_info%needs_allocation_check = .true.

        if (typ%get_args_count() > 0) then
            inner = typ%get_arg(1)
            inner = convert_to_deferred_shape_array(inner)
            allocate (args(1))
            args(1) = inner
            deferred = create_mono_type(TARRAY, args=args)
            deferred%size = 0
            deferred%alloc_info%is_allocatable = .true.
            deferred%alloc_info%needs_allocation_check = .true.
        end if
    end function convert_to_deferred_shape_array

end module semantic_function_inference
