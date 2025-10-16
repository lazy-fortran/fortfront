module semantic_function_analysis
    ! Function definition analysis extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, create_fun_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, &
                              call_or_subscript_node, literal_node, binary_op_node, &
                              program_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use scope_manager, only: scope_stack_t
    use semantic_validation_utils, only: update_identifier_type_in_arena, int_to_str
    use semantic_type_operations, only: get_common_type, &
                                        instantiate_type_scheme_op
    implicit none
    private

    public :: infer_type_from_usage_context
    public :: analyze_function_parameters
    public :: determine_function_return_type
    public :: create_function_scope

contains

    ! Helper function to infer type from usage context (enhanced type inference)
    function infer_type_from_usage_context(var_name, next_var_id) result(typ)
        character(len=*), intent(in) :: var_name
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ

        ! Enhanced type inference based on variable name patterns and context
        ! This improves user experience by making reasonable type guesses

        ! Pattern-based type inference for common variable names
        select case (var_name)
        case ('i', 'j', 'k', 'n', 'count', 'index', 'num', 'size')
            ! Common integer variable patterns
            typ = create_mono_type(TINT)
        case ('x', 'y', 'z', 'result', 'value', 'temp')
            ! Ambiguous numeric names: defer to usage-driven inference
            typ = create_mono_type(TVAR, var=create_type_var(next_var_id, "v"))
            next_var_id = next_var_id + 1
        case ('flag', 'found', 'done', 'success', 'valid')
            ! Common logical variable patterns
            typ = create_mono_type(TLOGICAL)
        case default
            ! Check if name suggests a specific type
            if (index(var_name, 'str') > 0 .or. index(var_name, 'name') > 0 .or. &
                index(var_name, 'msg') > 0 .or. index(var_name, 'text') > 0) then
                ! String-like variable names
                typ = create_mono_type(TCHAR)
            else if (index(var_name, 'num') > 0 .or. index(var_name, 'count') > 0 .or. &
                     index(var_name, 'idx') > 0) then
                ! Number-like variable names
                typ = create_mono_type(TINT)
            else
                ! Default: create type variable for later unification
                typ = create_mono_type(TVAR, var=create_type_var(next_var_id, "v"))
                next_var_id = next_var_id + 1
            end if
        end select
    end function infer_type_from_usage_context

    ! Analyze function parameters and extract their types
    subroutine analyze_function_parameters(arena, func_node, param_types, param_names, &
                                           scopes, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(out) :: param_types(:)
        character(len=64), allocatable, intent(out) :: param_names(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id

        integer :: i, idx, arg_idx
        type(mono_type_t) :: temp_type
        type(mono_type_t) :: inferred_arg_type
        type(poly_type_t) :: scheme
        character(len=64), allocatable :: stored_names(:)
        character(len=64) :: param_name
        character(len=64) :: trimmed_name

        if (.not. allocated(func_node%param_indices)) then
            allocate (param_types(0))
            allocate (param_names(0))
            return
        end if

        allocate (param_types(size(func_node%param_indices)))
        allocate (stored_names(size(func_node%param_indices)))

        do i = 1, size(func_node%param_indices)
            param_name = ''
            temp_type%kind = 0
            if (func_node%param_indices(i) > 0 .and. &
                func_node%param_indices(i) <= arena%size) then
                if (allocated(arena%entries(func_node%param_indices(i))%node)) then
                    select type (param_node => &
                                 arena%entries(func_node%param_indices(i))%node)
                    type is (identifier_node)
                        param_name = param_node%name
                    type is (parameter_declaration_node)
                        param_name = param_node%name
                        temp_type = declaration_type_to_mono(param_node%type_name)
                        if (temp_type%kind == 0 .and. &
                            param_node%inferred_type%kind > 0) then
                            temp_type = param_node%inferred_type
                        end if
                    type is (declaration_node)
                        param_name = param_node%var_name
                        temp_type = declaration_type_to_mono(param_node%type_name)
                        if (temp_type%kind == 0 .and. &
                            param_node%inferred_type%kind > 0) then
                            temp_type = param_node%inferred_type
                        end if
                    class default
                        param_name = ''
                    end select
                end if
            end if

            trimmed_name = trim(param_name)
            if (len_trim(trimmed_name) == 0) then
                trimmed_name = 'arg' // trim(int_to_str(i))
            end if

            if (temp_type%kind == 0) then
                temp_type = infer_type_from_usage_context(trimmed_name, next_var_id)
            end if

            if (temp_type%kind == TVAR) then
                if (temp_type%var%id == 0) then
                    temp_type = create_mono_type(TVAR, &
                                                 var=create_type_var(next_var_id, "arg"))
                    next_var_id = next_var_id + 1
                end if
                if (len_trim(trimmed_name) > 0) then
                    select case (trimmed_name(1:1))
                    case ('i', 'j', 'k', 'l', 'm', 'n')
                        temp_type = create_mono_type(TINT)
                    end select
                end if
            end if

            param_types(i) = temp_type
            stored_names(i) = trimmed_name

        end do

        do idx = 1, arena%size
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (call_node => arena%entries(idx)%node)
            type is (call_or_subscript_node)
                if (.not. allocated(func_node%name)) cycle
                if (.not. allocated(call_node%name)) cycle
                if (trim(call_node%name) /= trim(func_node%name)) cycle
                if (.not. allocated(call_node%arg_indices)) cycle
                do i = 1, min(size(call_node%arg_indices), size(param_types))
                    arg_idx = call_node%arg_indices(i)
                    if (arg_idx <= 0 .or. arg_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(arg_idx)%node)) then
                        cycle
                    end if
                    select type (arg_node => arena%entries(arg_idx)%node)
                    type is (literal_node)
                        if (arg_node%literal_kind == LITERAL_INTEGER) then
                            param_types(i) = create_mono_type(TINT)
                        end if
                    type is (identifier_node)
                        if (arg_node%inferred_type%kind == TINT) then
                            param_types(i) = create_mono_type(TINT)
                        else
                            inferred_arg_type = &
                                infer_identifier_type_from_context( &
                                arena, arg_node%name, stored_names, param_types, &
                                scopes, arg_idx, next_var_id)
                            if (inferred_arg_type%kind == TINT) then
                                param_types(i) = inferred_arg_type
                            end if
                        end if
                    end select
                end do
            end select
        end do

        do i = 1, size(param_types)
            if (func_node%param_indices(i) <= 0 .or. func_node%param_indices(i) > &
                & arena%size) cycle
            if (.not. allocated(arena%entries(func_node%param_indices(i))%node)) cycle
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

        do i = 1, size(param_types)
            if (len_trim(stored_names(i)) == 0) cycle
            scheme = create_poly_type(forall_vars=[type_var_t ::], mono=param_types(i))
            call scopes%define(trim(stored_names(i)), scheme)
            call update_identifier_type_in_arena( &
                arena, trim(stored_names(i)), param_types(i))
        end do

        param_names = stored_names

    end subroutine analyze_function_parameters

    ! Determine function return type based on name and result variable
    function determine_function_return_type( &
        arena, func_node, param_names, param_types, next_var_id) result(return_type)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: return_type
        character(len=:), allocatable :: result_var_name
        integer :: i, stmt_index

        ! Determine the name of the result variable
        if (allocated(func_node%result_variable) .and. &
            len_trim(func_node%result_variable) > 0) then
            result_var_name = trim(func_node%result_variable)
        else
            result_var_name = detect_result_name(arena, func_node)
            if (len_trim(result_var_name) == 0) then
                if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
                    result_var_name = trim(func_node%name)
                else
                    result_var_name = ''
                end if
            end if
        end if

        ! First try to find explicit declaration in function body
        if (len_trim(result_var_name) > 0 .and. allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                stmt_index = func_node%body_indices(i)
                if (stmt_index <= 0 .or. stmt_index > arena%size) cycle
                if (.not. allocated(arena%entries(stmt_index)%node)) cycle
                select type (stmt => arena%entries(stmt_index)%node)
                type is (declaration_node)
                    if (trim(stmt%var_name) == result_var_name) then
                        return_type = declaration_type_to_mono(stmt%type_name)
                        if (return_type%kind /= 0) return
                    end if
                end select
            end do
        end if

        if (len_trim(result_var_name) > 0) then
            return_type = infer_result_type_from_assignments( &
                          arena, func_node, result_var_name, param_names, param_types)
            if (return_type%kind /= 0) return
        end if

        ! Fall back to heuristic inference if no explicit declaration found
        if (allocated(func_node%result_variable) .and. &
            len_trim(func_node%result_variable) > 0) then
            return_type = infer_type_from_usage_context( &
                          func_node%result_variable, next_var_id)
        else if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            return_type = infer_type_from_usage_context(func_node%name, next_var_id)
        else
            return_type = create_mono_type(TREAL)
        end if

        if (return_type%kind == TVAR) then
            if (return_type%var%id == 0) then
                return_type = create_mono_type(TVAR, &
                                               var=create_type_var(next_var_id, "ret"))
                next_var_id = next_var_id + 1
            end if
        end if
    end function determine_function_return_type

    function infer_result_type_from_assignments( &
        arena, func_node, result_name, param_names, param_types) result(inferred)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: result_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: inferred
        integer :: i, stmt_index
        type(mono_type_t) :: expr_type

        inferred%kind = 0
        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            stmt_index = func_node%body_indices(i)
            if (stmt_index <= 0 .or. stmt_index > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_index)%node)) cycle
            select type (stmt => arena%entries(stmt_index)%node)
            type is (assignment_node)
                if (stmt%target_index <= 0 .or. stmt%target_index > arena%size) cycle
                if (.not. allocated(arena%entries(stmt%target_index)%node)) cycle
                select type (target => arena%entries(stmt%target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%name)) cycle
                    if (trim(target%name) /= trim(result_name)) cycle
                    expr_type = infer_expression_type_static( &
                                arena, stmt%value_index, param_names, param_types)
                    if (expr_type%kind /= 0) then
                        inferred = expr_type
                        return
                    end if
                end select
            end select
        end do
    end function infer_result_type_from_assignments

    recursive function infer_expression_type_static( &
        arena, expr_index, param_names, param_types) result(typ)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(mono_type_t) :: typ
        integer :: i
        type(mono_type_t) :: left_typ, right_typ

        typ%kind = 0
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            select case (node%literal_kind)
            case (LITERAL_INTEGER)
                typ = create_mono_type(TINT)
            case (LITERAL_REAL)
                typ = create_mono_type(TREAL)
            case default
                typ%kind = 0
            end select
        type is (identifier_node)
            if (allocated(node%name)) then
                do i = 1, size(param_names)
                    if (trim(param_names(i)) == trim(node%name)) then
                        typ = param_types(i)
                        if (typ%kind /= 0) return
                    end if
                end do
            end if
            if (node%inferred_type%kind > 0) then
                typ = node%inferred_type
            end if
        type is (binary_op_node)
            left_typ = infer_expression_type_static( &
                       arena, node%left_index, param_names, param_types)
            right_typ = infer_expression_type_static( &
                        arena, node%right_index, param_names, param_types)
            if (left_typ%kind == 0 .and. right_typ%kind == 0) return
            if (left_typ%kind == 0) left_typ = right_typ
            if (right_typ%kind == 0) right_typ = left_typ
            typ = get_common_type(left_typ, right_typ)
        type is (call_or_subscript_node)
            if (node%inferred_type%kind > 0) typ = node%inferred_type
        end select
    end function infer_expression_type_static

    function infer_identifier_type_from_context(arena, ident_name, param_names, &
                                                param_types, scopes, anchor_index, &
                                                next_var_id) result(typ)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: ident_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        type(scope_stack_t), intent(in) :: scopes
        integer, intent(in) :: anchor_index
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        integer :: scope_index, program_index, search_start, forward_start
        character(len=64) :: lowered_name

        typ%kind = 0
        lowered_name = trim(ident_name)
        if (len_trim(lowered_name) == 0) return
        if (arena%size <= 0) return

        call scopes%lookup(lowered_name, scheme)
        if (allocated(scheme)) then
            typ = instantiate_type_scheme_op(scheme, next_var_id)
            if (typ%kind /= 0) then
                deallocate (scheme)
                return
            end if
            deallocate (scheme)
        end if

        scope_index = -1
        program_index = -1
        if (anchor_index > 0 .and. anchor_index <= arena%size) then
            scope_index = find_nearest_scope_owner(arena, anchor_index)
            program_index = find_program_owner(arena, anchor_index)
            search_start = anchor_index - 1
        else
            search_start = arena%size
        end if

        if (search_start >= 1) then
            call search_identifier_type_range(arena, lowered_name, param_names, &
                                              param_types, scope_index, program_index, &
                                              search_start, 1, -1, typ)
            if (typ%kind /= 0) return
        end if

        if (anchor_index > 0 .and. anchor_index < arena%size) then
            forward_start = anchor_index + 1
        else
            forward_start = 1
        end if

        if (forward_start <= arena%size) then
            call search_identifier_type_range(arena, lowered_name, param_names, &
                                              param_types, scope_index, program_index, &
                                              forward_start, arena%size, 1, typ)
        end if
    end function infer_identifier_type_from_context

    function infer_identifier_type_at_index(arena, entry_index, lowered_name, &
                                            param_names, param_types, scope_index, &
                                            program_index) result(candidate)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: entry_index
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: scope_index, program_index
        type(mono_type_t) :: candidate
        integer :: name_idx, target_idx

        candidate%kind = 0
        if (.not. allocated(arena%entries(entry_index)%node)) return

        select type (node => arena%entries(entry_index)%node)
        type is (declaration_node)
            if (allocated(node%var_name)) then
                if (trim(node%var_name) == lowered_name) then
                    candidate = declaration_type_to_mono(node%type_name)
                    if (candidate%kind /= 0) return
                end if
            end if
            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                do name_idx = 1, size(node%var_names)
                    if (trim(node%var_names(name_idx)) == lowered_name) then
                        candidate = declaration_type_to_mono(node%type_name)
                        if (candidate%kind /= 0) return
                    end if
                end do
            end if
        type is (assignment_node)
            target_idx = node%target_index
            if (target_idx <= 0 .or. target_idx > arena%size) return
            if (.not. allocated(arena%entries(target_idx)%node)) return
            if (.not. identifier_visible_in_scope(arena, target_idx, scope_index, &
                                                  program_index)) return
            select type (target => arena%entries(target_idx)%node)
            type is (identifier_node)
                if (trim(target%name) /= lowered_name) return
                candidate = infer_expression_type_static(arena, node%value_index, &
                                                         param_names, param_types)
            end select
        end select
    end function infer_identifier_type_at_index

    subroutine search_identifier_type_range(arena, lowered_name, param_names, &
                                            param_types, scope_index, program_index, &
                                            start_idx, end_idx, step, typ)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: lowered_name
        character(len=64), allocatable, intent(in) :: param_names(:)
        type(mono_type_t), allocatable, intent(in) :: param_types(:)
        integer, intent(in) :: scope_index, program_index
        integer, intent(in) :: start_idx, end_idx, step
        type(mono_type_t), intent(inout) :: typ
        type(mono_type_t) :: candidate
        integer :: idx

        if (typ%kind /= 0 .or. step == 0) return
        do idx = start_idx, end_idx, step
            if (.not. allocated(arena%entries(idx)%node)) cycle
            if (.not. identifier_visible_in_scope(arena, idx, scope_index, &
                                                  program_index)) cycle
            candidate = infer_identifier_type_at_index(arena, idx, lowered_name, &
                                                       param_names, param_types, &
                                                       scope_index, program_index)
            if (candidate%kind /= 0) then
                typ = candidate
                return
            end if
        end do
    end subroutine search_identifier_type_range

    ! Create function scope with result variable
    subroutine create_function_scope(arena, func_node, func_index, return_type, scopes)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: func_index
        type(mono_type_t), intent(in) :: return_type
        type(scope_stack_t), intent(inout) :: scopes
        type(poly_type_t) :: result_scheme
        character(len=:), allocatable :: func_name
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: type_string

        if (allocated(func_node%name)) then
            func_name = trim(func_node%name)
        else
            func_name = 'anonymous_function'
        end if

        call scopes%enter_function(func_name)

        if (allocated(func_node%result_variable) .and. &
            len_trim(func_node%result_variable) > 0) then
            result_name = trim(func_node%result_variable)
        else
            result_name = detect_result_name(arena, func_node)
            if (len_trim(result_name) == 0) then
                if (len_trim(func_name) > 0) then
                    result_name = func_name
                else
                    result_name = 'result'
                end if
            end if
        end if

        result_scheme = create_poly_type(forall_vars=[type_var_t ::], mono=return_type)
        call scopes%define(result_name, result_scheme)
        if (result_name /= func_name) then
            call scopes%define(func_name, result_scheme)
        end if

        call update_identifier_type_in_arena(arena, result_name, return_type)
        if (result_name /= func_name) then
            call update_identifier_type_in_arena(arena, func_name, return_type)
        end if

        type_string = mono_type_to_string(return_type)
        if (len_trim(type_string) > 0) then
            if (func_index > 0 .and. func_index <= arena%size) then
                if (allocated(arena%entries(func_index)%node)) then
                    select type (node => arena%entries(func_index)%node)
                    type is (function_def_node)
                        node%return_type = type_string
                        if (.not. allocated(node%result_variable) .or. &
                            len_trim(node%result_variable) == 0) then
                            node%result_variable = result_name
                        end if
                        arena%entries(func_index)%node = node
                    end select
                end if
            end if
        end if
    end subroutine create_function_scope

    integer function find_nearest_scope_owner(arena, node_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        scope_index = 0
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (.not. allocated(arena%entries(current)%node)) then
                current = arena%entries(current)%parent_index
                cycle
            end if
            select type (owner => arena%entries(current)%node)
            type is (function_def_node)
                scope_index = current
                return
            type is (subroutine_def_node)
                scope_index = current
                return
            end select
            current = arena%entries(current)%parent_index
        end do
    end function find_nearest_scope_owner

    integer function find_program_owner(arena, node_index) result(program_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current

        program_index = 0
        current = node_index
        do while (current > 0 .and. current <= arena%size)
            if (.not. allocated(arena%entries(current)%node)) then
                current = arena%entries(current)%parent_index
                cycle
            end if
            select type (owner => arena%entries(current)%node)
            type is (program_node)
                program_index = current
                return
            end select
            current = arena%entries(current)%parent_index
        end do
    end function find_program_owner

    logical function identifier_visible_in_scope(arena, candidate_index, &
                                                 scope_index, program_index) &
        result(is_visible)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: candidate_index
        integer, intent(in) :: scope_index
        integer, intent(in) :: program_index
        integer :: candidate_scope, candidate_program

        is_visible = .false.
        if (candidate_index <= 0 .or. candidate_index > arena%size) return
        if (.not. allocated(arena%entries(candidate_index)%node)) return

        if (scope_index < 0) then
            is_visible = .true.
            return
        end if

        candidate_scope = find_nearest_scope_owner(arena, candidate_index)

        if (scope_index > 0) then
            if (candidate_scope == scope_index) then
                is_visible = .true.
            end if
            return
        end if

        candidate_program = find_program_owner(arena, candidate_index)
        if (candidate_scope == 0) then
            if (program_index < 0) then
                is_visible = .true.
            else if (candidate_program == program_index) then
                is_visible = .true.
            end if
        end if
    end function identifier_visible_in_scope

    function declaration_type_to_mono(type_name) result(mono)
        character(len=*), intent(in) :: type_name
        type(mono_type_t) :: mono
        character(len=:), allocatable :: trimmed
        integer :: paren_pos

        mono%kind = 0
        trimmed = adjustl(type_name)
        if (.not. allocated(trimmed)) return
        if (len_trim(trimmed) == 0) return

        paren_pos = index(trimmed, '(')
        if (paren_pos > 0) then
            trimmed = trim(trimmed(1:paren_pos - 1))
        else
            trimmed = trim(trimmed)
        end if

        select case (trimmed)
        case ('integer')
            mono = create_mono_type(TINT)
        case ('real')
            mono = create_mono_type(TREAL)
        case ('logical')
            mono = create_mono_type(TLOGICAL)
        case ('character')
            mono = create_mono_type(TCHAR)
        case default
            mono%kind = 0
        end select
    end function declaration_type_to_mono

    function mono_type_to_string(typ) result(name)
        type(mono_type_t), intent(in) :: typ
        character(len=:), allocatable :: name
        character(len=32) :: size_buf

        select case (typ%kind)
        case (TINT)
            name = 'integer'
        case (TREAL)
            name = 'real'
        case (TLOGICAL)
            name = 'logical'
        case (TCHAR)
            if (typ%size > 0) then
                write (size_buf, '(I0)') typ%size
                name = 'character(len=' // trim(size_buf) // ')'
            else
                name = 'character(len=:), allocatable'
            end if
        case default
            name = ''
        end select
    end function mono_type_to_string

    function detect_result_name(arena, func_node) result(res_name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=:), allocatable :: res_name
        integer :: i, stmt_index, target_index

        res_name = ''
        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            stmt_index = func_node%body_indices(i)
            if (stmt_index <= 0 .or. stmt_index > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_index)%node)) cycle
            select type (stmt => arena%entries(stmt_index)%node)
            type is (assignment_node)
                target_index = stmt%target_index
                if (target_index > 0 .and. target_index <= arena%size) then
                    if (allocated(arena%entries(target_index)%node)) then
                        select type (target => arena%entries(target_index)%node)
                        type is (identifier_node)
                            res_name = trim(target%name)
                            if (len_trim(res_name) > 0) return
                        end select
                    end if
                end if
            end select
        end do
    end function detect_result_name

end module semantic_function_analysis
