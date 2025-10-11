module semantic_function_analysis
    ! Function definition analysis extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, create_fun_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN
    use ast_base, only: LITERAL_INTEGER
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, call_or_subscript_node, literal_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use scope_manager, only: scope_stack_t
    use semantic_validation_utils, only: update_identifier_type_in_arena, int_to_str
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
        select case(var_name)
        case ('i', 'j', 'k', 'n', 'count', 'index', 'num', 'size')
            ! Common integer variable patterns
            typ = create_mono_type(TINT)
        case ('x', 'y', 'z', 'result', 'value', 'temp')
            ! Common real variable patterns  
            typ = create_mono_type(TREAL)
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
    subroutine analyze_function_parameters(arena, func_node, param_types, scopes, next_var_id)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), allocatable, intent(out) :: param_types(:)
        type(scope_stack_t), intent(inout) :: scopes
        integer, intent(inout) :: next_var_id
        integer :: i
        type(mono_type_t) :: temp_type
        type(poly_type_t) :: scheme
        character(len=64) :: param_name
        character(len=64) :: trimmed_name
        character(len=64), allocatable :: stored_names(:)
        integer :: idx

        if (allocated(func_node%param_indices)) then
            allocate(param_types(size(func_node%param_indices)))
            allocate(stored_names(size(func_node%param_indices)))
            do i = 1, size(func_node%param_indices)
                param_name = ''
                temp_type%kind = 0
                if (func_node%param_indices(i) > 0 .and. &
                    func_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(func_node%param_indices(i))%node)) then
                        select type (param_node => arena%entries(func_node%param_indices(i))%node)
                        type is (identifier_node)
                            param_name = param_node%name
                        type is (parameter_declaration_node)
                            param_name = param_node%name
                            temp_type = declaration_type_to_mono(param_node%type_name)
                        type is (declaration_node)
                            param_name = param_node%var_name
                            temp_type = declaration_type_to_mono(param_node%type_name)
                        class default
                            param_name = ''
                        end select
                    end if
                end if

                trimmed_name = trim(param_name)
                if (len_trim(trimmed_name) == 0) then
                    trimmed_name = 'arg'//trim(int_to_str(i))
                end if

                if (temp_type%kind == 0) then
                    temp_type = infer_type_from_usage_context(trimmed_name, next_var_id)
                end if

                if (temp_type%kind == TVAR) then
                    if (len_trim(trimmed_name) > 0) then
                        select case (trimmed_name(1:1))
                        case ('i','j','k','l','m','n')
                            temp_type = create_mono_type(TINT)
                        case default
                            temp_type = create_mono_type(TREAL)
                        end select
                    else
                        temp_type = create_mono_type(TREAL)
                    end if
                end if

                param_types(i) = temp_type
                stored_names(i) = trimmed_name
            end do

            ! Adjust parameter types based on call sites
            do idx = 1, arena%size
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (call_node => arena%entries(idx)%node)
                type is (call_or_subscript_node)
                    if (.not. allocated(func_node%name)) cycle
                    if (trim(call_node%name) /= trim(func_node%name)) cycle
                    if (.not. allocated(call_node%arg_indices)) cycle
                    do i = 1, min(size(call_node%arg_indices), size(param_types))
                        if (call_node%arg_indices(i) <= 0 .or. call_node%arg_indices(i) > arena%size) cycle
                        if (.not. allocated(arena%entries(call_node%arg_indices(i))%node)) cycle
                        select type (arg_node => arena%entries(call_node%arg_indices(i))%node)
                        type is (literal_node)
                            if (arg_node%literal_kind == LITERAL_INTEGER) then
                                param_types(i) = create_mono_type(TINT)
                            end if
                        type is (identifier_node)
                            if (arg_node%inferred_type%kind == TINT) then
                                param_types(i) = create_mono_type(TINT)
                            end if
                        end select
                    end do
                end select
            end do

            do i = 1, size(param_types)
                if (len_trim(stored_names(i)) == 0) cycle
                scheme = create_poly_type(forall_vars=[type_var_t::], mono=param_types(i))
                call scopes%define(trim(stored_names(i)), scheme)
                call update_identifier_type_in_arena(arena, trim(stored_names(i)), param_types(i))
            end do
        else
            allocate(param_types(0))
        end if
    end subroutine analyze_function_parameters

    ! Determine function return type based on name and result variable
    function determine_function_return_type(arena, func_node, next_var_id) result(return_type)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: return_type
        character(len=:), allocatable :: result_var_name
        integer :: i, stmt_index

        ! Determine the name of the result variable
        if (allocated(func_node%result_variable) .and. len_trim(func_node%result_variable) > 0) then
            result_var_name = trim(func_node%result_variable)
        else if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            result_var_name = trim(func_node%name)
        else
            result_var_name = ''
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

        ! Fall back to heuristic inference if no explicit declaration found
        if (allocated(func_node%result_variable) .and. len_trim(func_node%result_variable) > 0) then
            return_type = infer_type_from_usage_context(func_node%result_variable, next_var_id)
        else if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            return_type = infer_type_from_usage_context(func_node%name, next_var_id)
        else
            return_type = create_mono_type(TREAL)
        end if

        if (return_type%kind == TVAR) then
            if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
                select case (func_node%name(1:1))
                case ('i','j','k','l','m','n')
                    return_type = create_mono_type(TINT)
                case default
                    return_type = create_mono_type(TREAL)
                end select
            else
                return_type = create_mono_type(TREAL)
            end if
        end if
    end function determine_function_return_type

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

        if (allocated(func_node%result_variable) .and. len_trim(func_node%result_variable) > 0) then
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

        result_scheme = create_poly_type(forall_vars=[type_var_t::], mono=return_type)
        call scopes%define(result_name, result_scheme)
        if (result_name /= func_name) then
            call scopes%define(func_name, result_scheme)
        end if

        call update_identifier_type_in_arena(arena, result_name, return_type)
        if (result_name /= func_name) call update_identifier_type_in_arena(arena, func_name, return_type)

        type_string = mono_type_to_string(return_type)
        if (len_trim(type_string) > 0) then
            if (func_index > 0 .and. func_index <= arena%size) then
                if (allocated(arena%entries(func_index)%node)) then
                    select type (node => arena%entries(func_index)%node)
                    type is (function_def_node)
                        node%return_type = type_string
                        if (.not. allocated(node%result_variable) .or. len_trim(node%result_variable) == 0) then
                            node%result_variable = result_name
                        end if
                        arena%entries(func_index)%node = node
                    end select
                end if
            end if
        end if
    end subroutine create_function_scope

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
            trimmed = trim(trimmed(1:paren_pos-1))
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
                write(size_buf, '(I0)') typ%size
                name = 'character(len='//trim(size_buf)//')'
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
