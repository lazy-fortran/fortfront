module semantic_function_analysis
    ! Function definition analysis extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, create_fun_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node
    use scope_manager, only: scope_stack_t
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
        
        ! Analyze function parameters (lightweight: avoid scope mutations)
        if (allocated(func_node%param_indices)) then
            allocate(param_types(size(func_node%param_indices)))
            do i = 1, size(func_node%param_indices)
                temp_type = create_mono_type(TREAL)  ! Default to real
                param_types(i) = temp_type
            end do
        else
            allocate(param_types(0))
        end if
    end subroutine analyze_function_parameters

    ! Determine function return type based on name and result variable
    function determine_function_return_type(func_node, next_var_id) result(return_type)
        type(function_def_node), intent(in) :: func_node
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: return_type
        
        ! Determine return type based on function name and return variable
        if (allocated(func_node%result_variable) .and. len_trim(func_node%result_variable) > 0) then
            ! Function has explicit result variable
            return_type = infer_type_from_usage_context(func_node%result_variable, next_var_id)
        else if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            ! Function name is the result variable (standard Fortran)
            return_type = infer_type_from_usage_context(func_node%name, next_var_id)
        else
            ! Default return type for unnamed functions
            return_type = create_mono_type(TREAL)
        end if
    end function determine_function_return_type

    ! Create function scope with result variable
    subroutine create_function_scope(func_node, return_type, scopes, arena, param_types)
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), intent(in) :: return_type
        type(scope_stack_t), intent(inout) :: scopes
        type(ast_arena_t), intent(in) :: arena
        type(mono_type_t), intent(in), optional :: param_types(:)

        type(poly_type_t) :: result_scheme, param_scheme
        integer :: i, param_count, param_index
        character(len=:), allocatable :: scope_name, result_name, param_name

        ! Determine the name of the scope we are about to enter
        if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            scope_name = trim(func_node%name)
        else
            scope_name = 'unnamed_function'
        end if

        call scopes%enter_function(scope_name)

        ! Ensure the function result is available within the new scope
        result_scheme = create_poly_type(forall_vars=[type_var_t::], mono=return_type)

        if (allocated(func_node%result_variable) .and. &
                len_trim(func_node%result_variable) > 0) then
            result_name = trim(func_node%result_variable)
            call scopes%define(result_name, result_scheme)
            if (result_name /= scope_name) then
                call scopes%define(scope_name, result_scheme)
            end if
        else
            result_name = scope_name
            call scopes%define(scope_name, result_scheme)
        end if

        ! Register all parameters within the function scope so subsequent
        ! statements can resolve their types during inference.
        if (allocated(func_node%param_indices)) then
            param_count = size(func_node%param_indices)
        else
            param_count = 0
        end if

        do i = 1, param_count
            param_index = func_node%param_indices(i)
            if (param_index <= 0) cycle
            if (param_index > arena%size) cycle
            if (.not. allocated(arena%entries(param_index)%node)) cycle

            param_name = ''
            select type (param_node => arena%entries(param_index)%node)
            type is (parameter_declaration_node)
                if (allocated(param_node%name)) param_name = trim(param_node%name)
            type is (declaration_node)
                if (allocated(param_node%var_name)) then
                    param_name = trim(param_node%var_name)
                else if (allocated(param_node%var_names)) then
                    if (size(param_node%var_names) >= 1) then
                        param_name = trim(param_node%var_names(1))
                    end if
                end if
            type is (identifier_node)
                if (allocated(param_node%name)) param_name = trim(param_node%name)
            class default
                ! Unknown node type - skip
            end select

            if (len_trim(param_name) == 0) cycle

            if (present(param_types)) then
                if (size(param_types) >= i) then
                    param_scheme = create_poly_type(forall_vars=[type_var_t::], &
                                                   mono=param_types(i))
                else
                    param_scheme = create_poly_type(forall_vars=[type_var_t::], &
                                                   mono=create_mono_type(TREAL))
                end if
            else
                param_scheme = create_poly_type(forall_vars=[type_var_t::], &
                                               mono=create_mono_type(TREAL))
            end if

            call scopes%define(param_name, param_scheme)
        end do
    end subroutine create_function_scope

end module semantic_function_analysis
