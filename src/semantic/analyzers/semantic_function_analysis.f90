module semantic_function_analysis
    ! Function definition analysis extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, create_fun_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN
    use ast_core
    use ast_nodes_core, only: identifier_node
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
        
        ! Analyze function parameters
        if (allocated(func_node%param_indices)) then
            allocate(param_types(size(func_node%param_indices)))
            
            ! Create new scope for function parameters
            call scopes%enter_block()
            
            do i = 1, size(func_node%param_indices)
                ! Simple parameter type inference for now
                temp_type = create_mono_type(TREAL)  ! Default to real
                param_types(i) = temp_type
                
                ! Add parameter to function scope if it's an identifier
                if (func_node%param_indices(i) > 0 .and. func_node%param_indices(i) <= arena%size) then
                    select type (param_node => arena%entries(func_node%param_indices(i))%node)
                    type is (identifier_node)
                        if (allocated(param_node%name) .and. len_trim(param_node%name) > 0) then
                            block
                                type(poly_type_t) :: param_scheme
                                param_scheme = create_poly_type(forall_vars=[type_var_t::], mono=param_types(i))
                                call scopes%define(param_node%name, param_scheme)
                            end block
                        end if
                    end select
                end if
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
    subroutine create_function_scope(func_node, return_type, scopes)
        type(function_def_node), intent(in) :: func_node
        type(mono_type_t), intent(in) :: return_type
        type(scope_stack_t), intent(inout) :: scopes
        type(poly_type_t) :: result_scheme
        
        ! Add result variable to function scope
        result_scheme = create_poly_type(forall_vars=[type_var_t::], mono=return_type)
        
        if (allocated(func_node%result_variable) .and. len_trim(func_node%result_variable) > 0) then
            call scopes%define(func_node%result_variable, result_scheme)
        else if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            ! Function name is the result variable (standard Fortran)
            call scopes%define(func_node%name, result_scheme)
        end if
    end subroutine create_function_scope

end module semantic_function_analysis