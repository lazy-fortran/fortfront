module semantic_scope_creation
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_poly_type, TCHAR
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_procedure, only: function_def_node
    use scope_manager, only: scope_stack_t
    use semantic_procedure_utils, only: detect_result_name
    use semantic_validation_utils, only: update_identifier_type_in_arena, &
                                         rename_identifier_in_arena
    use type_string_utils, only: mono_type_to_string
    implicit none
    private

    public :: create_function_scope

contains

    subroutine create_function_scope(arena, func_node, func_index, return_type, scopes)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: func_index
        type(mono_type_t), intent(in) :: return_type
        type(scope_stack_t), intent(inout) :: scopes
        character(len=:), allocatable :: func_name

        func_name = select_function_name(func_node)
        call scopes%enter_function(func_name)
        call register_function_scope_details(arena, func_node, func_index, func_name, &
                                             return_type, scopes)
    end subroutine create_function_scope

    subroutine register_function_scope_details(arena, func_node, func_index, &
                                               func_name, return_type, scopes)
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: func_name
        type(mono_type_t), intent(in) :: return_type
        type(scope_stack_t), intent(inout) :: scopes
        character(len=:), allocatable :: result_name

        result_name = resolve_scope_result_name(arena, func_node, func_name)
        call register_result_symbols(scopes, result_name, func_name, return_type)
        call update_function_metadata(arena, func_index, result_name, return_type)
    end subroutine register_function_scope_details

    function select_function_name(func_node) result(name)
        type(function_def_node), intent(in) :: func_node
        character(len=:), allocatable :: name

        if (allocated(func_node%name)) then
            name = trim(func_node%name)
            if (len_trim(name) > 0) return
        end if
        name = 'anonymous_function'
    end function select_function_name

    function resolve_scope_result_name(arena, func_node, func_name) result(name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: func_name
        character(len=:), allocatable :: name

        if (allocated(func_node%result_variable)) then
            if (len_trim(func_node%result_variable) > 0) then
                name = trim(func_node%result_variable)
                return
            end if
        end if

        name = detect_result_name(arena, func_node)
        if (len_trim(name) == 0) then
            if (len_trim(func_name) > 0) then
                name = func_name
            else
                name = 'result'
            end if
        end if
    end function resolve_scope_result_name

    subroutine register_result_symbols(scopes, result_name, func_name, return_type)
        type(scope_stack_t), intent(inout) :: scopes
        character(len=*), intent(in) :: result_name
        character(len=*), intent(in) :: func_name
        type(mono_type_t), intent(in) :: return_type
        type(poly_type_t) :: scheme

        scheme = create_poly_type(forall_vars=[type_var_t ::], mono=return_type)
        call scopes%define(trim(result_name), scheme)
        if (trim(result_name) /= trim(func_name)) then
            call scopes%define(trim(func_name), scheme)
        end if
    end subroutine register_result_symbols

    subroutine update_function_metadata(arena, func_index, result_name, return_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: result_name
        type(mono_type_t), intent(in) :: return_type
        character(len=:), allocatable :: type_string
        logical :: type_success

        if (func_index <= 0 .or. func_index > arena%size) return
        if (.not. allocated(arena%entries(func_index)%node)) return

        type_string = build_function_return_string(return_type, type_success)
        call apply_result_metadata(arena, func_index, result_name, return_type, &
                                   type_string, type_success)
    end subroutine update_function_metadata

    function build_function_return_string(return_type, success) result(text)
        type(mono_type_t), intent(in) :: return_type
        logical, intent(out) :: success
        character(len=:), allocatable :: text

        text = mono_type_to_string(return_type, include_shape=.false., &
                                   success=success)
        if (.not. success) text = ''
        if (success) then
            if (return_type%kind == TCHAR .and. return_type%size <= 0) then
                if (.not. return_type%alloc_info%needs_allocatable_string) then
                    text = 'character(len=:), allocatable'
                end if
            end if
        end if
    end function build_function_return_string

    subroutine apply_result_metadata(arena, func_index, result_name, return_type, &
                                     type_string, type_success)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: result_name
        type(mono_type_t), intent(in) :: return_type
        character(len=*), intent(in) :: type_string
        logical, intent(in) :: type_success
        character(len=:), allocatable :: function_name

        select type (node => arena%entries(func_index)%node)
        type is (function_def_node)
            call update_identifier_type_in_arena(arena, result_name, return_type)
            if (allocated(node%name)) then
                function_name = trim(node%name)
            else
                function_name = ''
            end if
            if (len_trim(function_name) > 0) then
                if (len_trim(result_name) > 0) then
                    if (trim(function_name) /= trim(result_name)) then
                        if (allocated(node%body_indices)) then
                            call rename_identifier_in_arena(arena, &
                                                            trim(function_name), &
                                                            trim(result_name), &
                                                            node%body_indices, &
                                                            func_index)
                        end if
                    end if
                end if
            end if
            if (len_trim(function_name) > 0 .and. result_name /= function_name) then
                call update_identifier_type_in_arena(arena, function_name, &
                                                     return_type)
            end if
            if (type_success .and. len_trim(type_string) > 0) then
                node%return_type = type_string
            end if
            if (.not. allocated(node%result_variable) .or. &
                len_trim(node%result_variable) == 0) then
                node%result_variable = result_name
            end if
            arena%entries(func_index)%node = node
        end select
    end subroutine apply_result_metadata

end module semantic_scope_creation
