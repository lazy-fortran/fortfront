module semantic_scope_creation
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_poly_type, TCHAR, TARRAY
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, identifier_node
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

        if (.not. arena%has_node_at(func_index)) return

        type_string = build_function_return_string(return_type, type_success)
        call apply_result_metadata(arena, func_index, result_name, return_type, &
                                   type_string, type_success)
    end subroutine update_function_metadata

    function build_function_return_string(return_type, success) result(text)
        type(mono_type_t), intent(in) :: return_type
        logical, intent(out) :: success
        character(len=:), allocatable :: text

        type(mono_type_t) :: resolved_type

        resolved_type = return_type
        call resolved_type%sync_from_arena()

        text = mono_type_to_string(resolved_type, include_shape=.false., &
                                   success=success)
        if (.not. success) text = ''
        if (success) then
            if (resolved_type%kind == TCHAR .and. resolved_type%size <= 0) then
                if (.not. resolved_type%alloc_info%needs_allocatable_string) then
                    text = 'character(len=:), allocatable'
                end if
            else if (resolved_type%kind == TARRAY) then
                text = mono_type_to_string(resolved_type, include_shape=.true., &
                                           success=success)
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
        character(len=:), allocatable :: resolved_result_name
        type(mono_type_t) :: return_copy

        if (.not. arena%has_node_at(func_index)) return

        return_copy = return_type
        call return_copy%sync_from_arena()

        if (len_trim(result_name) > 0) then
            resolved_result_name = trim(result_name)
        else
            resolved_result_name = ''
        end if

        select type (node => arena%entries(func_index)%node)
        type is (function_def_node)
            if (len_trim(resolved_result_name) > 0) then
                call update_identifier_type_in_arena(arena, resolved_result_name, &
                                                     return_copy)
            end if
            if (allocated(node%name)) then
                function_name = trim(node%name)
            else
                function_name = ''
            end if
            if (len_trim(function_name) > 0) then
                if (len_trim(resolved_result_name) > 0) then
                    if (trim(function_name) /= trim(resolved_result_name)) then
                        if (has_assignment_to_function(arena, node, &
                                                       function_name)) then
                            resolved_result_name = function_name
                        else if (allocated(node%body_indices)) then
                            call rename_identifier_in_arena( &
                                arena, trim(function_name), &
                                trim(resolved_result_name), node%body_indices, &
                                func_index)
                        end if
                    end if
                end if
            end if
            if (len_trim(function_name) > 0) then
                if (trim(resolved_result_name) /= trim(function_name)) then
                    call update_identifier_type_in_arena(arena, function_name, &
                                                         return_copy)
                end if
            end if
            call refine_return_type_from_body(arena, func_index, &
                                              resolved_result_name, &
                                              function_name, return_copy)
            if (type_success .and. len_trim(type_string) > 0) then
                node%return_type = type_string
            end if
            if (requires_explicit_result_name(return_type, resolved_result_name, &
                                              function_name)) then
                resolved_result_name = trim(function_name) // "_result"
                call rename_identifier_in_arena(arena, trim(function_name), &
                                                trim(resolved_result_name), &
                                                node%body_indices, func_index)
                call update_identifier_type_in_arena(arena, resolved_result_name, &
                                                     return_copy)
            end if
            node%result_variable = resolved_result_name
            node%inferred_type = return_copy
            arena%entries(func_index)%node = node
        end select
    end subroutine apply_result_metadata

    subroutine refine_return_type_from_body(arena, func_index, result_name, &
                                            function_name, return_type)
        use ast_nodes_core, only: assignment_node, identifier_node, &
                                  call_or_subscript_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: func_index
        character(len=*), intent(in) :: result_name
        character(len=*), intent(in) :: function_name
        type(mono_type_t), intent(inout) :: return_type
        character(len=:), allocatable :: target_name
        integer :: i
        integer :: stmt_index
        integer :: target_index

        if (.not. arena%has_node_at(func_index)) return

        target_name = trim(result_name)
        if (len_trim(target_name) == 0) target_name = trim(function_name)
        if (len_trim(target_name) == 0) return

        select type (func => arena%entries(func_index)%node)
        type is (function_def_node)
            if (.not. allocated(func%body_indices)) return
            do i = 1, size(func%body_indices)
                stmt_index = func%body_indices(i)
                if (.not. arena%has_node_at(stmt_index)) cycle
                select type (stmt => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    target_index = stmt%target_index
                    if (.not. arena%has_node_at(target_index)) cycle
                    select type (target => arena%entries(target_index)%node)
                    type is (identifier_node)
                        if (trim(target%name) /= trim(target_name)) cycle
                    type is (call_or_subscript_node)
                        if (.not. allocated(target%name)) cycle
                        if (trim(target%name) /= trim(target_name)) cycle
                    class default
                        cycle
                    end select
                    if (stmt%inferred_type%kind > 0) then
                        if (return_type%kind == 0) then
                            return_type = stmt%inferred_type
                            call return_type%sync_from_arena()
                        else if (stmt%inferred_type%kind == TARRAY .and. &
                                 return_type%kind /= TARRAY) then
                            return_type = stmt%inferred_type
                            call return_type%sync_from_arena()
                        end if
                    end if
                end select
            end do
        end select
    end subroutine refine_return_type_from_body

    logical function requires_explicit_result_name(return_type, result_name, &
                                                   function_name) result(required)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(in) :: return_type
        character(len=*), intent(in) :: result_name
        character(len=*), intent(in) :: function_name

        required = .false.
        if (return_type%kind /= TARRAY) return
        if (len_trim(result_name) == 0) then
            required = .true.
            return
        end if
        if (len_trim(function_name) == 0) return
        required = (trim(result_name) == trim(function_name))
    end function requires_explicit_result_name

    logical function has_assignment_to_function(arena, func_node, function_name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=*), intent(in) :: function_name
        integer :: i
        integer :: stmt_index
        integer :: target_index

        has_assignment_to_function = .false.
        if (len_trim(function_name) == 0) return
        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            stmt_index = func_node%body_indices(i)
            if (.not. arena%has_node_at(stmt_index)) cycle
            select type (stmt => arena%entries(stmt_index)%node)
            type is (assignment_node)
                target_index = stmt%target_index
                if (.not. arena%has_node_at(target_index)) cycle
                select type (target => arena%entries(target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%name)) cycle
                    if (trim(target%name) == trim(function_name)) then
                        has_assignment_to_function = .true.
                        return
                    end if
                end select
            end select
        end do
    end function has_assignment_to_function

end module semantic_scope_creation
