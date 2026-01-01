module semantic_explicit_interface_checker
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, program_node
    use ast_nodes_data, only: module_node
    use ast_nodes_misc, only: contains_node, interface_block_node, &
                              module_procedure_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
                                   subroutine_def_node
    use error_handling, only: ERROR_SEMANTIC, create_error_result, &
                              error_collection_t
    use intrinsic_registry, only: is_intrinsic_function, is_intrinsic_subroutine
    use scope_manager, only: scope_stack_t
    use string_utils_mod, only: to_lower
    use type_system_unified, only: TARRAY, mono_type_t, poly_type_t
    implicit none
    private

    public :: validate_explicit_interface_for_function_reference
    public :: validate_explicit_interface_for_subroutine_call

contains

    subroutine validate_explicit_interface_for_function_reference(arena, scopes, &
                                                                  errors, expr, &
                                                                  expr_index)
        type(ast_arena_t), intent(in) :: arena
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        type(call_or_subscript_node), intent(in) :: expr
        integer, intent(in) :: expr_index

        character(len=:), allocatable :: proc_name

        if (.not. allocated(expr%name)) return
        if (len_trim(expr%name) == 0) return
        if (expr%base_expr_index /= 0) return

        proc_name = to_lower(trim(expr%name))
        if (is_intrinsic_function(proc_name)) return
        if (is_known_array_reference(scopes, proc_name)) return
        if (has_explicit_interface_in_arena(arena, proc_name)) return

        call emit_missing_explicit_interface(errors, expr%name)
    end subroutine validate_explicit_interface_for_function_reference

    subroutine validate_explicit_interface_for_subroutine_call(arena, scopes, &
                                                               errors, expr, &
                                                               expr_index)
        type(ast_arena_t), intent(in) :: arena
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: expr
        integer, intent(in) :: expr_index

        character(len=:), allocatable :: proc_name

        if (.not. allocated(expr%name)) return
        if (len_trim(expr%name) == 0) return

        proc_name = to_lower(trim(expr%name))
        if (is_intrinsic_subroutine(proc_name)) return
        if (has_explicit_interface_in_arena(arena, proc_name)) return

        call emit_missing_explicit_interface(errors, expr%name)
    end subroutine validate_explicit_interface_for_subroutine_call

    logical function is_known_array_reference(scopes, name) result(is_array)
        type(scope_stack_t), intent(inout) :: scopes
        character(len=*), intent(in) :: name

        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: mono

        is_array = .false.
        if (len_trim(name) == 0) return

        call scopes%lookup(name, scheme)
        if (.not. allocated(scheme)) return

        mono = scheme%get_mono()
        call mono%sync_from_arena()
        is_array = mono%kind == TARRAY
    end function is_known_array_reference

    logical function has_explicit_interface_in_arena(arena, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name

        integer :: i

        found = .false.
        if (len_trim(name) == 0) return

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle

            select type (node => arena%entries(i)%node)
            type is (interface_block_node)
                if (interface_block_has_named_procedure(arena, node, name)) then
                    found = .true.
                    return
                end if
            type is (module_node)
                if (allocated(node%procedure_indices)) then
                    if (indices_contain_named_procedure(arena, node%procedure_indices, &
                                                        name)) then
                        found = .true.
                        return
                    end if
                end if
            type is (program_node)
                if (body_has_internal_procedure_interface(arena, node%body_indices, &
                                                          name)) then
                    found = .true.
                    return
                end if
            type is (function_def_node)
                if (body_has_internal_procedure_interface(arena, node%body_indices, &
                                                          name)) then
                    found = .true.
                    return
                end if
            type is (subroutine_def_node)
                if (body_has_internal_procedure_interface(arena, node%body_indices, &
                                                          name)) then
                    found = .true.
                    return
                end if
            class default
                cycle
            end select
        end do
    end function has_explicit_interface_in_arena

    logical function body_has_internal_procedure_interface(arena, body_indices, &
                                                           name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name

        integer :: i
        logical :: in_contains

        found = .false.
        if (len_trim(name) == 0) return
        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        in_contains = .false.
        do i = 1, size(body_indices)
            if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(i))%node)) cycle

            select type (node => arena%entries(body_indices(i))%node)
            type is (contains_node)
                in_contains = .true.
            type is (function_def_node)
                if (.not. in_contains) cycle
                if (node_name_matches(node%name, name)) then
                    found = .true.
                    return
                end if
            type is (subroutine_def_node)
                if (.not. in_contains) cycle
                if (node_name_matches(node%name, name)) then
                    found = .true.
                    return
                end if
            class default
                cycle
            end select
        end do
    end function body_has_internal_procedure_interface

    logical function interface_block_has_named_procedure(arena, iface, name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: iface
        character(len=*), intent(in) :: name

        found = .false.
        if (.not. allocated(iface%procedure_indices)) return
        if (size(iface%procedure_indices) == 0) return

        found = indices_contain_named_procedure(arena, iface%procedure_indices, name)
    end function interface_block_has_named_procedure

    logical function indices_contain_named_procedure(arena, indices, name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        character(len=*), intent(in) :: name

        integer :: i

        found = .false.
        if (len_trim(name) == 0) return
        if (size(indices) == 0) return

        do i = 1, size(indices)
            if (indices(i) <= 0 .or. indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(indices(i))%node)) cycle

            select type (proc => arena%entries(indices(i))%node)
            type is (function_def_node)
                if (node_name_matches(proc%name, name)) then
                    found = .true.
                    return
                end if
            type is (subroutine_def_node)
                if (node_name_matches(proc%name, name)) then
                    found = .true.
                    return
                end if
            type is (module_procedure_node)
                if (module_procedure_has_name(proc, name)) then
                    found = .true.
                    return
                end if
            class default
                cycle
            end select
        end do
    end function indices_contain_named_procedure

    logical function module_procedure_has_name(node, name) result(found)
        type(module_procedure_node), intent(in) :: node
        character(len=*), intent(in) :: name

        integer :: i
        character(len=:), allocatable :: lowered

        found = .false.
        if (len_trim(name) == 0) return
        if (.not. allocated(node%procedure_names)) return
        if (size(node%procedure_names) == 0) return

        do i = 1, size(node%procedure_names)
            lowered = to_lower(trim(node%procedure_names(i)%s))
            if (lowered == name) then
                found = .true.
                return
            end if
        end do
    end function module_procedure_has_name

    logical function node_name_matches(node_name, name) result(matches)
        character(len=:), allocatable, intent(in) :: node_name
        character(len=*), intent(in) :: name

        matches = .false.
        if (.not. allocated(node_name)) return
        if (len_trim(node_name) == 0) return
        if (to_lower(trim(node_name)) == name) matches = .true.
    end function node_name_matches

    subroutine emit_missing_explicit_interface(errors, original_name)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: original_name
        character(len=:), allocatable :: message
        character(len=:), allocatable :: suggestion

        message = "No explicit interface for procedure '" // trim(original_name) // &
                  "'"
        suggestion = "Move the procedure into a module or contains block, " // &
                     "or add an interface block"

        call errors%add_result(create_error_result( &
                               message, ERROR_SEMANTIC, &
                               component="semantic_analyzer", &
                               context="explicit_interface_requirement", &
                               suggestion=suggestion))
    end subroutine emit_missing_explicit_interface

end module semantic_explicit_interface_checker
