module semantic_explicit_interface_checker
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, program_node
    use ast_nodes_data, only: declaration_node, module_node, submodule_node
    use ast_nodes_misc, only: contains_node, interface_block_node, &
        module_procedure_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
        subroutine_def_node
    use error_handling, only: ERROR_SEMANTIC, create_error_result, &
        error_collection_t
    use identifier_table, only: identifier_table_t, identifier_table_find, &
        identifier_table_intern, &
        identifier_table_init, &
        identifier_table_reset, identifier_id_kind
    use intrinsic_registry, only: is_intrinsic_function, is_intrinsic_subroutine
    use scope_manager, only: scope_stack_t
    use string_utils_mod, only: to_lower
    use type_system_unified, only: TARRAY, mono_type_t, poly_type_t
    implicit none
    private

    public :: validate_explicit_interface_for_function_reference
    public :: validate_explicit_interface_for_subroutine_call
    public :: validate_call_target_has_no_type
    public :: build_explicit_interface_name_cache

    ! Classification of a name found in an enclosing scoping unit.
    integer, parameter :: TARGET_KIND_NONE = 0
    integer, parameter :: TARGET_KIND_PROC = 1
    integer, parameter :: TARGET_KIND_FUNC = 2
    integer, parameter :: TARGET_KIND_VAR = 3

    ! Guard against malformed parent chains.
    integer, parameter :: MAX_SCOPE_WALK = 64

contains

    ! F2018 C1521 / 15.5.1: the procedure designator in a CALL statement must
    ! name a subroutine.  A name that is declared with a type in an accessible
    ! scoping unit -- a local variable, or a function -- is not a subroutine.
    subroutine validate_call_target_has_no_type(arena, errors, expr, expr_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: expr
        integer, intent(in) :: expr_index

        character(len=:), allocatable :: proc_name
        integer :: scope_index
        integer :: verdict
        integer :: steps

        if (.not. allocated(expr%name)) return
        if (len_trim(expr%name) == 0) return

        proc_name = to_lower(trim(expr%name))
        if (is_intrinsic_subroutine(proc_name)) return

        scope_index = expr_index
        do steps = 1, MAX_SCOPE_WALK
            scope_index = enclosing_scope_index(arena, scope_index)
            if (scope_index <= 0) return
            verdict = classify_name_in_scope(arena, scope_index, proc_name)
            if (verdict == TARGET_KIND_NONE) cycle
            if (verdict == TARGET_KIND_PROC) return
            call emit_typed_call_target(errors, trim(expr%name), expr%line, &
                expr%column)
            return
        end do
    end subroutine validate_call_target_has_no_type

    ! Index of the nearest enclosing scoping unit, or 0 when there is none.
    integer function enclosing_scope_index(arena, from_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: from_index

        integer :: current
        integer :: steps

        scope_index = 0
        current = from_index
        do steps = 1, MAX_SCOPE_WALK
            if (current <= 0 .or. current > arena%size) return
            current = arena%entries(current)%parent_index
            if (current <= 0 .or. current > arena%size) return
            if (.not. allocated(arena%entries(current)%node)) cycle
            select type (node => arena%entries(current)%node)
                type is (program_node)
                scope_index = current
                return
                type is (module_node)
                scope_index = current
                return
                type is (submodule_node)
                scope_index = current
                return
                type is (function_def_node)
                scope_index = current
                return
                type is (subroutine_def_node)
                scope_index = current
                return
            class default
                cycle
            end select
        end do
    end function enclosing_scope_index

    integer function classify_name_in_scope(arena, scope_index, name) result(verdict)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: name

        verdict = TARGET_KIND_NONE
        if (.not. arena%has_node_at(scope_index)) return

        select type (node => arena%entries(scope_index)%node)
            type is (program_node)
            verdict = classify_name_in_indices(arena, node%body_indices, name)
            type is (function_def_node)
            verdict = classify_name_in_indices(arena, node%body_indices, name)
            type is (subroutine_def_node)
            verdict = classify_name_in_indices(arena, node%body_indices, name)
            type is (module_node)
            verdict = classify_name_in_indices(arena, node%declaration_indices, name)
            if (verdict == TARGET_KIND_NONE) then
                verdict = classify_name_in_indices(arena, node%procedure_indices, &
                    name)
            end if
            type is (submodule_node)
            verdict = classify_name_in_indices(arena, node%declaration_indices, name)
            if (verdict == TARGET_KIND_NONE) then
                verdict = classify_name_in_indices(arena, node%procedure_indices, &
                    name)
            end if
        class default
            verdict = TARGET_KIND_NONE
        end select
    end function classify_name_in_scope

    ! Procedure evidence wins over type evidence inside one scoping unit, so a
    ! subroutine that also has a stale declaration is never reported.
    integer function classify_name_in_indices(arena, indices, name) result(verdict)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        character(len=*), intent(in) :: name

        integer :: i
        integer :: typed_verdict

        verdict = TARGET_KIND_NONE
        typed_verdict = TARGET_KIND_NONE
        if (.not. allocated(indices)) return

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (child => arena%entries(indices(i))%node)
                type is (subroutine_def_node)
                if (names_match(child%name, name)) then
                    verdict = TARGET_KIND_PROC
                    return
                end if
                type is (interface_block_node)
                if (interface_declares_subroutine(arena, child, name)) then
                    verdict = TARGET_KIND_PROC
                    return
                end if
                type is (function_def_node)
                if (names_match(child%name, name)) then
                    typed_verdict = TARGET_KIND_FUNC
                end if
                type is (declaration_node)
                if (declaration_gives_name_a_type(child, name)) then
                    typed_verdict = TARGET_KIND_VAR
                end if
            class default
                cycle
            end select
        end do

        verdict = typed_verdict
    end function classify_name_in_indices

    logical function interface_declares_subroutine(arena, block, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: block
        character(len=*), intent(in) :: name

        integer :: i
        integer :: j

        found = .false.
        if (.not. allocated(block%procedure_indices)) return

        do i = 1, size(block%procedure_indices)
            if (.not. arena%has_node_at(block%procedure_indices(i))) cycle
            select type (proc => arena%entries(block%procedure_indices(i))%node)
                type is (subroutine_def_node)
                if (names_match(proc%name, name)) then
                    found = .true.
                    return
                end if
                type is (module_procedure_node)
                if (.not. allocated(proc%procedure_names)) cycle
                do j = 1, size(proc%procedure_names)
                    if (to_lower(trim(proc%procedure_names(j)%s)) == name) then
                        found = .true.
                        return
                    end if
                end do
            class default
                cycle
            end select
        end do
    end function interface_declares_subroutine

    ! A declaration only gives the name a type when it is a data object.
    ! EXTERNAL and procedure-pointer declarations name procedures instead.
    logical function declaration_gives_name_a_type(decl, name) result(is_typed)
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: name

        integer :: j

        is_typed = .false.
        if (decl%is_external) return
        if (decl%is_pointer) return

        if (decl%is_multi_declaration) then
            if (.not. allocated(decl%var_names)) return
            do j = 1, size(decl%var_names)
                if (to_lower(trim(decl%var_names(j))) == name) then
                    is_typed = .true.
                    return
                end if
            end do
        else
            is_typed = names_match(decl%var_name, name)
        end if
    end function declaration_gives_name_a_type

    logical function names_match(node_name, lowered_name) result(matches)
        character(len=:), allocatable, intent(in) :: node_name
        character(len=*), intent(in) :: lowered_name

        matches = .false.
        if (.not. allocated(node_name)) return
        if (len_trim(node_name) == 0) return
        matches = to_lower(trim(node_name)) == lowered_name
    end function names_match

    subroutine emit_typed_call_target(errors, original_name, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: original_name
        integer, intent(in) :: line, column

        call errors%add_result(create_error_result( &
            "'"//original_name//"' has a type, which is not consistent with "// &
            "the CALL", ERROR_SEMANTIC, &
            component="semantic_analyzer", &
            context="call_target_has_type", &
            suggestion="CALL requires a subroutine; use a function reference "// &
            "in an expression instead", &
            line=line, column=column, end_line=line, end_column=column + 1))
    end subroutine emit_typed_call_target

    subroutine validate_explicit_interface_for_function_reference(arena, scopes, &
            cache, errors, &
            expr, expr_index)
        type(ast_arena_t), intent(in) :: arena
        type(scope_stack_t), intent(inout) :: scopes
        type(identifier_table_t), intent(in) :: cache
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
        if (has_explicit_interface_in_cache(cache, proc_name)) return

        call emit_missing_explicit_interface(errors, expr%name, expr%line, &
            expr%column)
    end subroutine validate_explicit_interface_for_function_reference

    subroutine validate_explicit_interface_for_subroutine_call(arena, scopes, &
            cache, errors, &
            expr, expr_index)
        type(ast_arena_t), intent(in) :: arena
        type(scope_stack_t), intent(inout) :: scopes
        type(identifier_table_t), intent(in) :: cache
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: expr
        integer, intent(in) :: expr_index

        character(len=:), allocatable :: proc_name

        if (.not. allocated(expr%name)) return
        if (len_trim(expr%name) == 0) return

        proc_name = to_lower(trim(expr%name))
        if (is_intrinsic_subroutine(proc_name)) return
        if (has_explicit_interface_in_cache(cache, proc_name)) return

        call emit_missing_explicit_interface(errors, expr%name, expr%line, &
            expr%column)
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

    subroutine build_explicit_interface_name_cache(arena, cache)
        type(ast_arena_t), intent(in) :: arena
        type(identifier_table_t), intent(inout) :: cache

        integer :: i

        if (.not. allocated(cache%buckets)) then
            call identifier_table_init(cache)
        end if
        call identifier_table_reset(cache)

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle

            select type (node => arena%entries(i)%node)
                type is (interface_block_node)
                call cache_procedure_names_from_indices(arena, &
                    node%procedure_indices, &
                    cache)
                type is (module_node)
                call cache_procedure_names_from_indices(arena, &
                    node%procedure_indices, &
                    cache)
                type is (program_node)
                call cache_internal_procedure_names(arena, node%body_indices, &
                    cache)
                type is (function_def_node)
                call cache_internal_procedure_names(arena, node%body_indices, &
                    cache)
                type is (subroutine_def_node)
                call cache_internal_procedure_names(arena, node%body_indices, &
                    cache)
            class default
                cycle
            end select
        end do
    end subroutine build_explicit_interface_name_cache

    logical function has_explicit_interface_in_cache(cache, name) result(found)
        type(identifier_table_t), intent(in) :: cache
        character(len=*), intent(in) :: name

        found = .false.
        if (len_trim(name) == 0) return

        found = identifier_table_find(cache, name) > 0
    end function has_explicit_interface_in_cache

    subroutine cache_internal_procedure_names(arena, body_indices, cache)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        type(identifier_table_t), intent(inout) :: cache

        integer :: i
        logical :: in_contains

        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        in_contains = .false.
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle

            select type (node => arena%entries(body_indices(i))%node)
                type is (contains_node)
                in_contains = .true.
                type is (function_def_node)
                if (in_contains) call cache_allocated_name(node%name, cache)
                type is (subroutine_def_node)
                if (in_contains) call cache_allocated_name(node%name, cache)
            class default
                cycle
            end select
        end do
    end subroutine cache_internal_procedure_names

    subroutine cache_procedure_names_from_indices(arena, indices, cache)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        type(identifier_table_t), intent(inout) :: cache

        integer :: i

        if (.not. allocated(indices)) return
        if (size(indices) == 0) return

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle

            select type (proc => arena%entries(indices(i))%node)
                type is (function_def_node)
                call cache_allocated_name(proc%name, cache)
                type is (subroutine_def_node)
                call cache_allocated_name(proc%name, cache)
                type is (module_procedure_node)
                call cache_module_procedure_names(proc, cache)
            class default
                cycle
            end select
        end do
    end subroutine cache_procedure_names_from_indices

    subroutine cache_module_procedure_names(proc, cache)
        type(module_procedure_node), intent(in) :: proc
        type(identifier_table_t), intent(inout) :: cache

        integer :: i
        character(len=:), allocatable :: lowered
        integer(identifier_id_kind) :: interned_id

        if (.not. allocated(proc%procedure_names)) return
        if (size(proc%procedure_names) == 0) return

        do i = 1, size(proc%procedure_names)
            lowered = to_lower(trim(proc%procedure_names(i)%s))
            if (len_trim(lowered) == 0) cycle
            interned_id = identifier_table_intern(cache, lowered)
        end do
    end subroutine cache_module_procedure_names

    subroutine cache_allocated_name(node_name, cache)
        character(len=:), allocatable, intent(in) :: node_name
        type(identifier_table_t), intent(inout) :: cache

        character(len=:), allocatable :: lowered
        integer(identifier_id_kind) :: interned_id

        if (.not. allocated(node_name)) return
        lowered = to_lower(trim(node_name))
        if (len_trim(lowered) == 0) return

        interned_id = identifier_table_intern(cache, lowered)
    end subroutine cache_allocated_name

    subroutine emit_missing_explicit_interface(errors, original_name, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: original_name
        integer, intent(in) :: line, column
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
            suggestion=suggestion, line=line, column=column, end_line=line, &
            end_column=column + 1))
    end subroutine emit_missing_explicit_interface

end module semantic_explicit_interface_checker
