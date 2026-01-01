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
    public :: build_explicit_interface_name_cache

contains

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

        call emit_missing_explicit_interface(errors, expr%name)
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
            if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(i))%node)) cycle

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
            if (indices(i) <= 0 .or. indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(indices(i))%node)) cycle

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
