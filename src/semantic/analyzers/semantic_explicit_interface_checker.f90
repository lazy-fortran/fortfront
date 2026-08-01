module semantic_explicit_interface_checker
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, program_node
    use ast_nodes_bounds, only: range_expression_node
    use ast_nodes_data, only: declaration_node, module_node, submodule_node, &
        multi_unit_container_node, parameter_declaration_node
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

    ! Guard against malformed parent chains.
    integer, parameter :: MAX_SCOPE_WALK = 64

    public :: validate_explicit_interface_for_function_reference
    public :: validate_explicit_interface_for_subroutine_call
    public :: validate_whole_file_explicit_interface
    public :: build_explicit_interface_name_cache
    public :: is_part_reference

contains

    ! F2018 15.4.2.2: a procedure reference requires an explicit interface when
    ! the procedure has a dummy argument with the ALLOCATABLE, ASYNCHRONOUS,
    ! OPTIONAL, POINTER, TARGET, VALUE or VOLATILE attribute, or an
    ! assumed-shape dummy. When such a procedure is an external subprogram of
    ! the same file, the reference can be checked here.
    ! gfortran.dg/whole_file_16.f90 and volatile14.f90 are the reference cases.
    ! Issue #2883.
    ! `standard_input` is false for lazy Fortran, whose standardizer moves an
    ! external subprogram into the main program before the result is compiled,
    ! so the requirement does not apply to the source as written.
    subroutine validate_whole_file_explicit_interface(arena, errors, &
            standard_input)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: standard_input

        type(identifier_table_t) :: required
        integer :: i

        if (.not. standard_input) return
        call identifier_table_init(required)
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (multi_unit_container_node)
                call collect_interface_requiring_units(arena, node%body_indices, &
                    required)
            class default
                cycle
            end select
        end do
        if (required%count == 0) return

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (subroutine_call_node)
                call check_call_needs_interface(arena, errors, required, node, i)
            class default
                cycle
            end select
        end do
    end subroutine validate_whole_file_explicit_interface

    ! External subprograms of this file whose interface must be explicit.
    subroutine collect_interface_requiring_units(arena, body_indices, required)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        type(identifier_table_t), intent(inout) :: required

        integer :: i

        if (.not. allocated(body_indices)) return
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (unit => arena%entries(body_indices(i))%node)
                type is (subroutine_def_node)
                if (.not. procedure_requires_explicit_interface(arena, &
                    unit%param_indices, unit%body_indices)) cycle
                call cache_allocated_name(unit%name, required)
                type is (function_def_node)
                if (.not. procedure_requires_explicit_interface(arena, &
                    unit%param_indices, unit%body_indices)) cycle
                call cache_allocated_name(unit%name, required)
            class default
                cycle
            end select
        end do
    end subroutine collect_interface_requiring_units

    ! True when a dummy argument carries an attribute or shape that forces the
    ! interface of the procedure to be explicit.
    logical function procedure_requires_explicit_interface(arena, param_indices, &
            body_indices) result(requires)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)

        integer :: i

        requires = .false.
        if (.not. allocated(param_indices)) return
        if (.not. allocated(body_indices)) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (decl => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. declaration_names_a_dummy(arena, param_indices, decl)) &
                    cycle
                if (declaration_forces_explicit_interface(arena, decl)) then
                    requires = .true.
                    return
                end if
            class default
                cycle
            end select
        end do
    end function procedure_requires_explicit_interface

    logical function declaration_names_a_dummy(arena, param_indices, decl) &
            result(is_dummy)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        type(declaration_node), intent(in) :: decl

        integer :: i

        is_dummy = .false.
        if (.not. allocated(decl%var_name)) return
        do i = 1, size(param_indices)
            if (.not. arena%has_node_at(param_indices(i))) cycle
            select type (param => arena%entries(param_indices(i))%node)
                type is (parameter_declaration_node)
                if (.not. allocated(param%name)) cycle
                if (to_lower(trim(param%name)) /= to_lower(trim(decl%var_name))) &
                    cycle
                is_dummy = .true.
                return
            class default
                cycle
            end select
        end do
    end function declaration_names_a_dummy

    logical function declaration_forces_explicit_interface(arena, decl) &
            result(forces)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl

        integer :: i

        forces = decl%is_allocatable .or. decl%is_pointer
        if (forces) return
        forces = decl%is_optional .or. decl%is_target
        if (forces) return
        forces = decl%is_value .or. decl%is_volatile .or. decl%is_asynchronous
        if (forces) return

        if (.not. decl%is_array) return
        if (.not. allocated(decl%dimension_indices)) return
        do i = 1, size(decl%dimension_indices)
            if (.not. arena%has_node_at(decl%dimension_indices(i))) cycle
            select type (bound => arena%entries(decl%dimension_indices(i))%node)
                type is (range_expression_node)
                if (bound%start_index > 0) cycle
                if (bound%end_index > 0) cycle
                forces = .true.
                return
            class default
                cycle
            end select
        end do
    end function declaration_forces_explicit_interface

    ! Report a call whose target needs an explicit interface that is not
    ! visible in any enclosing scoping unit of the call.
    subroutine check_call_needs_interface(arena, errors, required, expr, &
            expr_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(identifier_table_t), intent(in) :: required
        type(subroutine_call_node), intent(in) :: expr
        integer, intent(in) :: expr_index

        character(len=:), allocatable :: proc_name
        integer :: scope_index
        integer :: steps

        if (.not. allocated(expr%name)) return
        if (len_trim(expr%name) == 0) return
        proc_name = to_lower(trim(expr%name))
        if (is_part_reference(proc_name)) return
        if (identifier_table_find(required, proc_name) <= 0) return

        scope_index = expr_index
        do steps = 1, MAX_SCOPE_WALK
            scope_index = enclosing_scope_index(arena, scope_index)
            if (scope_index <= 0) exit
            if (scope_declares_name(arena, scope_index, proc_name)) return
        end do

        call errors%add_result(create_error_result( &
            "Explicit interface required for '"//trim(expr%name)// &
            "': the procedure has a dummy argument whose attributes or shape "// &
            "make an explicit interface mandatory", ERROR_SEMANTIC, &
            component="semantic_analyzer", &
            context="explicit_interface_requirement", &
            suggestion="Add an interface block for the procedure, or move it "// &
            "into a module", &
            line=expr%line, column=expr%column, end_line=expr%line, &
            end_column=expr%column + 1))
    end subroutine check_call_needs_interface

    ! Whether one scoping unit provides an explicit interface for the name,
    ! either through an interface block or by being the procedure itself.
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
            if (.not. arena%has_node_at(current)) cycle
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

    logical function names_match(node_name, lowered_name) result(matches)
        character(len=:), allocatable, intent(in) :: node_name
        character(len=*), intent(in) :: lowered_name

        matches = .false.
        if (.not. allocated(node_name)) return
        if (len_trim(node_name) == 0) return
        matches = to_lower(trim(node_name)) == lowered_name
    end function names_match

    ! Whether an interface block declares a procedure of this name.
    logical function interface_declares_subroutine(arena, block, name) &
            result(found)
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
                type is (function_def_node)
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

    logical function scope_declares_name(arena, scope_index, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: name

        found = .false.
        if (.not. arena%has_node_at(scope_index)) return
        select type (scope => arena%entries(scope_index)%node)
            type is (module_node)
            found = indices_declare_name(arena, scope%declaration_indices, name)
            if (found) return
            found = indices_declare_name(arena, scope%procedure_indices, name)
            type is (program_node)
            found = indices_declare_name(arena, scope%body_indices, name)
            type is (function_def_node)
            found = names_match(scope%name, name)
            if (found) return
            found = indices_declare_name(arena, scope%body_indices, name)
            type is (subroutine_def_node)
            found = names_match(scope%name, name)
            if (found) return
            found = indices_declare_name(arena, scope%body_indices, name)
        class default
            found = .false.
        end select
    end function scope_declares_name

    logical function indices_declare_name(arena, indices, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        character(len=*), intent(in) :: name

        integer :: i

        found = .false.
        if (.not. allocated(indices)) return
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
                type is (interface_block_node)
                found = interface_declares_subroutine(arena, node, name)
                if (found) return
                type is (function_def_node)
                found = names_match(node%name, name)
                if (found) return
                type is (subroutine_def_node)
                found = names_match(node%name, name)
                if (found) return
            class default
                cycle
            end select
        end do
    end function indices_declare_name

    ! True when the stored call designator is a part reference rather than a
    ! bare procedure name: a type-bound call (`b%show`), a coindexed call
    ! (`x[1]%c%sub`), or an array-element base (`arr(i)%sub`).  Such a call
    ! names a binding of a declared type, so name-based lookup of the stored
    ! text against declarations of the enclosing scope is meaningless.
    logical function is_part_reference(designator) result(is_part)
        character(len=*), intent(in) :: designator

        is_part = index(designator, '%') > 0
        if (is_part) return
        is_part = index(designator, '[') > 0
        if (is_part) return
        is_part = index(designator, '(') > 0
    end function is_part_reference

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
