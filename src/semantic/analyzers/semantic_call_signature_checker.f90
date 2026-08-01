module semantic_call_signature_checker
    ! Rejects procedure-call signature mismatches (issue #2882).
    !
    ! One structural sweep over the arena visits every scoping unit, keeps the
    ! stack of enclosing scopes, and validates each CALL statement it reaches:
    !
    !   * the procedure designator must name a subroutine, not a data object
    !     or a function (F2018 15.5.1);
    !   * an actual argument that is passed to a dummy procedure must be a
    !     procedure whose characteristics match the dummy's interface
    !     (F2018 15.5.2.9): same procedure kind, argument count, result type,
    !     INTENT and OPTIONAL attributes.
    !
    ! Data-argument type checking of the call is delegated to the strict
    ! argument type checker so exactly one implementation of that rule exists.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_conditional, only: if_node
    use ast_base, only: LITERAL_INTEGER, LITERAL_LOGICAL, LITERAL_REAL, &
        LITERAL_STRING
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
        subroutine_def_node
    use error_handling, only: ERROR_SEMANTIC, create_error_result, &
        error_collection_t
    use intrinsic_registry, only: get_intrinsic_info, is_intrinsic_subroutine
    use semantic_call_signature_scopes, only: ENTITY_EXTERNAL, ENTITY_FUNCTION, &
        ENTITY_INTRINSIC, ENTITY_NONE, ENTITY_PROCEDURE, ENTITY_SUBROUTINE, &
        ENTITY_VARIABLE, MAX_SCOPE_DEPTH, declaration_names_entity, &
        find_dummy_procedure_interface, is_scoping_unit, name_entity_t, &
        resolve_name_in_scopes, scope_specification_indices
    use semantic_strict_argument_type_checker_validation, only: &
        validate_call_against_interface
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: validate_call_signatures_in_arena

contains

    subroutine validate_call_signatures_in_arena(arena, errors, strict_mode)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode

        logical, allocatable :: is_nested(:)
        integer :: scope_stack(MAX_SCOPE_DEPTH)
        integer :: i

        if (arena%size <= 0) return

        allocate (is_nested(arena%size))
        is_nested = .false.
        call mark_nested_scopes(arena, is_nested)

        scope_stack = 0
        do i = 1, arena%size
            if (is_nested(i)) cycle
            if (.not. is_scoping_unit(arena, i)) cycle
            call walk_scope(arena, errors, strict_mode, scope_stack, 0, i)
        end do
    end subroutine validate_call_signatures_in_arena

    ! A scoping unit reached from another scoping unit's specification part is
    ! visited through its parent, never as a sweep root.
    subroutine mark_nested_scopes(arena, is_nested)
        type(ast_arena_t), intent(in) :: arena
        logical, intent(inout) :: is_nested(:)

        integer, allocatable :: indices(:)
        integer :: i
        integer :: j

        do i = 1, arena%size
            if (.not. is_scoping_unit(arena, i)) cycle
            call scope_specification_indices(arena, i, indices)
            if (.not. allocated(indices)) cycle
            do j = 1, size(indices)
                if (indices(j) <= 0 .or. indices(j) > arena%size) cycle
                if (indices(j) == i) cycle
                if (.not. is_scoping_unit(arena, indices(j))) cycle
                is_nested(indices(j)) = .true.
            end do
        end do
    end subroutine mark_nested_scopes

    recursive subroutine walk_scope(arena, errors, strict_mode, scope_stack, &
            depth, scope_index)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        integer, intent(inout) :: scope_stack(:)
        integer, intent(in) :: depth
        integer, intent(in) :: scope_index

        integer, allocatable :: indices(:)
        integer :: inner_depth
        integer :: i

        if (depth >= MAX_SCOPE_DEPTH) return
        inner_depth = depth + 1
        scope_stack(inner_depth) = scope_index

        call scope_specification_indices(arena, scope_index, indices)
        if (.not. allocated(indices)) return

        do i = 1, size(indices)
            if (indices(i) <= 0 .or. indices(i) > arena%size) cycle
            if (indices(i) == scope_index) cycle
            if (is_scoping_unit(arena, indices(i))) then
                call walk_scope(arena, errors, strict_mode, scope_stack, &
                    inner_depth, indices(i))
            else
                call check_statement(arena, errors, strict_mode, scope_stack, &
                    inner_depth, indices(i))
            end if
        end do
    end subroutine walk_scope

    recursive subroutine check_statement(arena, errors, strict_mode, scope_stack, &
            depth, stmt_index)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        integer, intent(inout) :: scope_stack(:)
        integer, intent(in) :: depth
        integer, intent(in) :: stmt_index

        integer, allocatable :: nested(:)
        integer :: i

        if (.not. arena%has_node_at(stmt_index)) return

        select type (stmt => arena%entries(stmt_index)%node)
            type is (subroutine_call_node)
            call check_call(arena, errors, strict_mode, scope_stack, depth, stmt)
            return
            type is (do_loop_node)
            if (allocated(stmt%body_indices)) nested = stmt%body_indices
            type is (do_while_node)
            if (allocated(stmt%body_indices)) nested = stmt%body_indices
            type is (block_construct_node)
            if (allocated(stmt%body_indices)) nested = stmt%body_indices
            type is (associate_node)
            if (allocated(stmt%body_indices)) nested = stmt%body_indices
            type is (if_node)
            nested = if_body_indices(stmt)
        class default
            return
        end select

        if (.not. allocated(nested)) return
        do i = 1, size(nested)
            call check_statement(arena, errors, strict_mode, scope_stack, depth, &
                nested(i))
        end do
    end subroutine check_statement

    function if_body_indices(stmt) result(indices)
        type(if_node), intent(in) :: stmt
        integer, allocatable :: indices(:)
        integer :: i

        allocate (indices(0))
        if (allocated(stmt%then_body_indices)) then
            indices = [indices, stmt%then_body_indices]
        end if
        if (allocated(stmt%elseif_blocks)) then
            do i = 1, size(stmt%elseif_blocks)
                if (.not. allocated(stmt%elseif_blocks(i)%body_indices)) cycle
                indices = [indices, stmt%elseif_blocks(i)%body_indices]
            end do
        end if
        if (allocated(stmt%else_body_indices)) then
            indices = [indices, stmt%else_body_indices]
        end if
    end function if_body_indices

    subroutine check_call(arena, errors, strict_mode, scope_stack, depth, stmt)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
        integer, intent(in) :: scope_stack(:)
        integer, intent(in) :: depth
        type(subroutine_call_node), intent(in) :: stmt

        character(len=:), allocatable :: proc_name
        type(name_entity_t) :: entity
        type(name_entity_t) :: local

        if (.not. allocated(stmt%name)) return
        if (len_trim(stmt%name) == 0) return

        proc_name = to_lower(trim(stmt%name))
        ! A part reference (type-bound call, coindexed or array-element base)
        ! is not a bare procedure name, so name lookup is meaningless.
        if (index(proc_name, '%') > 0) return
        if (index(proc_name, '[') > 0) return
        if (index(proc_name, '(') > 0) return
        if (is_intrinsic_subroutine(proc_name)) return

        entity = resolve_name_in_scopes(arena, scope_stack, depth, proc_name)

        select case (entity%kind)
        case (ENTITY_FUNCTION, ENTITY_VARIABLE)
            call emit_typed_call_target(errors, trim(stmt%name), stmt%line, &
                stmt%column)
            return
        case (ENTITY_SUBROUTINE)
            call check_actual_procedure_arguments(arena, errors, scope_stack, &
                depth, stmt, entity%def_index)
            ! Only an interface body of the calling scope itself is taken as
            ! the characterisation of the procedure. An interface body further
            ! out may be shadowed by an EXTERNAL declaration or by host
            ! association, so no argument conclusion is drawn from it.
            local = resolve_name_in_scopes(arena, scope_stack(depth:depth), 1, &
                proc_name)
            if (local%from_interface_body) then
                if (local%def_index == entity%def_index) then
                    call check_interface_body_literals(arena, errors, stmt, &
                        entity%def_index)
                end if
            end if
            if (strict_mode) then
                call check_data_arguments(arena, errors, stmt, entity%def_index)
            end if
        end select
    end subroutine check_call


    subroutine check_data_arguments(arena, errors, stmt, def_index)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        integer, intent(in) :: def_index

        if (.not. arena%has_node_at(def_index)) return
        select type (iface => arena%entries(def_index)%node)
            type is (subroutine_def_node)
            call validate_call_against_interface(arena, errors, stmt%name, &
                stmt%arg_indices, iface%param_indices, iface%body_indices)
        class default
            return
        end select
    end subroutine check_data_arguments

    ! F2018 15.5.2.9: an actual argument associated with a dummy procedure
    ! shall be a procedure with the same characteristics.
    subroutine check_actual_procedure_arguments(arena, errors, scope_stack, depth, &
            stmt, def_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: scope_stack(:)
        integer, intent(in) :: depth
        type(subroutine_call_node), intent(in) :: stmt
        integer, intent(in) :: def_index

        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        character(len=:), allocatable :: dummy_name
        character(len=:), allocatable :: actual_name
        type(name_entity_t) :: actual
        integer :: iface_index
        integer :: i

        if (.not. arena%has_node_at(def_index)) return
        if (.not. allocated(stmt%arg_indices)) return

        select type (iface => arena%entries(def_index)%node)
            type is (subroutine_def_node)
            if (.not. allocated(iface%param_indices)) return
            param_indices = iface%param_indices
            if (allocated(iface%body_indices)) body_indices = iface%body_indices
        class default
            return
        end select
        if (.not. allocated(body_indices)) allocate (body_indices(0))

        do i = 1, min(size(stmt%arg_indices), size(param_indices))
            actual_name = identifier_name(arena, stmt%arg_indices(i))
            if (len_trim(actual_name) == 0) cycle
            dummy_name = param_name(arena, param_indices(i))
            if (len_trim(dummy_name) == 0) cycle

            actual = resolve_name_in_scopes(arena, scope_stack, depth, actual_name)
            if (actual%kind == ENTITY_NONE) cycle
            if (actual%kind == ENTITY_VARIABLE) cycle
            if (actual%kind == ENTITY_PROCEDURE) cycle

            iface_index = find_dummy_procedure_interface(arena, def_index, &
                dummy_name)
            if (iface_index > 0) then
                call compare_with_dummy_interface(arena, errors, stmt, actual, &
                    actual_name, iface_index)
                cycle
            end if
            call compare_with_external_dummy(arena, errors, stmt, actual, &
                actual_name, body_indices, dummy_name)
        end do
    end subroutine check_actual_procedure_arguments

    subroutine compare_with_dummy_interface(arena, errors, stmt, actual, &
            actual_name, iface_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        type(name_entity_t), intent(in) :: actual
        character(len=*), intent(in) :: actual_name
        integer, intent(in) :: iface_index

        select type (iface => arena%entries(iface_index)%node)
            type is (function_def_node)
            call compare_actual_with_function_dummy(arena, errors, stmt, actual, &
                actual_name, iface)
            type is (subroutine_def_node)
            call compare_actual_with_subroutine_dummy(arena, errors, stmt, actual, &
                actual_name, iface)
        class default
            return
        end select
    end subroutine compare_with_dummy_interface

    subroutine compare_actual_with_function_dummy(arena, errors, stmt, actual, &
            actual_name, iface)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        type(name_entity_t), intent(in) :: actual
        character(len=*), intent(in) :: actual_name
        type(function_def_node), intent(in) :: iface

        character(len=:), allocatable :: signature
        logical :: found
        integer :: expected_count
        integer :: actual_count

        expected_count = 0
        if (allocated(iface%param_indices)) expected_count = size(iface%param_indices)

        select case (actual%kind)
        case (ENTITY_INTRINSIC)
            call get_intrinsic_info(actual_name, found, signature)
            if (.not. found) return
            actual_count = intrinsic_argument_count(signature)
            if (actual_count == expected_count) return
            call emit_call_error(errors, "interface of '"//trim(actual_name)// &
                "' has the wrong number of arguments for the dummy procedure '"// &
                trim(iface%name)//"'", &
                "pass a procedure whose interface matches the dummy procedure", &
                stmt%line, stmt%column)
        case (ENTITY_SUBROUTINE)
            call emit_call_error(errors, "'"//trim(actual_name)// &
                "' is not a function, but the dummy procedure '"// &
                trim(iface%name)//"' is", &
                "pass a function, or change the dummy procedure interface", &
                stmt%line, stmt%column)
        case (ENTITY_FUNCTION)
            if (actual%def_index <= 0) return
            if (.not. arena%has_node_at(actual%def_index)) return
            select type (target => arena%entries(actual%def_index)%node)
                type is (function_def_node)
                actual_count = 0
                if (allocated(target%param_indices)) then
                    actual_count = size(target%param_indices)
                end if
                if (actual_count /= expected_count) then
                    call emit_call_error(errors, "'"//trim(actual_name)// &
                        "' has the wrong number of arguments for the dummy "// &
                        "function '"//trim(iface%name)//"'", &
                        "match the dummy function interface", stmt%line, &
                        stmt%column)
                    return
                end if
                if (function_types_differ(iface%return_type, &
                        target%return_type)) then
                    call emit_call_error(errors, &
                        "Type mismatch in function result of '"// &
                        trim(actual_name)//"'", &
                        "give the actual function the result type the dummy "// &
                        "procedure interface requires", stmt%line, stmt%column)
                    return
                end if
                call compare_function_dummy_types(arena, errors, stmt, iface, &
                    target, actual_name)
            class default
                return
            end select
        case (ENTITY_EXTERNAL)
            if (external_has_type(arena, actual%decl_index, actual_name)) return
            call emit_call_error(errors, "'"//trim(actual_name)// &
                "' is not a function, but the dummy procedure '"// &
                trim(iface%name)//"' is", &
                "declare the actual argument as a function, for example with "// &
                "an interface block", &
                stmt%line, stmt%column)
        end select
    end subroutine compare_actual_with_function_dummy

    subroutine compare_function_dummy_types(arena, errors, stmt, expected, &
            actual, actual_name)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        type(function_def_node), intent(in) :: expected
        type(function_def_node), intent(in) :: actual
        character(len=*), intent(in) :: actual_name

        character(len=:), allocatable :: expected_type
        character(len=:), allocatable :: actual_type
        character(len=:), allocatable :: dummy_name
        integer :: i
        integer :: expected_count

        expected_count = 0
        if (allocated(expected%param_indices)) then
            expected_count = size(expected%param_indices)
        end if

        do i = 1, expected_count
            dummy_name = param_name(arena, expected%param_indices(i))
            if (len_trim(dummy_name) == 0) cycle
            call dummy_type_text(arena, expected%body_indices, dummy_name, &
                expected_type)
            call dummy_type_text(arena, actual%body_indices, &
                param_name(arena, actual%param_indices(i)), actual_type)
            if (len_trim(expected_type) == 0 .or. &
                    len_trim(actual_type) == 0) cycle
            if (function_types_differ(expected_type, actual_type)) then
                call emit_call_error(errors, "Type mismatch in argument '"// &
                    trim(dummy_name)//"' of procedure '"//trim(actual_name)// &
                    "'", &
                    "give the actual function the same dummy argument type as "// &
                    "the interface", stmt%line, stmt%column)
                return
            end if
        end do
    end subroutine compare_function_dummy_types

    subroutine dummy_type_text(arena, body_indices, name, type_text)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: type_text

        integer :: i

        type_text = ''
        if (.not. allocated(body_indices)) return
        if (len_trim(name) == 0) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (decl => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. declaration_names_entity(decl, to_lower(trim(name)))) &
                    cycle
                if (allocated(decl%type_name)) type_text = trim(decl%type_name)
                return
            class default
                cycle
            end select
        end do
    end subroutine dummy_type_text

    logical function function_types_differ(expected, actual) result(differ)
        character(len=:), allocatable, intent(in) :: expected
        character(len=:), allocatable, intent(in) :: actual

        differ = .false.
        if (len_trim(expected) == 0 .or. len_trim(actual) == 0) return
        differ = to_lower(trim(expected)) /= to_lower(trim(actual))
    end function function_types_differ

    subroutine compare_actual_with_subroutine_dummy(arena, errors, stmt, actual, &
            actual_name, iface)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        type(name_entity_t), intent(in) :: actual
        character(len=*), intent(in) :: actual_name
        type(subroutine_def_node), intent(in) :: iface

        integer :: i
        integer :: expected_count
        integer :: actual_count
        character(len=:), allocatable :: expected_intent
        character(len=:), allocatable :: actual_intent
        character(len=:), allocatable :: dummy_name
        logical :: expected_optional
        logical :: actual_optional
        logical :: expected_found
        logical :: actual_found

        if (actual%kind /= ENTITY_SUBROUTINE) return
        if (actual%def_index <= 0) return
        if (.not. arena%has_node_at(actual%def_index)) return

        expected_count = 0
        if (allocated(iface%param_indices)) expected_count = size(iface%param_indices)

        select type (target => arena%entries(actual%def_index)%node)
            type is (subroutine_def_node)
            actual_count = 0
            if (allocated(target%param_indices)) then
                actual_count = size(target%param_indices)
            end if
            if (actual_count /= expected_count) then
                call emit_call_error(errors, "'"//trim(actual_name)// &
                    "' has the wrong number of arguments for the dummy "// &
                    "procedure '"//trim(iface%name)//"'", &
                    "match the dummy procedure interface", stmt%line, stmt%column)
                return
            end if
            do i = 1, expected_count
                dummy_name = param_name(arena, iface%param_indices(i))
                call dummy_attributes(arena, iface%body_indices, &
                    dummy_name, expected_intent, expected_optional, expected_found)
                call dummy_attributes(arena, target%body_indices, &
                    param_name(arena, target%param_indices(i)), actual_intent, &
                    actual_optional, actual_found)
                if (.not. expected_found) cycle
                if (.not. actual_found) cycle
                if (expected_intent /= actual_intent) then
                    call emit_call_error(errors, "INTENT mismatch in argument '"// &
                        trim(dummy_name)//"' of procedure '"// &
                        trim(actual_name)//"'", &
                        "give the actual procedure the same INTENT as the "// &
                        "dummy procedure interface", stmt%line, stmt%column)
                    return
                end if
                if (expected_optional .neqv. actual_optional) then
                    call emit_call_error(errors, "OPTIONAL mismatch in argument '"// &
                        trim(dummy_name)//"' of procedure '"// &
                        trim(actual_name)//"'", &
                        "give the actual procedure the same OPTIONAL attribute "// &
                        "as the dummy procedure interface", stmt%line, stmt%column)
                    return
                end if
            end do
        class default
            return
        end select
    end subroutine compare_actual_with_subroutine_dummy

    ! A dummy procedure may also be declared as a typed EXTERNAL, which makes
    ! it an external function with that result type.
    subroutine compare_with_external_dummy(arena, errors, stmt, actual, &
            actual_name, body_indices, dummy_name)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        type(name_entity_t), intent(in) :: actual
        character(len=*), intent(in) :: actual_name
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: dummy_name

        character(len=:), allocatable :: expected_type
        character(len=:), allocatable :: actual_type

        if (actual%kind /= ENTITY_EXTERNAL) return
        expected_type = external_declaration_type(arena, body_indices, dummy_name)
        if (len_trim(expected_type) == 0) return
        actual_type = declaration_type_text(arena, actual%decl_index)
        if (len_trim(actual_type) == 0) return
        if (expected_type == actual_type) return

        call emit_call_error(errors, "Type mismatch in function result of '"// &
            trim(actual_name)//"': passing "//actual_type//" to "//expected_type, &
            "declare the actual function with the result type the dummy "// &
            "procedure requires", stmt%line, stmt%column)
    end subroutine compare_with_external_dummy

    function external_declaration_type(arena, body_indices, name) result(type_text)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: type_text

        integer :: i

        type_text = ''
        if (.not. allocated(body_indices)) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (decl => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. decl%is_external) cycle
                if (.not. declaration_names_entity(decl, to_lower(trim(name)))) cycle
                type_text = declaration_type_text(arena, body_indices(i))
                return
            class default
                cycle
            end select
        end do
    end function external_declaration_type

    logical function external_has_type(arena, decl_index, name) result(has_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        character(len=*), intent(in) :: name

        has_type = len_trim(declaration_type_text(arena, decl_index)) > 0
    end function external_has_type

    function declaration_type_text(arena, decl_index) result(type_text)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        character(len=:), allocatable :: type_text

        type_text = ''
        if (decl_index <= 0) return
        if (.not. arena%has_node_at(decl_index)) return

        select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (.not. allocated(decl%type_name)) return
            type_text = to_lower(trim(decl%type_name))
            ! A bare EXTERNAL statement records no type for the name.
            if (type_text == 'external') type_text = ''
        class default
            return
        end select
    end function declaration_type_text

    subroutine dummy_attributes(arena, body_indices, name, intent_text, &
            is_optional, found)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: intent_text
        logical, intent(out) :: is_optional
        logical, intent(out) :: found

        integer :: i

        intent_text = ''
        is_optional = .false.
        found = .false.
        if (.not. allocated(body_indices)) return
        if (len_trim(name) == 0) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (decl => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. declaration_names_entity(decl, to_lower(trim(name)))) cycle
                found = .true.
                is_optional = decl%is_optional
                if (decl%has_intent) then
                    if (allocated(decl%intent)) intent_text = to_lower(trim(decl%intent))
                end if
                return
            class default
                cycle
            end select
        end do
    end subroutine dummy_attributes

    ! Number of arguments in a registry signature of the form
    ! `return_type(arg_type[,arg_type...])`.
    integer function intrinsic_argument_count(signature) result(count)
        character(len=*), intent(in) :: signature

        character(len=:), allocatable :: args
        integer :: open_paren
        integer :: close_paren
        integer :: i

        count = 0
        open_paren = index(signature, '(')
        close_paren = index(signature, ')', back=.true.)
        if (open_paren <= 0) return
        if (close_paren <= open_paren) return

        args = trim(signature(open_paren + 1:close_paren - 1))
        if (len_trim(args) == 0) return

        count = 1
        do i = 1, len_trim(args)
            if (args(i:i) == ',') count = count + 1
        end do
    end function intrinsic_argument_count

    function identifier_name(arena, index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        name = ''
        if (.not. arena%has_node_at(index)) return

        select type (node => arena%entries(index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
        class default
            return
        end select
    end function identifier_name

    function param_name(arena, index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        name = ''
        if (.not. arena%has_node_at(index)) return

        select type (node => arena%entries(index)%node)
            type is (parameter_declaration_node)
            if (allocated(node%name)) name = trim(node%name)
            type is (declaration_node)
            if (allocated(node%var_name)) name = trim(node%var_name)
            type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
        class default
            return
        end select
    end function param_name

    ! An interface body gives the called subroutine an explicit interface, so a
    ! literal actual argument must agree in type with its dummy argument
    ! (F2018 15.5.2.4). Only literals are inspected: their type is known
    ! exactly, so this cannot misjudge an inferred expression.
    subroutine check_interface_body_literals(arena, errors, stmt, def_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(subroutine_call_node), intent(in) :: stmt
        integer, intent(in) :: def_index

        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        character(len=:), allocatable :: actual_class
        character(len=:), allocatable :: dummy_class
        character(len=:), allocatable :: dummy_name
        integer :: i

        if (.not. arena%has_node_at(def_index)) return
        if (.not. allocated(stmt%arg_indices)) return

        select type (iface => arena%entries(def_index)%node)
            type is (subroutine_def_node)
            if (.not. allocated(iface%param_indices)) return
            param_indices = iface%param_indices
            if (allocated(iface%body_indices)) body_indices = iface%body_indices
        class default
            return
        end select
        if (.not. allocated(body_indices)) allocate (body_indices(0))

        do i = 1, min(size(stmt%arg_indices), size(param_indices))
            actual_class = literal_type_class(arena, stmt%arg_indices(i))
            if (len_trim(actual_class) == 0) cycle
            dummy_name = param_name(arena, param_indices(i))
            if (len_trim(dummy_name) == 0) cycle
            dummy_class = dummy_type_class(arena, body_indices, dummy_name)
            if (len_trim(dummy_class) == 0) then
                ! Some interface bodies carry the dummy's type on the dummy
                ! node itself rather than in a separate declaration.
                dummy_class = param_type_class(arena, param_indices(i))
            end if
            if (len_trim(dummy_class) == 0) cycle
            if (dummy_class == actual_class) cycle
            call emit_call_error(errors, "Type mismatch in call to '"// &
                trim(stmt%name)//"': passing "//actual_class//" to "// &
                dummy_class//" dummy argument '"//trim(dummy_name)//"'", &
                "pass an actual argument of the type the interface declares", &
                stmt%line, stmt%column)
            return
        end do
    end subroutine check_interface_body_literals

    ! Intrinsic type class of a literal actual argument, or '' when the
    ! argument is not a literal or its class is not decided here.
    function literal_type_class(arena, index) result(class_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        character(len=:), allocatable :: class_name

        class_name = ''
        if (.not. arena%has_node_at(index)) return

        select type (node => arena%entries(index)%node)
            type is (literal_node)
            select case (node%literal_kind)
            case (LITERAL_INTEGER)
                class_name = 'integer'
            case (LITERAL_REAL)
                class_name = 'real'
            case (LITERAL_STRING)
                class_name = 'character'
            case (LITERAL_LOGICAL)
                class_name = 'logical'
            end select
        class default
            return
        end select
    end function literal_type_class

    ! Intrinsic type class recorded on the dummy argument node itself.
    function param_type_class(arena, index) result(class_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        character(len=:), allocatable :: class_name

        class_name = ''
        if (.not. arena%has_node_at(index)) return

        select type (node => arena%entries(index)%node)
            type is (parameter_declaration_node)
            if (node%is_array) return
            if (.not. allocated(node%type_name)) return
            class_name = type_name_class(node%type_name)
            type is (declaration_node)
            if (node%is_array) return
            if (node%is_pointer) return
            if (node%is_allocatable) return
            if (.not. allocated(node%type_name)) return
            class_name = type_name_class(node%type_name)
        class default
            return
        end select
    end function param_type_class

    function type_name_class(type_name) result(class_name)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: class_name

        character(len=:), allocatable :: base
        integer :: paren

        class_name = ''
        base = to_lower(trim(type_name))
        paren = index(base, '(')
        if (paren > 0) base = trim(base(1:paren - 1))
        select case (base)
        case ('integer')
            class_name = 'integer'
        case ('real', 'double precision', 'doubleprecision')
            class_name = 'real'
        case ('character')
            class_name = 'character'
        case ('logical')
            class_name = 'logical'
        end select
    end function type_name_class

    function dummy_type_class(arena, body_indices, name) result(class_name)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: class_name

        integer :: i

        class_name = ''
        if (.not. allocated(body_indices)) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (decl => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. declaration_names_entity(decl, to_lower(trim(name)))) cycle
                if (decl%is_array) return
                if (decl%is_pointer) return
                if (decl%is_allocatable) return
                if (.not. allocated(decl%type_name)) return
                class_name = type_name_class(decl%type_name)
                return
            class default
                cycle
            end select
        end do
    end function dummy_type_class

    subroutine emit_typed_call_target(errors, original_name, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: original_name
        integer, intent(in) :: line, column

        call emit_call_error(errors, "'"//original_name// &
            "' has a type, which is not consistent with the CALL", &
            "CALL requires a subroutine; use a function reference in an "// &
            "expression instead", line, column)
    end subroutine emit_typed_call_target

    subroutine emit_call_error(errors, message, suggestion, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: suggestion
        integer, intent(in) :: line, column

        call errors%add_result(create_error_result( &
            message, ERROR_SEMANTIC, &
            component="semantic_call_signature_checker", &
            context="call_signature_mismatch", &
            suggestion=suggestion, &
            line=line, column=column, end_line=line, end_column=column + 1))
    end subroutine emit_call_error

end module semantic_call_signature_checker
