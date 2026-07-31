module semantic_pure_dummy_validation
    ! Validates the dummy arguments of a PURE procedure.
    !
    ! F2008 C1279: a dummy argument of a PURE subprogram that has the
    ! INTENT(OUT) attribute shall not be polymorphic, because finalizing the
    ! actual argument could invoke an impure final subroutine.
    !
    ! F2008 C1290: a dummy procedure of a PURE procedure shall be PURE.
    !
    ! F2008 C1283: in a PURE subprogram a designator whose base object is a
    ! dummy argument of a PURE FUNCTION shall not appear in a variable
    ! definition context.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, &
        pointer_assignment_node, component_access_node, call_or_subscript_node, &
        range_subscript_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
        INTENT_OUT
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_conditional, only: if_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    public :: validate_pure_dummies

contains

    ! Entry point. The caller has already established that the enclosing
    ! procedure is PURE (explicitly or through ELEMENTAL).
    subroutine validate_pure_dummies(arena, param_indices, body_indices, &
            is_function, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)
        logical, intent(in) :: is_function
        type(error_collection_t), intent(inout) :: errors
        integer :: i
        character(len=:), allocatable :: name

        if (.not. allocated(param_indices)) return
        if (.not. allocated(body_indices)) return

        do i = 1, size(param_indices)
            name = dummy_name(arena, param_indices(i))
            if (len_trim(name) == 0) cycle
            call check_polymorphic_intent_out(arena, body_indices, name, errors)
            call check_dummy_procedure_purity(arena, body_indices, name, errors)
        end do

        if (is_function) then
            call check_dummy_definitions(arena, param_indices, body_indices, errors)
        end if
    end subroutine validate_pure_dummies

    ! C1279: polymorphic INTENT(OUT) dummy argument.
    subroutine check_polymorphic_intent_out(arena, body_indices, name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        type(error_collection_t), intent(inout) :: errors
        integer :: decl_index

        decl_index = find_declaration(arena, body_indices, name)
        if (decl_index <= 0) return

        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (.not. is_polymorphic_type(node%type_name)) return
            if (.not. node%has_intent) return
            if (.not. allocated(node%intent)) return
            if (to_lower(trim(node%intent)) /= 'out') return
            call report(errors, 'dummy argument "'//trim(name)// &
                '" of a PURE procedure may not be polymorphic with '// &
                'INTENT(OUT)', &
                'drop INTENT(OUT), make the dummy non-polymorphic, or drop '// &
                'the PURE prefix', node%line, node%column)
            type is (parameter_declaration_node)
            if (.not. is_polymorphic_type(node%type_name)) return
            if (node%intent_type /= INTENT_OUT) return
            call report(errors, 'dummy argument "'//trim(name)// &
                '" of a PURE procedure may not be polymorphic with '// &
                'INTENT(OUT)', &
                'drop INTENT(OUT), make the dummy non-polymorphic, or drop '// &
                'the PURE prefix', node%line, node%column)
        end select
    end subroutine check_polymorphic_intent_out

    ! C1290: a dummy procedure of a PURE procedure must itself be PURE.
    subroutine check_dummy_procedure_purity(arena, body_indices, name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        type(error_collection_t), intent(inout) :: errors
        integer :: i, j, proc_index

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
                type is (interface_block_node)
                if (.not. allocated(node%procedure_indices)) cycle
                do j = 1, size(node%procedure_indices)
                    proc_index = node%procedure_indices(j)
                    if (.not. arena%has_node_at(proc_index)) cycle
                    if (.not. interface_body_names(arena, proc_index, name)) cycle
                    if (interface_body_is_pure(arena, proc_index)) cycle
                    call report(errors, 'dummy procedure "'//trim(name)// &
                        '" of a PURE procedure must also be PURE', &
                        'add the PURE prefix to the dummy procedure '// &
                        'interface, or drop the PURE prefix', &
                        arena%entries(proc_index)%node%line, &
                        arena%entries(proc_index)%node%column)
                end do
            end select
        end do
    end subroutine check_dummy_procedure_purity

    ! C1283: dummy arguments of a PURE FUNCTION are not definable.
    subroutine check_dummy_definitions(arena, param_indices, body_indices, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors
        character(len=64), allocatable :: names(:)
        character(len=:), allocatable :: name
        integer :: i, count

        allocate (names(size(param_indices)))
        count = 0
        do i = 1, size(param_indices)
            name = dummy_name(arena, param_indices(i))
            if (len_trim(name) == 0) cycle
            if (len_trim(name) > len(names)) cycle
            if (dummy_has_value(arena, body_indices, name)) cycle
            count = count + 1
            names(count) = to_lower(trim(name))
        end do
        if (count == 0) return

        call scan_definitions(arena, body_indices, names(1:count), errors)
    end subroutine check_dummy_definitions

    recursive subroutine scan_definitions(arena, body_indices, names, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: names(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            call scan_definition_statement(arena, body_indices(i), names, errors)
        end do
    end subroutine scan_definitions

    recursive subroutine scan_definition_statement(arena, stmt_index, names, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=*), intent(in) :: names(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        select type (stmt => arena%entries(stmt_index)%node)
            type is (assignment_node)
            call report_if_dummy(arena, stmt%target_index, names, errors, &
                stmt%line, stmt%column)
            type is (pointer_assignment_node)
            call report_if_dummy(arena, stmt%pointer_index, names, errors, &
                stmt%line, stmt%column)
            type is (do_loop_node)
            if (allocated(stmt%body_indices)) &
                call scan_definitions(arena, stmt%body_indices, names, errors)
            type is (do_while_node)
            if (allocated(stmt%body_indices)) &
                call scan_definitions(arena, stmt%body_indices, names, errors)
            type is (if_node)
            if (allocated(stmt%then_body_indices)) &
                call scan_definitions(arena, stmt%then_body_indices, names, errors)
            if (allocated(stmt%elseif_blocks)) then
                do i = 1, size(stmt%elseif_blocks)
                    if (allocated(stmt%elseif_blocks(i)%body_indices)) &
                        call scan_definitions(arena, &
                        stmt%elseif_blocks(i)%body_indices, names, errors)
                end do
            end if
            if (allocated(stmt%else_body_indices)) &
                call scan_definitions(arena, stmt%else_body_indices, names, errors)
        end select
    end subroutine scan_definition_statement

    subroutine report_if_dummy(arena, target_index, names, errors, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index
        character(len=*), intent(in) :: names(:)
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: line, column
        character(len=:), allocatable :: base
        integer :: i

        base = base_object_name(arena, target_index)
        if (len_trim(base) == 0) return
        do i = 1, size(names)
            if (trim(names(i)) /= to_lower(trim(base))) cycle
            call report(errors, 'dummy argument "'//trim(base)// &
                '" of a PURE FUNCTION appears in a variable definition '// &
                'context', &
                'read the dummy argument only, or make the procedure a '// &
                'PURE SUBROUTINE', line, column)
            return
        end do
    end subroutine report_if_dummy

    ! Innermost base object of a designator such as a, a%b, a(i) or a(i:j).
    recursive function base_object_name(arena, node_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: name

        name = ''
        if (node_index <= 0) return
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
            type is (component_access_node)
            name = base_object_name(arena, node%base_expr_index)
            type is (range_subscript_node)
            name = base_object_name(arena, node%base_expr_index)
            type is (call_or_subscript_node)
            if (node%base_expr_index > 0) then
                name = base_object_name(arena, node%base_expr_index)
            else if (allocated(node%name)) then
                name = trim(node%name)
            end if
        end select
    end function base_object_name

    function dummy_name(arena, param_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=:), allocatable :: name

        name = ''
        if (.not. arena%has_node_at(param_index)) return

        select type (node => arena%entries(param_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
            type is (declaration_node)
            if (allocated(node%var_name)) name = trim(node%var_name)
            type is (parameter_declaration_node)
            if (allocated(node%name)) name = trim(node%name)
        end select
    end function dummy_name

    function find_declaration(arena, body_indices, name) result(decl_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        integer :: decl_index
        integer :: i

        decl_index = 0
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (declaration_names(node, name)) then
                    decl_index = body_indices(i)
                    return
                end if
                type is (parameter_declaration_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) == to_lower(trim(name))) then
                    decl_index = body_indices(i)
                    return
                end if
            end select
        end do
    end function find_declaration

    function declaration_names(node, name) result(matches)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: name
        logical :: matches
        integer :: i
        character(len=:), allocatable :: lowered

        matches = .false.
        lowered = to_lower(trim(name))
        if (allocated(node%var_name)) then
            if (to_lower(trim(node%var_name)) == lowered) then
                matches = .true.
                return
            end if
        end if
        if (.not. allocated(node%var_names)) return
        do i = 1, size(node%var_names)
            if (to_lower(trim(node%var_names(i))) == lowered) then
                matches = .true.
                return
            end if
        end do
    end function declaration_names

    function dummy_has_value(arena, body_indices, name) result(has_value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: name
        logical :: has_value
        integer :: decl_index

        has_value = .false.
        decl_index = find_declaration(arena, body_indices, name)
        if (decl_index <= 0) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            has_value = node%is_value
        end select
    end function dummy_has_value

    function is_polymorphic_type(type_name) result(is_polymorphic)
        character(len=:), allocatable, intent(in) :: type_name
        logical :: is_polymorphic
        character(len=:), allocatable :: lowered

        is_polymorphic = .false.
        if (.not. allocated(type_name)) return
        lowered = to_lower(trim(type_name))
        if (len(lowered) < 5) return
        is_polymorphic = lowered(1:5) == 'class'
    end function is_polymorphic_type

    function interface_body_names(arena, proc_index, name) result(matches)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index
        character(len=*), intent(in) :: name
        logical :: matches

        matches = .false.
        select type (node => arena%entries(proc_index)%node)
            type is (function_def_node)
            if (allocated(node%name)) &
                matches = to_lower(trim(node%name)) == to_lower(trim(name))
            type is (subroutine_def_node)
            if (allocated(node%name)) &
                matches = to_lower(trim(node%name)) == to_lower(trim(name))
        end select
    end function interface_body_names

    function interface_body_is_pure(arena, proc_index) result(is_pure)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index
        logical :: is_pure

        is_pure = .false.
        select type (node => arena%entries(proc_index)%node)
            type is (function_def_node)
            is_pure = prefix_is_pure(node%prefix_keywords)
            type is (subroutine_def_node)
            is_pure = prefix_is_pure(node%prefix_keywords)
        end select
    end function interface_body_is_pure

    function prefix_is_pure(prefix_keywords) result(is_pure)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical :: is_pure
        integer :: i

        is_pure = .false.
        if (.not. allocated(prefix_keywords)) return
        do i = 1, size(prefix_keywords)
            select case (to_lower(trim(prefix_keywords(i))))
            case ('pure', 'elemental')
                is_pure = .true.
            case ('impure')
                is_pure = .false.
                return
            end select
        end do
    end function prefix_is_pure

    subroutine report(errors, message, suggestion, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message, suggestion
        integer, intent(in) :: line, column

        call errors%add_error(message=message, code=ERROR_SEMANTIC, &
            component='semantic_pure_dummy_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), suggestion=suggestion, line=line, &
            column=column, end_line=line, end_column=column + 1)
    end subroutine report

end module semantic_pure_dummy_validation
