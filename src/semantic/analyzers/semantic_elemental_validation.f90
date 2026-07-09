module semantic_elemental_validation
    ! Validates ELEMENTAL procedure dummy arguments per F2008 C1290.
    ! Every dummy argument of an ELEMENTAL procedure must be scalar; an array
    ! dummy is prohibited. An explicit IMPURE prefix relaxes purity, but the
    ! scalar-dummy requirement still applies to ELEMENTAL procedures.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_misc, only: interface_block_node, module_procedure_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    public :: validate_elemental_procedure, is_elemental_prefix

contains

    ! Returns .true. when the prefix list marks the procedure as ELEMENTAL.
    function is_elemental_prefix(prefix_keywords) result(is_elemental)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical :: is_elemental
        integer :: i

        is_elemental = .false.
        if (.not. allocated(prefix_keywords)) return

        do i = 1, size(prefix_keywords)
            if (trim(prefix_keywords(i)) == 'elemental') then
                is_elemental = .true.
                return
            end if
        end do
    end function is_elemental_prefix

    ! Validate the dummy arguments of an ELEMENTAL procedure. Non-elemental
    ! procedures are accepted unchanged.
    subroutine validate_elemental_procedure(arena, param_indices, body_indices, &
            prefix_keywords, errors, proc_name, bind_c_clause, result_name, &
            require_declared_dummy_intent)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in), optional :: proc_name
        character(len=:), allocatable, intent(in), optional :: bind_c_clause
        character(len=*), intent(in), optional :: result_name
        logical, intent(in), optional :: require_declared_dummy_intent
        integer :: i
        character(len=:), allocatable :: dummy_name
        logical :: check_dummy_intent

        if (.not. is_elemental_prefix(prefix_keywords)) return
        check_dummy_intent = .true.
        if (present(require_declared_dummy_intent)) then
            check_dummy_intent = require_declared_dummy_intent
        end if

        call validate_elemental_bind_c(bind_c_clause, errors)
        if (present(result_name)) then
            call validate_elemental_result(arena, body_indices, result_name, &
                errors)
        else if (present(proc_name)) then
            call validate_elemental_result(arena, body_indices, proc_name, &
                errors)
        end if

        if (.not. allocated(param_indices)) return

        do i = 1, size(param_indices)
            if (param_is_array(arena, param_indices(i))) then
                call report_array_dummy(errors, arena, param_indices(i))
                cycle
            end if
            dummy_name = param_name(arena, param_indices(i))
            if (len_trim(dummy_name) == 0) cycle
            if (.not. allocated(body_indices)) cycle
            if (body_declares_array(arena, body_indices, dummy_name)) then
                call report_array_dummy(errors, arena, param_indices(i))
            end if
            if (body_declares_dummy_procedure(arena, body_indices, dummy_name)) then
                call report_dummy_procedure(errors, arena, param_indices(i), &
                    dummy_name)
            end if
            if (check_dummy_intent) then
                if (.not. dummy_has_intent_or_value(arena, param_indices(i), &
                    body_indices, dummy_name)) then
                    call report_dummy_intent(errors, arena, param_indices(i), &
                        dummy_name)
                end if
            end if
        end do
    end subroutine validate_elemental_procedure

    ! Whether the parameter node itself carries an array specification. Standard
    ! Fortran params are identifier nodes; the array spec then lives in a body
    ! declaration. Inline declaration params carry is_array directly.
    function param_is_array(arena, param_index) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        logical :: is_array

        is_array = .false.
        if (.not. arena%has_node_at(param_index)) return

        select type (node => arena%entries(param_index)%node)
            type is (declaration_node)
            is_array = node%is_array
            type is (parameter_declaration_node)
            is_array = node%is_array
        end select
    end function param_is_array

    function param_has_intent(arena, param_index) result(has_intent)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        logical :: has_intent

        has_intent = .false.
        if (.not. arena%has_node_at(param_index)) return

        select type (node => arena%entries(param_index)%node)
            type is (declaration_node)
            has_intent = node%has_intent
            type is (parameter_declaration_node)
            has_intent = node%intent_type /= 0
        end select
    end function param_has_intent

    ! Extract the dummy argument name from a parameter node.
    function param_name(arena, param_index) result(name)
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
    end function param_name

    ! Whether a body declaration declares dummy_name as an array.
    function body_declares_array(arena, body_indices, dummy_name) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: dummy_name
        logical :: is_array
        integer :: i

        is_array = .false.
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (.not. node%is_array) cycle
                if (declaration_names_dummy(node, dummy_name)) then
                    is_array = .true.
                    return
                end if
            end select
        end do
    end function body_declares_array

    ! Whether a declaration node names dummy_name among its declared variables.
    function declaration_names_dummy(node, dummy_name) result(matches)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: dummy_name
        logical :: matches
        integer :: i
        character(len=:), allocatable :: lowered_dummy

        matches = .false.
        lowered_dummy = to_lower(trim(dummy_name))
        if (allocated(node%var_name)) then
            if (to_lower(trim(node%var_name)) == lowered_dummy) then
                matches = .true.
                return
            end if
        end if
        if (allocated(node%var_names)) then
            do i = 1, size(node%var_names)
                if (to_lower(trim(node%var_names(i))) == lowered_dummy) then
                    matches = .true.
                    return
                end if
            end do
        end if
    end function declaration_names_dummy

    function dummy_has_intent_or_value(arena, param_index, body_indices, &
            dummy_name) result(has_attr)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: dummy_name
        logical :: has_attr
        integer :: decl_index

        has_attr = param_has_intent(arena, param_index)
        if (has_attr) return

        decl_index = find_body_declaration(arena, body_indices, dummy_name)
        if (decl_index <= 0) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            has_attr = node%has_intent
            if (.not. has_attr) has_attr = node%is_value
        end select
    end function dummy_has_intent_or_value

    function find_body_declaration(arena, body_indices, name) result(decl_index)
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
                if (declaration_names_dummy(node, name)) then
                    decl_index = body_indices(i)
                    return
                end if
            end select
        end do
    end function find_body_declaration

    function body_declares_dummy_procedure(arena, body_indices, dummy_name) &
            result(is_procedure)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: dummy_name
        logical :: is_procedure
        integer :: i

        is_procedure = .false.
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            if (node_declares_dummy_procedure(arena, body_indices(i), &
                dummy_name)) then
                is_procedure = .true.
                return
            end if
        end do
    end function body_declares_dummy_procedure

    function node_declares_dummy_procedure(arena, node_index, dummy_name) &
            result(is_procedure)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: dummy_name
        logical :: is_procedure

        is_procedure = .false.
        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            if (declaration_names_dummy(node, dummy_name)) then
                is_procedure = declaration_is_procedure(node)
            end if
            type is (interface_block_node)
            is_procedure = interface_declares_name(arena, node, dummy_name)
        end select
    end function node_declares_dummy_procedure

    function declaration_is_procedure(node) result(is_procedure)
        type(declaration_node), intent(in) :: node
        logical :: is_procedure
        character(len=:), allocatable :: lowered

        is_procedure = .false.
        if (.not. allocated(node%type_name)) return
        lowered = to_lower(trim(node%type_name))
        if (len(lowered) < 9) return
        is_procedure = lowered(1:9) == 'procedure'
    end function declaration_is_procedure

    function interface_declares_name(arena, node, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: node
        character(len=*), intent(in) :: name
        logical :: found
        integer :: i

        found = .false.
        if (.not. allocated(node%procedure_indices)) return
        do i = 1, size(node%procedure_indices)
            if (.not. arena%has_node_at(node%procedure_indices(i))) cycle
            if (procedure_node_name_matches(arena, node%procedure_indices(i), &
                name)) then
                found = .true.
                return
            end if
        end do
    end function interface_declares_name

    function procedure_node_name_matches(arena, node_index, name) result(matches)
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name
        logical :: matches

        matches = .false.
        select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
            if (allocated(node%name)) matches = names_match(node%name, name)
            type is (subroutine_def_node)
            if (allocated(node%name)) matches = names_match(node%name, name)
            type is (module_procedure_node)
            matches = module_procedure_names_match(node, name)
        end select
    end function procedure_node_name_matches

    function module_procedure_names_match(node, name) result(matches)
        type(module_procedure_node), intent(in) :: node
        character(len=*), intent(in) :: name
        logical :: matches
        integer :: i

        matches = .false.
        if (.not. allocated(node%procedure_names)) return
        do i = 1, size(node%procedure_names)
            if (names_match(node%procedure_names(i)%s, name)) then
                matches = .true.
                return
            end if
        end do
    end function module_procedure_names_match

    function names_match(left, right) result(matches)
        character(len=*), intent(in) :: left, right
        logical :: matches

        matches = to_lower(trim(left)) == to_lower(trim(right))
    end function names_match

    subroutine validate_elemental_bind_c(bind_c_clause, errors)
        character(len=:), allocatable, intent(in), optional :: bind_c_clause
        type(error_collection_t), intent(inout) :: errors

        if (.not. present(bind_c_clause)) return
        if (.not. allocated(bind_c_clause)) return
        if (len_trim(bind_c_clause) == 0) return
        call errors%add_error( &
            message='BIND(C) attribute conflicts with ELEMENTAL attribute', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            suggestion='drop BIND(C) or drop the ELEMENTAL prefix')
    end subroutine validate_elemental_bind_c

    subroutine validate_elemental_result(arena, body_indices, result_name, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: result_name
        type(error_collection_t), intent(inout) :: errors
        integer :: decl_index

        if (len_trim(result_name) == 0) return
        if (.not. allocated(body_indices)) return
        decl_index = find_body_declaration(arena, body_indices, result_name)
        if (decl_index <= 0) return

        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (node%is_array) call report_array_result(errors, node)
            if (node%is_pointer) call report_pointer_result(errors, node)
        end select
    end subroutine validate_elemental_result

    subroutine report_array_dummy(errors, arena, param_index)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        integer :: line, column

        line = 0
        column = 0
        if (arena%has_node_at(param_index)) then
            line = arena%entries(param_index)%node%line
            column = arena%entries(param_index)%node%column
        end if

        call errors%add_error( &
            message='array dummy argument is not allowed in an ELEMENTAL '// &
            'procedure', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='make the dummy argument scalar, or drop the '// &
            'ELEMENTAL prefix')
    end subroutine report_array_dummy

    subroutine report_dummy_procedure(errors, arena, param_index, dummy_name)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=*), intent(in) :: dummy_name
        integer :: line, column

        call source_position(arena, param_index, line, column)
        call errors%add_error( &
            message='procedure dummy argument "'//trim(dummy_name)// &
            '" is not allowed in an ELEMENTAL procedure', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='drop the ELEMENTAL prefix or remove the dummy procedure')
    end subroutine report_dummy_procedure

    subroutine report_dummy_intent(errors, arena, param_index, dummy_name)
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=*), intent(in) :: dummy_name
        integer :: line, column

        call source_position(arena, param_index, line, column)
        call errors%add_error( &
            message='dummy argument "'//trim(dummy_name)// &
            '" of ELEMENTAL procedure must have INTENT or VALUE', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='add INTENT or VALUE, or drop the ELEMENTAL prefix')
    end subroutine report_dummy_intent

    subroutine report_array_result(errors, node)
        type(error_collection_t), intent(inout) :: errors
        type(declaration_node), intent(in) :: node

        call errors%add_error( &
            message='array result is not allowed in an ELEMENTAL function', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            context='line '//int_to_string(node%line)//', column '// &
            int_to_string(node%column), &
            suggestion='make the result scalar, or drop the ELEMENTAL prefix')
    end subroutine report_array_result

    subroutine report_pointer_result(errors, node)
        type(error_collection_t), intent(inout) :: errors
        type(declaration_node), intent(in) :: node

        call errors%add_error( &
            message='pointer result is not allowed in an ELEMENTAL function', &
            code=ERROR_SEMANTIC, &
            component='semantic_elemental_validation', &
            context='line '//int_to_string(node%line)//', column '// &
            int_to_string(node%column), &
            suggestion='remove POINTER, or drop the ELEMENTAL prefix')
    end subroutine report_pointer_result

    subroutine source_position(arena, node_index, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: line, column

        line = 0
        column = 0
        if (arena%has_node_at(node_index)) then
            line = arena%entries(node_index)%node%line
            column = arena%entries(node_index)%node%column
        end if
    end subroutine source_position

end module semantic_elemental_validation
