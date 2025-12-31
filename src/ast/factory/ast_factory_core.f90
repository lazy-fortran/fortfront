module ast_factory_core
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_nodes_core, only: program_node, assignment_node, pointer_assignment_node, &
                              identifier_node, literal_node, binary_op_node, &
                              call_or_subscript_node, array_literal_node, &
                              component_access_node, range_subscript_node, &
                              create_array_literal, create_component_access, &
                              create_range_subscript, &
                              create_pointer_assignment
    use ast_nodes_misc, only: complex_literal_node
    use uid_generator, only: generate_uid
    use ast_base, only: LITERAL_STRING
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    use error_handling, only: result_t, success_result, create_error_result, &
                              critical_result, &
                              ERROR_VALIDATION, ERROR_MEMORY, ERROR_INTERNAL
    implicit none
    private

    ! Public validation utilities
    public :: validate_arena, validate_node_index

    ! Public core node creation functions
    public :: push_program, push_identifier, push_literal, push_binary_op
    public :: push_assignment, push_pointer_assignment
    public :: push_array_literal, push_complex_literal, push_component_access
    public :: push_range_subscript, push_type_constructor

contains

    ! Arena validation utility
    function validate_arena(arena, context) result(validation_result)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: context
        type(result_t) :: validation_result

        if (.not. allocated(arena%entries)) then
            validation_result = critical_result( &
                                "Arena entries not allocated", &
                                ERROR_MEMORY, &
                                component="ast_factory_core", &
                                context=context, &
                      suggestion="Initialize arena with create_ast_arena() before use" &
                                )
            return
        end if

        if (arena%size < 0 .or. arena%size > arena%capacity) then
            validation_result = critical_result( &
                                "Arena size inconsistent with capacity", &
                                ERROR_INTERNAL, &
                                component="ast_factory_core", &
                                context=context, &
                  suggestion="Check for memory corruption or incorrect initialization" &
                                )
            return
        end if

        validation_result = success_result()
    end function validate_arena

    ! Node index validation utility
    function validate_node_index(arena, node_index, context, allow_zero) &
        result(validation_result)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: context
        logical, intent(in), optional :: allow_zero
        type(result_t) :: validation_result
        logical :: zero_ok

        zero_ok = .false.
        if (present(allow_zero)) zero_ok = allow_zero

        if (node_index == 0 .and. zero_ok) then
            validation_result = success_result()
            return
        end if

        if (node_index <= 0) then
            validation_result = create_error_result( &
                                "Invalid node index: must be positive", &
                                ERROR_VALIDATION, &
                                component="ast_factory_core", &
                                context=context, &
                      suggestion="Ensure node index comes from valid push_* operation" &
                                )
            return
        end if

        if (node_index > arena%size) then
            validation_result = create_error_result( &
                                "Node index out of bounds", &
                                ERROR_VALIDATION, &
                                component="ast_factory_core", &
                                context=context, &
                     suggestion="Check that referenced node was created in this arena" &
                                )
            return
        end if

        if (.not. allocated(arena%entries(node_index)%node)) then
            validation_result = create_error_result( &
                                "Node index references unallocated node", &
                                ERROR_VALIDATION, &
                                component="ast_factory_core", &
                                context=context, &
                suggestion="Ensure referenced node is still valid and not deallocated" &
                                )
            return
        end if

        validation_result = success_result()
    end function validate_node_index

    ! Create program node and add to stack
    function push_program(arena, name, body_indices, line, column) result(prog_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        integer :: prog_index
        type(program_node) :: prog

        prog%uid = generate_uid()
        prog%name = name
        if (size(body_indices) > 0) prog%body_indices = body_indices
        if (present(line)) prog%line = line
        if (present(column)) prog%column = column
        call arena%push(prog, "program", 0)
        prog_index = arena%size

        ! Link children to this parent for AST traversal
        if (size(body_indices) > 0) then
            call link_children_to_parent(arena, prog_index, body_indices)
        end if
    end function push_program

    ! Create assignment node and add to stack
    function push_assignment(arena, target_index, value_index, line, column, &
                             parent_index, operator_text, suppress_codegen) &
        result(assign_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: operator_text
        logical, intent(in), optional :: suppress_codegen
        integer :: assign_index
        type(assignment_node) :: assign

        assign%uid = generate_uid()
        assign%target_index = target_index
        assign%value_index = value_index
        if (present(operator_text)) then
            assign%operator = operator_text
        else
            assign%operator = "="
        end if
        if (present(suppress_codegen)) then
            assign%suppress_codegen = suppress_codegen
        end if
        if (present(line)) assign%line = line
        if (present(column)) assign%column = column
        call arena%push(assign, "assignment", parent_index)
        assign_index = arena%size

        ! Link children to this parent for AST traversal
        if (target_index > 0) then
            call link_children_to_parent(arena, assign_index, [target_index])
        end if
        if (value_index > 0) then
            call link_children_to_parent(arena, assign_index, [value_index])
        end if
    end function push_assignment

    ! Create pointer assignment node and add to stack
    function push_pointer_assignment(arena, pointer_index, target_index, &
                                     line, column, parent_index) &
        result(assign_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: pointer_index
        integer, intent(in) :: target_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: assign_index
        type(pointer_assignment_node) :: ptr_assign

        ptr_assign = create_pointer_assignment(pointer_index, target_index, &
                                               line, column)
        call arena%push(ptr_assign, "pointer_assignment", parent_index)
        assign_index = arena%size

        ! Link children to this parent for AST traversal
        if (pointer_index > 0) then
            call link_children_to_parent(arena, assign_index, [pointer_index])
        end if
        if (target_index > 0) then
            call link_children_to_parent(arena, assign_index, [target_index])
        end if
    end function push_pointer_assignment

    ! Create binary operation node and add to stack
    function push_binary_op(arena, left_index, right_index, operator, &
                            line, column, parent_index) result(binop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column, parent_index
        integer :: binop_index
        type(binary_op_node) :: binop

        binop%uid = generate_uid()
        binop%left_index = left_index
        binop%right_index = right_index
        binop%operator = operator
        if (present(line)) binop%line = line
        if (present(column)) binop%column = column
        call arena%push(binop, "binary_op", parent_index)
        binop_index = arena%size

        ! Link children to this parent for AST traversal
        if (left_index > 0) then
            call link_children_to_parent(arena, binop_index, [left_index])
        end if
        if (right_index > 0) then
            call link_children_to_parent(arena, binop_index, [right_index])
        end if
    end function push_binary_op

    ! Create identifier node and add to stack
    function push_identifier(arena, name, line, column, parent_index) result(id_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column, parent_index
        integer :: id_index
        type(identifier_node) :: id

        id%uid = generate_uid()
        id%name = name
        if (present(line)) id%line = line
        if (present(column)) id%column = column
        call arena%push(id, "identifier", parent_index)
        id_index = arena%size
    end function push_identifier

    ! Create literal node and add to stack
    function push_literal(arena, value, kind, line, column, parent_index) &
        result(lit_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column, parent_index
        integer :: lit_index
        type(literal_node) :: lit

        lit%uid = generate_uid()
        lit%value = value
        lit%literal_kind = kind
        if (present(line)) lit%line = line
        if (present(column)) lit%column = column
        call arena%push(lit, "literal", parent_index)
        lit_index = arena%size
    end function push_literal

    ! Create array literal node and add to stack
    function push_array_literal(arena, element_indices, line, column, &
                                parent_index, syntax_style, type_spec) &
        result(array_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: element_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: syntax_style
        character(len=*), intent(in), optional :: type_spec
        integer :: array_index
        type(array_literal_node) :: array_lit

        if (present(type_spec)) then
            array_lit = create_array_literal(element_indices, line, column, &
                                             syntax_style, type_spec)
        else
            array_lit = create_array_literal(element_indices, line, column, &
                                             syntax_style)
        end if
        call arena%push(array_lit, "array_literal", parent_index)
        array_index = arena%size

        ! Link children to this parent for AST traversal
        if (size(element_indices) > 0) then
            call link_children_to_parent(arena, array_index, element_indices)
        end if
    end function push_array_literal

    ! Create complex literal node and add to stack
    function push_complex_literal(arena, real_index, imag_index, line, column, &
                                  parent_index) result(complex_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: real_index, imag_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: complex_index
        type(complex_literal_node) :: complex_node

        complex_node%real_index = real_index
        complex_node%imag_index = imag_index

        if (present(line)) complex_node%line = line
        if (present(column)) complex_node%column = column

        call arena%push(complex_node, "complex_literal", parent_index)
        complex_index = arena%size

        ! Link children to this parent for AST traversal
        if (real_index > 0) then
            call link_children_to_parent(arena, complex_index, [real_index])
        end if
        if (imag_index > 0) then
            call link_children_to_parent(arena, complex_index, [imag_index])
        end if
    end function push_complex_literal

    ! Create component access node and add to stack
    function push_component_access(arena, object_index, component_name, &
                                   line, column, parent_index) result(access_index)
        use ast_nodes_core, only: component_access_node, create_component_access
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: object_index
        character(len=*), intent(in) :: component_name
        integer, intent(in), optional :: line, column, parent_index
        integer :: access_index
        type(component_access_node) :: access_node

        ! Validate inputs
        if (object_index <= 0 .or. object_index > arena%size) then
            access_index = 0
            return
        end if

        if (len_trim(component_name) == 0) then
            access_index = 0
            return
        end if

        ! Create proper component access node
        access_node = create_component_access(object_index, component_name, &
                                              line, column)

        call arena%push(access_node, "component_access", parent_index)
        access_index = arena%size

        ! Link children to this parent for AST traversal
        if (object_index > 0) then
            call link_children_to_parent(arena, access_index, [object_index])
        end if
    end function push_component_access

    ! Create range subscript node (for array slices or character substrings)
    function push_range_subscript(arena, base_expr_index, start_index, end_index, &
                                  line, column, parent_index) result(subscript_index)
        use ast_nodes_core, only: range_subscript_node, create_range_subscript
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr_index
        integer, intent(in), optional :: start_index, end_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: subscript_index
        type(range_subscript_node) :: subscript_node

        ! Validate base expression index
        if (base_expr_index <= 0 .or. base_expr_index > arena%size) then
            subscript_index = 0
            return
        end if

        ! Create range subscript node
        subscript_node = create_range_subscript(base_expr_index, start_index, &
                                                end_index, &
                                                line, column)

        call arena%push(subscript_node, "range_subscript", parent_index)
        subscript_index = arena%size

        ! Link children to this parent for AST traversal
        if (base_expr_index > 0) then
            call link_children_to_parent(arena, subscript_index, [base_expr_index])
        end if
        if (present(start_index)) then
            if (start_index > 0) then
                call link_children_to_parent(arena, subscript_index, [start_index])
            end if
        end if
        if (present(end_index)) then
            if (end_index > 0) then
                call link_children_to_parent(arena, subscript_index, [end_index])
            end if
        end if
    end function push_range_subscript

    ! Create type constructor node and add to stack
    function push_type_constructor(arena, type_name, arg_indices, line, &
                                   column, parent_index) result(constructor_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        integer, intent(in), optional :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: constructor_index
        type(call_or_subscript_node) :: constructor_node

        ! Type constructors are treated as special function calls
        constructor_node%uid = generate_uid()
        constructor_node%name = type_name
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                constructor_node%arg_indices = arg_indices
            end if
        end if

        if (present(line)) constructor_node%line = line
        if (present(column)) constructor_node%column = column

        call arena%push(constructor_node, "type_constructor", parent_index)
        constructor_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                call link_children_to_parent(arena, constructor_index, arg_indices)
            end if
        end if
    end function push_type_constructor

end module ast_factory_core
