module ast_factory_procedures
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use uid_generator, only: generate_uid
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   create_function_def, create_subroutine_def
    use ast_nodes_misc, only: interface_block_node, module_procedure_node, &
                              create_module_procedure
    use ast_nodes_data, only: module_node, block_data_node, submodule_node
    use ast_base, only: string_t
    implicit none
    private

    ! Public procedure node creation functions
    public :: push_function_def, push_subroutine_def, push_interface_block
    public :: push_module_procedure
    public :: push_module, push_module_structured, push_block_data
    public :: push_submodule_structured

contains

    ! Create function definition node and add to stack
    function push_function_def(arena, name, param_indices, return_type, body_indices, &
                               line, column, parent_index, result_variable, &
                               is_recursive, prefix_keywords, bind_c_clause) &
        result(func_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in), optional :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: result_variable
        logical, intent(in), optional :: is_recursive
        character(len=16), intent(in), optional :: prefix_keywords(:)
        character(len=*), intent(in), optional :: bind_c_clause
        integer :: func_index
        type(function_def_node) :: func_def

        func_def = create_function_def(name, param_indices, return_type, &
                                       body_indices, line, column, result_variable, &
                                       prefix_keywords)
        if (present(is_recursive)) then
            func_def%is_recursive = is_recursive
        end if
        if (present(bind_c_clause)) then
            func_def%bind_c_clause = bind_c_clause
        end if
        call arena%push(func_def, "function_def", parent_index)
        func_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                call link_children_to_parent(arena, func_index, param_indices)
            end if
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, func_index, body_indices)
            end if
        end if
    end function push_function_def

    ! Create subroutine definition node and add to stack
    function push_subroutine_def(arena, name, param_indices, body_indices, &
                                 line, column, parent_index, is_recursive, &
                                 prefix_keywords, bind_c_clause) result(sub_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        logical, intent(in), optional :: is_recursive
        character(len=16), intent(in), optional :: prefix_keywords(:)
        character(len=*), intent(in), optional :: bind_c_clause
        integer :: sub_index
        type(subroutine_def_node) :: sub_def

        if (present(prefix_keywords)) then
            if (present(is_recursive)) then
                sub_def = create_subroutine_def(name, param_indices, body_indices, &
                                                line, column, &
                                                prefix_keywords=prefix_keywords, &
                                                is_recursive=is_recursive)
            else
                sub_def = create_subroutine_def(name, param_indices, body_indices, &
                                                line, column, &
                                                prefix_keywords=prefix_keywords)
            end if
        else
            if (present(is_recursive)) then
                sub_def = create_subroutine_def(name, param_indices, body_indices, &
                                                line, column, &
                                                is_recursive=is_recursive)
            else
                sub_def = create_subroutine_def(name, param_indices, body_indices, &
                                                line, column)
            end if
        end if
        if (present(bind_c_clause)) then
            sub_def%bind_c_clause = bind_c_clause
        end if
        call arena%push(sub_def, "subroutine_def", parent_index)
        sub_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                call link_children_to_parent(arena, sub_index, param_indices)
            end if
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, sub_index, body_indices)
            end if
        end if
    end function push_subroutine_def

    ! Create interface block node and add to stack
    function push_interface_block(arena, interface_name, procedure_indices, &
                                  line, column, parent_index, is_abstract, &
                                  kind, operator_symbol) result(interface_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: interface_name
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        logical, intent(in), optional :: is_abstract
        character(len=*), intent(in), optional :: kind
        character(len=*), intent(in), optional :: operator_symbol
        integer :: interface_index
        type(interface_block_node) :: interface_block

        interface_block%uid = generate_uid()
        if (present(interface_name)) then
            if (len_trim(interface_name) > 0) interface_block%name = interface_name
        end if
        if (present(kind)) then
            interface_block%kind = kind
        else
            interface_block%kind = "interface"
        end if
        if (present(operator_symbol)) then
            if (len_trim(operator_symbol) > 0) interface_block%operator = &
                operator_symbol
        end if
        if (present(is_abstract)) interface_block%is_abstract = is_abstract
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) interface_block%procedure_indices = &
                procedure_indices
        end if
        interface_block%line = line
        interface_block%column = column
        call arena%push(interface_block, "interface_block", parent_index)
        interface_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, interface_index, procedure_indices)
            end if
        end if
    end function push_interface_block

    function push_module_procedure(arena, procedure_names, line, column, &
                                   parent_index, has_module_prefix, &
                                   has_double_colon) result(proc_index)
        type(ast_arena_t), intent(inout) :: arena
        type(string_t), intent(in), optional :: procedure_names(:)
        integer, intent(in), optional :: line, column, parent_index
        logical, intent(in), optional :: has_module_prefix
        logical, intent(in), optional :: has_double_colon
        integer :: proc_index
        type(module_procedure_node) :: module_proc

        module_proc = create_module_procedure(procedure_names, line, column, &
                                              has_module_prefix=has_module_prefix, &
                                              has_double_colon=has_double_colon)
        call arena%push(module_proc, "module_procedure", parent_index)
        proc_index = arena%size
    end function push_module_procedure

    ! Create module node and add to stack
    function push_module(arena, name, body_indices, line, column, &
                         parent_index) result(module_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: module_index
        type(module_node) :: mod_node

        mod_node%uid = generate_uid()
        mod_node%name = name
        if (present(body_indices)) then
            if (size(body_indices) > 0) mod_node%declaration_indices = body_indices
        end if
        mod_node%line = line
        mod_node%column = column

        call arena%push(mod_node, "module_node", parent_index)
        module_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, module_index, body_indices)
            end if
        end if
    end function push_module

    ! Create complete module node with declaration and procedure indices
    function push_module_structured(arena, name, declaration_indices, &
                                    procedure_indices, has_contains, line, column, &
                                    parent_index) result(module_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: declaration_indices(:), procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column, parent_index
        integer :: module_index
        type(module_node) :: mod_node

        mod_node%uid = generate_uid()
        mod_node%name = name
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                mod_node%declaration_indices = declaration_indices
            end if
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                mod_node%procedure_indices = procedure_indices
            end if
        end if
        if (present(has_contains)) mod_node%has_contains = has_contains
        mod_node%line = line
        mod_node%column = column

        call arena%push(mod_node, "module_node", parent_index)
        module_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                call link_children_to_parent(arena, module_index, declaration_indices)
            end if
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, module_index, procedure_indices)
            end if
        end if
    end function push_module_structured

    function push_block_data(arena, name, statement_indices, line, column, &
                             parent_index, header_label, end_label) &
        result(block_data_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: statement_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: header_label, end_label
        integer :: block_data_index
        type(block_data_node) :: bd_node

        bd_node%uid = generate_uid()
        bd_node%name = name
        if (present(header_label)) then
            if (len_trim(header_label) > 0) bd_node%header_label = header_label
        end if
        if (present(end_label)) then
            if (len_trim(end_label) > 0) bd_node%end_label = end_label
        end if
        if (present(statement_indices)) then
            if (size(statement_indices) > 0) then
                bd_node%statement_indices = statement_indices
            end if
        end if
        bd_node%line = line
        bd_node%column = column

        call arena%push(bd_node, "block_data_node", parent_index)
        block_data_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(statement_indices)) then
            if (size(statement_indices) > 0) then
                call link_children_to_parent(arena, block_data_index, statement_indices)
            end if
        end if
    end function push_block_data

    ! Create complete submodule node (Fortran 2008)
    function push_submodule_structured(arena, name, parent_identifier, &
                                       declaration_indices, procedure_indices, &
                                       has_contains, line, column, parent_index) &
        result(submod_idx)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: parent_identifier
        integer, intent(in), optional :: declaration_indices(:), procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column, parent_index
        integer :: submod_idx
        type(submodule_node) :: sub_node

        sub_node%uid = generate_uid()
        sub_node%name = name
        sub_node%parent_identifier = parent_identifier
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                sub_node%declaration_indices = declaration_indices
            end if
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                sub_node%procedure_indices = procedure_indices
            end if
        end if
        if (present(has_contains)) sub_node%has_contains = has_contains
        sub_node%line = line
        sub_node%column = column

        call arena%push(sub_node, "submodule_node", parent_index)
        submod_idx = arena%size

        ! Link children to this parent for AST traversal
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                call link_children_to_parent(arena, submod_idx, declaration_indices)
            end if
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, submod_idx, procedure_indices)
            end if
        end if
    end function push_submodule_structured

end module ast_factory_procedures
