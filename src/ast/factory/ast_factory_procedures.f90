module ast_factory_procedures
    use ast_arena_modern, only: ast_arena_t
    use uid_generator, only: generate_uid
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                    create_function_def, create_subroutine_def
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_data, only: module_node
    implicit none
    private

    ! Public procedure node creation functions
    public :: push_function_def, push_subroutine_def, push_interface_block
    public :: push_module, push_module_structured

contains

    ! Create function definition node and add to stack
    function push_function_def(arena, name, param_indices, return_type, body_indices, &
                              line, column, parent_index, result_variable, is_recursive, &
                                prefix_keywords) result(func_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in), optional :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: result_variable
        logical, intent(in), optional :: is_recursive
        character(len=16), intent(in), optional :: prefix_keywords(:)
        integer :: func_index
        type(function_def_node) :: func_def

        func_def = create_function_def(name, param_indices, return_type, &
                                        body_indices, line, column, result_variable, &
                                        prefix_keywords)
        if (present(is_recursive)) then
            func_def % is_recursive = is_recursive
        end if
        call arena % push(func_def, "function_def", parent_index)
        func_index = arena % size
    end function push_function_def

    ! Create subroutine definition node and add to stack
    function push_subroutine_def(arena, name, param_indices, body_indices, &
                                 line, column, parent_index) result(sub_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: sub_index
        type(subroutine_def_node) :: sub_def

        sub_def = create_subroutine_def(name, param_indices, body_indices, line, column)
        call arena % push(sub_def, "subroutine_def", parent_index)
        sub_index = arena % size
    end function push_subroutine_def

    ! Create interface block node and add to stack
    function push_interface_block(arena, interface_name, procedure_indices, &
                                  line, column, parent_index) result(interface_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: interface_name
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: interface_index
        type(interface_block_node) :: interface_block

        interface_block % uid = generate_uid()
        if (len_trim(interface_name) > 0) interface_block % name = interface_name
        interface_block % kind = "interface"
        if (present(procedure_indices)) then
  if (size(procedure_indices) > 0) interface_block % procedure_indices = procedure_indices
        end if
        interface_block % line = line
        interface_block % column = column
        call arena % push(interface_block, "interface_block", parent_index)
        interface_index = arena % size
    end function push_interface_block

    ! Create module node and add to stack
    function push_module(arena, name, body_indices, line, column, &
                         parent_index) result(module_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: module_index
        type(module_node) :: mod_node

        mod_node % uid = generate_uid()
        mod_node % name = name
        if (present(body_indices)) then
            if (size(body_indices) > 0) mod_node % declaration_indices = body_indices
        end if
        mod_node % line = line
        mod_node % column = column

        call arena % push(mod_node, "module_node", parent_index)
        module_index = arena % size
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

        mod_node % uid = generate_uid()
        mod_node % name = name
        if (present(declaration_indices)) then
    if (size(declaration_indices) > 0) mod_node % declaration_indices = declaration_indices
        end if
        if (present(procedure_indices)) then
         if (size(procedure_indices) > 0) mod_node % procedure_indices = procedure_indices
        end if
        if (present(has_contains)) mod_node % has_contains = has_contains
        mod_node % line = line
        mod_node % column = column

        call arena % push(mod_node, "module_node", parent_index)
        module_index = arena % size
    end function push_module_structured

end module ast_factory_procedures
