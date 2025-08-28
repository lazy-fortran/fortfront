module ast_factory_procedures
    use ast_core
    implicit none
    private

    ! Public procedure node creation functions
    public :: push_function_def, push_subroutine_def, push_interface_block
    public :: push_module, push_module_structured

contains

    ! Create function definition node and add to stack
    function push_function_def(arena, name, param_indices, return_type, body_indices, &
                               line, column, parent_index, result_variable) result(func_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in), optional :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: result_variable
        integer :: func_index
        type(function_def_node) :: func_def

        func_def = create_function_def(name, param_indices, return_type, &
                                       body_indices, line, column, result_variable)
        call arena%push(func_def, "function_def", parent_index)
        func_index = arena%size
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
        call arena%push(sub_def, "subroutine_def", parent_index)
        sub_index = arena%size
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

        interface_block = create_interface_block(interface_name, "interface", &
                          procedure_indices=procedure_indices, line=line, column=column)
        call arena%push(interface_block, "interface_block", parent_index)
        interface_index = arena%size
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

        mod_node = create_module(name, declaration_indices=body_indices, &
                                line=line, column=column)

        call arena%push(mod_node, "module_node", parent_index)
        module_index = arena%size
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

        mod_node = create_module(name, declaration_indices=declaration_indices, &
                                procedure_indices=procedure_indices, &
                                has_contains=has_contains, line=line, column=column)

        call arena%push(mod_node, "module_node", parent_index)
        module_index = arena%size
    end function push_module_structured

end module ast_factory_procedures