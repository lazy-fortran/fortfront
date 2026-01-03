module ast_factory_generics
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use uid_generator, only: generate_uid
    use ast_nodes_generics, only: template_block_node, instantiate_statement_node, &
                                  trait_block_node, requirement_block_node, &
                                  implements_block_node, create_template_block, &
                                  create_instantiate_statement, create_trait_block, &
                                  create_requirement_block, create_implements_block
    implicit none
    private

    public :: push_template_block
    public :: push_instantiate_statement
    public :: push_trait_block
    public :: push_requirement_block
    public :: push_implements_block

contains

    function push_template_block(arena, name, parameter_names, declaration_indices, &
                                 procedure_indices, has_contains, line, column, &
                                 parent_index) result(template_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column, parent_index
        integer :: template_index
        type(template_block_node) :: node

        node = create_template_block(name, parameter_names, declaration_indices, &
                                     procedure_indices, has_contains, line, column)
        node%uid = generate_uid()

        call arena%push(node, "template_block_node", parent_index)
        template_index = arena%size

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                call link_children_to_parent(arena, template_index, declaration_indices)
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, template_index, procedure_indices)
            end if
        end if

        if (allocated(node%name)) deallocate (node%name)
        if (allocated(node%parameter_names)) deallocate (node%parameter_names)
        if (allocated(node%declaration_indices)) deallocate (node%declaration_indices)
        if (allocated(node%procedure_indices)) deallocate (node%procedure_indices)
        if (allocated(node%stmt_label)) deallocate (node%stmt_label)
    end function push_template_block

    function push_instantiate_statement(arena, template_name, spec_text, line, column, &
                                        parent_index) result(stmt_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: template_name
        character(len=*), intent(in) :: spec_text
        integer, intent(in), optional :: line, column, parent_index
        integer :: stmt_index
        type(instantiate_statement_node) :: node

        node = create_instantiate_statement(template_name, spec_text, line, column)
        node%uid = generate_uid()

        call arena%push(node, "instantiate_statement_node", parent_index)
        stmt_index = arena%size

        if (allocated(node%template_name)) deallocate (node%template_name)
        if (allocated(node%spec_text)) deallocate (node%spec_text)
        if (allocated(node%stmt_label)) deallocate (node%stmt_label)
    end function push_instantiate_statement

    function push_trait_block(arena, name, parameter_names, declaration_indices, &
                              procedure_indices, has_contains, line, column, &
                              parent_index) result(trait_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column, parent_index
        integer :: trait_index
        type(trait_block_node) :: node

        node = create_trait_block(name, parameter_names, declaration_indices, &
                                  procedure_indices, has_contains, line, column)
        node%uid = generate_uid()

        call arena%push(node, "trait_block_node", parent_index)
        trait_index = arena%size

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                call link_children_to_parent(arena, trait_index, declaration_indices)
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, trait_index, procedure_indices)
            end if
        end if

        if (allocated(node%name)) deallocate (node%name)
        if (allocated(node%parameter_names)) deallocate (node%parameter_names)
        if (allocated(node%declaration_indices)) deallocate (node%declaration_indices)
        if (allocated(node%procedure_indices)) deallocate (node%procedure_indices)
        if (allocated(node%stmt_label)) deallocate (node%stmt_label)
    end function push_trait_block

    function push_requirement_block(arena, name, parameter_names, declaration_indices, &
                                    procedure_indices, has_contains, line, column, &
                                    parent_index) result(requirement_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column, parent_index
        integer :: requirement_index
        type(requirement_block_node) :: node

        node = create_requirement_block(name, parameter_names, declaration_indices, &
                                        procedure_indices, has_contains, line, column)
        node%uid = generate_uid()

        call arena%push(node, "requirement_block_node", parent_index)
        requirement_index = arena%size

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                call link_children_to_parent(arena, requirement_index, &
                                             declaration_indices)
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, requirement_index, &
                                             procedure_indices)
            end if
        end if

        if (allocated(node%name)) deallocate (node%name)
        if (allocated(node%parameter_names)) deallocate (node%parameter_names)
        if (allocated(node%declaration_indices)) deallocate (node%declaration_indices)
        if (allocated(node%procedure_indices)) deallocate (node%procedure_indices)
        if (allocated(node%stmt_label)) deallocate (node%stmt_label)
    end function push_requirement_block

    function push_implements_block(arena, name, parameter_names, declaration_indices, &
                                   procedure_indices, has_contains, line, column, &
                                   parent_index) result(implements_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column, parent_index
        integer :: implements_index
        type(implements_block_node) :: node

        node = create_implements_block(name, parameter_names, declaration_indices, &
                                       procedure_indices, has_contains, line, column)
        node%uid = generate_uid()

        call arena%push(node, "implements_block_node", parent_index)
        implements_index = arena%size

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                call link_children_to_parent(arena, implements_index, &
                                             declaration_indices)
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                call link_children_to_parent(arena, implements_index, procedure_indices)
            end if
        end if

        if (allocated(node%name)) deallocate (node%name)
        if (allocated(node%parameter_names)) deallocate (node%parameter_names)
        if (allocated(node%declaration_indices)) deallocate (node%declaration_indices)
        if (allocated(node%procedure_indices)) deallocate (node%procedure_indices)
        if (allocated(node%stmt_label)) deallocate (node%stmt_label)
    end function push_implements_block

end module ast_factory_generics
