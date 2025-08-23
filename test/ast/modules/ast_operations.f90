module ast_operations
    ! Operations on AST nodes
    use ast_core
    use ast_factory
    implicit none

    private

    ! Re-export necessary functions
    public :: create_ast_arena, create_literal_node, create_program_node
    public :: get_node_type

    ! Node type constants
    integer, parameter, public :: NODE_PROGRAM = 1
    integer, parameter, public :: NODE_LITERAL = 2
    integer, parameter, public :: INT_TYPE = 1

contains


    function create_literal_node(arena, value, type_kind) result(node_id)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: value
        integer, intent(in) :: type_kind
        integer :: node_id

        node_id = push_literal(arena, value, 1, 1)
    end function create_literal_node

    function create_program_node(arena, name, body_indices) result(node_id)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer :: node_id

        node_id = push_program(arena, name, body_indices, 1, 1)
    end function create_program_node

    function get_node_type(arena, node_id) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_id
        integer :: node_type

        if (node_id > 0 .and. node_id <= arena%size) then
            if (arena%entries(node_id)%node_type == "program") then
                node_type = NODE_PROGRAM
            else if (arena%entries(node_id)%node_type == "literal") then
                node_type = NODE_LITERAL
            else
                node_type = 0
            end if
        else
            node_type = 0
        end if
    end function get_node_type

end module ast_operations
