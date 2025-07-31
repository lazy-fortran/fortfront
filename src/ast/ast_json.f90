module ast_json
    ! This module provides JSON serialization for AST nodes
    ! Uses the built-in to_json methods on the AST nodes
    use ast_core
    use json_module
    implicit none
    
    public :: ast_to_json_string

contains

    function ast_to_json_string(arena, node_index) result(json_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: json_str
        type(json_core) :: json
        type(json_value), pointer :: root
        
        call json%initialize()
        
        if (node_index == 0 .or. node_index > arena%size) then
            call json%create_null(root, '')
        else
            ! Create a root object (not array) to hold the AST node
            call json%create_object(root, '')
            ! Use the node's built-in to_json method
            ! The node should add its properties to the root object
            call arena%entries(node_index)%node%to_json(json, root)
        end if
        
        call json%print_to_string(root, json_str)
        call json%destroy(root)
    end function ast_to_json_string

end module ast_json