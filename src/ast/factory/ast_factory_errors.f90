module ast_factory_errors
    use ast_arena_modern, only: ast_arena_t
    use ast_error_nodes, only: error_node_t
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: push_error_node

contains

    function push_error_node(arena, message, original_data, line, column, &
            parent_index) result(error_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: original_data
        integer, intent(in), optional :: line, column, parent_index
        integer :: error_index
        type(error_node_t) :: err_node

        err_node%uid = generate_uid()
        err_node%error_message = message
        if (present(original_data)) then
            err_node%original_data = original_data
        else
            err_node%original_data = "unknown"
        end if
        err_node%error_code = -1

        if (present(line)) err_node%line = line
        if (present(column)) err_node%column = column

        call arena%push(err_node, "error_node", parent_index)
        error_index = arena%size
    end function push_error_node

end module ast_factory_errors
