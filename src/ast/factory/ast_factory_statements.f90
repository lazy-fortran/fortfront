module ast_factory_statements
    use ast_core
    implicit none
    private

    ! Public statement node creation functions
    public :: push_use_statement, push_implicit_statement, push_include_statement
    public :: push_end_statement
    public :: push_stop, push_return, push_goto, push_error_stop
    public :: push_cycle, push_exit
    public :: push_allocate, push_deallocate

contains

    ! Create use statement node and add to stack
    function push_use_statement(arena, module_name, only_list, rename_list, &
                                has_only, line, column, parent_index, &
                                url_spec) result(use_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        character(len=*), intent(in), optional :: url_spec
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column, parent_index
        integer :: use_index
        type(use_statement_node) :: use_stmt

        use_stmt = create_use_statement(module_name, only_list, rename_list, &
                                        has_only, line, column, url_spec)
        call arena%push(use_stmt, "use_statement", parent_index)
        use_index = arena%size
    end function push_use_statement

    ! Create implicit statement node and add to stack
    function push_implicit_statement(arena, is_none, type_name, kind_value, has_kind, &
                                     length_value, has_length, letter_ranges, &
                                     line, column, parent_index) result(implicit_index)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: is_none
        character(len=*), intent(in), optional :: type_name
        integer, intent(in), optional :: kind_value
        logical, intent(in), optional :: has_kind
        integer, intent(in), optional :: length_value
        logical, intent(in), optional :: has_length
        character(len=*), intent(in), optional :: letter_ranges(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: implicit_index
        type(implicit_statement_node) :: implicit_stmt

        implicit_stmt = create_implicit_statement(is_none, type_name, kind_value, &
                                                  has_kind, length_value, has_length, &
                                                  letter_ranges, line, column)
        call arena%push(implicit_stmt, "implicit_statement", parent_index)
        implicit_index = arena%size
    end function push_implicit_statement

    ! Create include statement node and add to stack
    function push_include_statement(arena, filename, line, column, &
                                   parent_index) result(include_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column, parent_index
        integer :: include_index
        type(include_statement_node) :: include_stmt

        include_stmt = create_include_statement(filename, line, column)
        call arena%push(include_stmt, "include_statement", parent_index)
        include_index = arena%size
    end function push_include_statement

    ! Create end statement node and add to stack
    function push_end_statement(arena, line, column, parent_index) result(end_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: end_index
        type(end_statement_node) :: end_stmt

        end_stmt = create_end_statement(line, column)
        call arena%push(end_stmt, "end_statement", parent_index)
        end_index = arena%size
    end function push_end_statement

    ! Create STOP statement node and add to stack
    function push_stop(arena, stop_code_index, stop_message, line, column, &
                      parent_index) result(stop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: stop_code_index
        character(len=*), intent(in), optional :: stop_message
        integer, intent(in), optional :: line, column, parent_index
        integer :: stop_index
        type(stop_node) :: stop_stmt

        stop_stmt = create_stop(stop_code_index=stop_code_index, &
                                stop_message=stop_message, &
                                line=line, column=column)

        call arena%push(stop_stmt, "stop_node", parent_index)
        stop_index = arena%size
    end function push_stop

    ! Create RETURN statement node and add to stack
    function push_return(arena, line, column, parent_index) result(return_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: return_index
        type(return_node) :: return_stmt

        return_stmt = create_return(line=line, column=column)

        call arena%push(return_stmt, "return_node", parent_index)
        return_index = arena%size
    end function push_return

    ! Create GOTO statement node and add to stack
    function push_goto(arena, label, line, column, parent_index) result(goto_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: label
        integer, intent(in), optional :: line, column, parent_index
        integer :: goto_index
        type(goto_node) :: goto_stmt

        goto_stmt = create_goto(label=label, line=line, column=column)

        call arena%push(goto_stmt, "goto_node", parent_index)
        goto_index = arena%size
    end function push_goto

    ! Create ERROR STOP statement node and add to stack
    function push_error_stop(arena, error_code_index, error_message, line, column, parent_index) result(error_stop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: error_code_index
        character(len=*), intent(in), optional :: error_message
        integer, intent(in), optional :: line, column, parent_index
        integer :: error_stop_index
        type(error_stop_node) :: error_stop_stmt

        error_stop_stmt = create_error_stop(error_code_index=error_code_index, &
                                           error_message=error_message, &
                                           line=line, column=column)

        call arena%push(error_stop_stmt, "error_stop_node", parent_index)
        error_stop_index = arena%size
    end function push_error_stop

    ! Create CYCLE statement node and add to stack
    function push_cycle(arena, loop_label, line, column, parent_index) result(cycle_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: cycle_index
        type(cycle_node) :: cycle_stmt

        cycle_stmt = create_cycle(loop_label=loop_label, line=line, column=column)

        call arena%push(cycle_stmt, "cycle_node", parent_index)
        cycle_index = arena%size
    end function push_cycle

    ! Create EXIT statement node and add to stack
    function push_exit(arena, loop_label, line, column, parent_index) result(exit_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: exit_index
        type(exit_node) :: exit_stmt

        exit_stmt = create_exit(loop_label=loop_label, line=line, column=column)

        call arena%push(exit_stmt, "exit_node", parent_index)
        exit_index = arena%size
    end function push_exit

    ! Create allocate statement node and add to stack
    function push_allocate(arena, var_indices, shape_indices, stat_var_index, &
                           errmsg_var_index, source_expr_index, mold_expr_index, &
                           line, column, parent_index) result(alloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: var_indices(:)
        integer, intent(in), optional :: shape_indices(:)
        integer, intent(in), optional :: stat_var_index
        integer, intent(in), optional :: errmsg_var_index
        integer, intent(in), optional :: source_expr_index
        integer, intent(in), optional :: mold_expr_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: alloc_index
        type(allocate_statement_node) :: alloc_stmt

        if (size(var_indices) > 0) then
            alloc_stmt%var_indices = var_indices
        end if

        if (present(shape_indices)) then
            if (size(shape_indices) > 0) then
                alloc_stmt%shape_indices = shape_indices
            end if
        end if

        if (present(stat_var_index)) alloc_stmt%stat_var_index = stat_var_index
        if (present(errmsg_var_index)) alloc_stmt%errmsg_var_index = errmsg_var_index
        if (present(source_expr_index)) alloc_stmt%source_expr_index = source_expr_index
        if (present(mold_expr_index)) alloc_stmt%mold_expr_index = mold_expr_index
        if (present(line)) alloc_stmt%line = line
        if (present(column)) alloc_stmt%column = column

        call arena%push(alloc_stmt, "allocate_statement", parent_index)
        alloc_index = arena%size
    end function push_allocate

    ! Create deallocate statement node and add to stack
    function push_deallocate(arena, var_indices, stat_var_index, errmsg_var_index, &
                             line, column, parent_index) result(dealloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: var_indices(:)
        integer, intent(in), optional :: stat_var_index
        integer, intent(in), optional :: errmsg_var_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: dealloc_index
        type(deallocate_statement_node) :: dealloc_stmt

        if (size(var_indices) > 0) then
            dealloc_stmt%var_indices = var_indices
        end if

        if (present(stat_var_index)) dealloc_stmt%stat_var_index = stat_var_index
        if (present(errmsg_var_index)) dealloc_stmt%errmsg_var_index = errmsg_var_index
        if (present(line)) dealloc_stmt%line = line
        if (present(column)) dealloc_stmt%column = column

        call arena%push(dealloc_stmt, "deallocate_statement", parent_index)
        dealloc_index = arena%size
    end function push_deallocate

end module ast_factory_statements