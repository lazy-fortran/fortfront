module ast_factory_statements
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use uid_generator, only: generate_uid
    use ast_nodes_misc, only: use_statement_node, implicit_statement_node, &
                              include_statement_node, &
                              end_statement_node, allocate_statement_node, &
                              deallocate_statement_node
    use ast_nodes_control, only: stop_node, return_node, goto_node, error_stop_node, &
                                 cycle_node, exit_node
    use ast_nodes_io, only: io_implied_do_node
    implicit none
    private

    ! Public statement node creation functions
    public :: push_use_statement, push_implicit_statement, push_include_statement
    public :: push_end_statement
    public :: push_stop, push_return, push_goto, push_error_stop
    public :: push_cycle, push_exit
    public :: push_allocate, push_deallocate
    public :: push_io_implied_do

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
        integer :: i

        use_stmt%uid = generate_uid()
        use_stmt%module_name = module_name
        if (present(url_spec)) use_stmt%url_spec = url_spec
        if (present(has_only)) use_stmt%has_only = has_only
        if (present(only_list)) then
            if (size(only_list) > 0) then
                allocate (use_stmt%only_list(size(only_list)))
                do i = 1, size(only_list)
                    use_stmt%only_list(i) = string_t(only_list(i))
                end do
            end if
        end if
        if (present(rename_list)) then
            if (size(rename_list) > 0) then
                allocate (use_stmt%rename_list(size(rename_list)))
                do i = 1, size(rename_list)
                    use_stmt%rename_list(i) = string_t(rename_list(i))
                end do
            end if
        end if
        if (present(line)) use_stmt%line = line
        if (present(column)) use_stmt%column = column
        call arena%push(use_stmt, "use_statement", parent_index)
        use_index = arena%size
    end function push_use_statement

    ! Create IO implied-do node and add to arena
    function push_io_implied_do(arena, expr_index, var_name, start_expr_index, &
                                end_expr_index, step_expr_index, line, column, &
                                parent_index) result(node_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index
        integer, intent(in) :: end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: node_index
        type(io_implied_do_node) :: node

        node%uid = generate_uid()
        node%expr_index = expr_index
        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index
        if (present(line)) node%line = line
        if (present(column)) node%column = column

        call arena%push(node, "io_implied_do", parent_index)
        node_index = arena%size
    end function push_io_implied_do

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
        integer :: i, dash_pos

        implicit_stmt%uid = generate_uid()
        implicit_stmt%is_none = is_none
        if (present(type_name)) implicit_stmt%type_spec%type_name = type_name
        if (present(has_kind)) implicit_stmt%type_spec%has_kind = has_kind
        if (present(kind_value)) implicit_stmt%type_spec%kind_value = kind_value
        if (present(has_length)) implicit_stmt%type_spec%has_length = has_length
        if (present(length_value)) implicit_stmt%type_spec%length_value = length_value
        if (present(letter_ranges)) then
            if (size(letter_ranges) > 0) then
                allocate (implicit_stmt%letter_specs(size(letter_ranges)))
                do i = 1, size(letter_ranges)
                    dash_pos = index(letter_ranges(i), '-')
                    if (dash_pos > 0) then
                        implicit_stmt%letter_specs(i)%start_letter = &
                            letter_ranges(i) (1:1)
                        implicit_stmt%letter_specs(i)%end_letter = &
                            letter_ranges(i) (dash_pos + 1:dash_pos + 1)
                    else
                        implicit_stmt%letter_specs(i)%start_letter = &
                            letter_ranges(i) (1:1)
                        implicit_stmt%letter_specs(i)%end_letter = &
                            letter_ranges(i) (1:1)
                    end if
                end do
            end if
        end if
        if (present(line)) implicit_stmt%line = line
        if (present(column)) implicit_stmt%column = column
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

        include_stmt%uid = generate_uid()
        include_stmt%filename = filename
        if (present(line)) include_stmt%line = line
        if (present(column)) include_stmt%column = column
        call arena%push(include_stmt, "include_statement", parent_index)
        include_index = arena%size
    end function push_include_statement

    ! Create end statement node and add to stack
    function push_end_statement(arena, line, column, parent_index) result(end_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: end_index
        type(end_statement_node) :: end_stmt
        end_stmt%uid = generate_uid()
        if (present(line)) end_stmt%line = line
        if (present(column)) end_stmt%column = column
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
        stop_stmt%uid = generate_uid()
        if (present(stop_code_index)) stop_stmt%stop_code_index = stop_code_index
        if (present(stop_message)) stop_stmt%stop_message = stop_message
        if (present(line)) stop_stmt%line = line
        if (present(column)) stop_stmt%column = column

        call arena%push(stop_stmt, "stop_node", parent_index)
        stop_index = arena%size
    end function push_stop

    ! Create RETURN statement node and add to stack
    function push_return(arena, line, column, parent_index) result(return_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: return_index
        type(return_node) :: return_stmt
        return_stmt%uid = generate_uid()
        if (present(line)) return_stmt%line = line
        if (present(column)) return_stmt%column = column

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
        goto_stmt%uid = generate_uid()
        if (present(label)) goto_stmt%label = label
        if (present(line)) goto_stmt%line = line
        if (present(column)) goto_stmt%column = column

        call arena%push(goto_stmt, "goto_node", parent_index)
        goto_index = arena%size
    end function push_goto

    ! Create ERROR STOP statement node and add to stack
    function push_error_stop(arena, error_code_index, error_message, line, column, &
                             parent_index) result(error_stop_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: error_code_index
        character(len=*), intent(in), optional :: error_message
        integer, intent(in), optional :: line, column, parent_index
        integer :: error_stop_index
        type(error_stop_node) :: error_stop_stmt
        error_stop_stmt%uid = generate_uid()
        if (present(error_code_index)) error_stop_stmt%error_code_index = &
            error_code_index
        if (present(error_message)) error_stop_stmt%error_message = error_message
        if (present(line)) error_stop_stmt%line = line
        if (present(column)) error_stop_stmt%column = column

        call arena%push(error_stop_stmt, "error_stop_node", parent_index)
        error_stop_index = arena%size
    end function push_error_stop

    ! Create CYCLE statement node and add to stack
    function push_cycle(arena, loop_label, line, column, parent_index) &
        result(cycle_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: cycle_index
        type(cycle_node) :: cycle_stmt
        cycle_stmt%uid = generate_uid()
        if (present(loop_label)) cycle_stmt%label = loop_label
        if (present(line)) cycle_stmt%line = line
        if (present(column)) cycle_stmt%column = column

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
        exit_stmt%uid = generate_uid()
        if (present(loop_label)) exit_stmt%label = loop_label
        if (present(line)) exit_stmt%line = line
        if (present(column)) exit_stmt%column = column

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

        alloc_stmt%uid = generate_uid()

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

        dealloc_stmt%uid = generate_uid()

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
