module ast_factory_statements
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use uid_generator, only: generate_uid
    use ast_nodes_misc, only: use_statement_node, implicit_statement_node, &
                              intrinsic_statement_node, visibility_statement_node, &
                              namelist_statement_node, data_statement_node, &
                              include_statement_node, import_statement_node, &
                              end_statement_node, allocate_statement_node, &
                              deallocate_statement_node, create_implicit_statement
    use ast_nodes_control, only: stop_node, return_node, entry_node, goto_node, &
                                 error_stop_node, cycle_node, exit_node, &
                                 continue_node, pause_node, nullify_node
    use ast_nodes_io, only: io_implied_do_node
    use ast_nodes_legacy, only: common_block_node, enum_node
    implicit none
    private

    ! Public statement node creation functions
    public :: push_use_statement, push_intrinsic_statement, &
              push_visibility_statement, push_namelist_statement, &
              push_data_statement, push_implicit_statement, &
              push_include_statement, push_import_statement
    public :: push_end_statement
    public :: push_stop, push_return, push_entry, push_continue, push_goto, &
              push_error_stop
    public :: push_cycle, push_exit, push_pause, push_nullify
    public :: push_allocate, push_deallocate
    public :: push_io_implied_do
    public :: push_common_block, push_enum

contains

    ! Create use statement node and add to stack
    function push_use_statement(arena, module_name, only_list, rename_list, &
                                has_only, line, column, parent_index, &
                                url_spec, has_double_colon, is_intrinsic, &
                                is_non_intrinsic) result(use_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        character(len=*), intent(in), optional :: url_spec
        logical, intent(in), optional :: has_only
        logical, intent(in), optional :: has_double_colon, is_intrinsic, &
                                         is_non_intrinsic
        integer, intent(in), optional :: line, column, parent_index
        integer :: use_index
        type(use_statement_node) :: use_stmt
        integer :: i

        use_stmt%uid = generate_uid()
        use_stmt%module_name = module_name
        if (present(url_spec)) use_stmt%url_spec = url_spec
        if (present(has_only)) use_stmt%has_only = has_only
        if (present(has_double_colon)) use_stmt%has_double_colon = has_double_colon
        if (present(is_intrinsic)) use_stmt%is_intrinsic = is_intrinsic
        if (present(is_non_intrinsic)) use_stmt%is_non_intrinsic = is_non_intrinsic
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

    function push_intrinsic_statement(arena, procedure_names, line, column, &
                                      parent_index, has_double_colon) &
        result(stmt_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: procedure_names(:)
        integer, intent(in), optional :: line, column, parent_index
        logical, intent(in), optional :: has_double_colon
        integer :: stmt_index
        type(intrinsic_statement_node) :: node
        integer :: i

        node%uid = generate_uid()
        if (present(has_double_colon)) node%has_double_colon = has_double_colon

        if (size(procedure_names) > 0) then
            allocate (node%procedure_names(size(procedure_names)))
            do i = 1, size(procedure_names)
                node%procedure_names(i) = string_t(trim(procedure_names(i)))
            end do
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column

        call arena%push(node, "intrinsic_statement", parent_index)
        stmt_index = arena%size
    end function push_intrinsic_statement

    function push_visibility_statement(arena, is_private, names, line, column, &
                                       parent_index, has_double_colon) &
        result(vis_index)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: is_private
        character(len=*), intent(in), optional :: names(:)
        integer, intent(in), optional :: line, column, parent_index
        logical, intent(in), optional :: has_double_colon
        integer :: vis_index
        type(visibility_statement_node) :: vis_stmt
        integer :: i

        vis_stmt%uid = generate_uid()
        vis_stmt%is_private = is_private
        if (present(has_double_colon)) vis_stmt%has_double_colon = has_double_colon

        if (present(names)) then
            if (size(names) > 0) then
                vis_stmt%has_list = .true.
                allocate (vis_stmt%names(size(names)))
                do i = 1, size(names)
                    vis_stmt%names(i) = string_t(names(i))
                end do
            end if
        end if

        if (present(line)) vis_stmt%line = line
        if (present(column)) vis_stmt%column = column
        call arena%push(vis_stmt, "visibility_statement", parent_index)
        vis_index = arena%size
    end function push_visibility_statement

    function push_namelist_statement(arena, group_name, variable_names, line, column, &
                                     parent_index) result(namelist_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: group_name
        character(len=*), intent(in), optional :: variable_names(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: namelist_index
        type(namelist_statement_node) :: node
        integer :: i

        node%uid = generate_uid()
        node%group_name = trim(group_name)

        if (present(variable_names)) then
            if (size(variable_names) > 0) then
                allocate (node%variable_names(size(variable_names)))
                do i = 1, size(variable_names)
                    node%variable_names(i) = string_t(trim(variable_names(i)))
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
        call arena%push(node, "namelist_statement", parent_index)
        namelist_index = arena%size
    end function push_namelist_statement

    function push_data_statement(arena, object_indices, value_indices, line, column, &
                                 parent_index) result(data_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: object_indices(:)
        integer, intent(in) :: value_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: data_index
        type(data_statement_node) :: node

        node%uid = generate_uid()

        if (size(object_indices) > 0) then
            node%object_indices = object_indices
        end if

        if (size(value_indices) > 0) then
            node%value_indices = value_indices
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
        call arena%push(node, "data_statement", parent_index)
        data_index = arena%size
    end function push_data_statement

    function push_common_block(arena, block_names, member_names, member_block, &
                               line, column, parent_index) result(common_index)
        type(ast_arena_t), intent(inout) :: arena
        type(string_t), intent(in) :: block_names(:)
        type(string_t), intent(in) :: member_names(:)
        integer, intent(in) :: member_block(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: common_index
        type(common_block_node) :: node

        node%uid = generate_uid()
        node%block_names = block_names
        node%member_names = member_names
        node%member_block = member_block
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        call arena%push(node, "common_block", parent_index)
        common_index = arena%size
    end function push_common_block

    function push_enum(arena, enumerator_names, enumerator_values, is_bind_c, &
                       line, column, parent_index) result(enum_index)
        type(ast_arena_t), intent(inout) :: arena
        type(string_t), intent(in) :: enumerator_names(:)
        integer, intent(in) :: enumerator_values(:)
        logical, intent(in) :: is_bind_c
        integer, intent(in), optional :: line, column, parent_index
        integer :: enum_index
        type(enum_node) :: node

        node%uid = generate_uid()
        node%enumerator_names = enumerator_names
        node%enumerator_values = enumerator_values
        node%is_bind_c = is_bind_c
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        call arena%push(node, "enum", parent_index)
        enum_index = arena%size
    end function push_enum

    ! Create IO implied-do node and add to arena
    function push_io_implied_do(arena, expr_index, var_name, start_expr_index, &
                                end_expr_index, step_expr_index, line, column, &
                                parent_index, object_indices) result(node_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index
        integer, intent(in) :: end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: line, column, parent_index
        integer, intent(in), optional :: object_indices(:)
        integer :: node_index
        type(io_implied_do_node) :: node

        node%uid = generate_uid()
        node%expr_index = expr_index
        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index
        if (present(object_indices)) then
            if (size(object_indices) > 0) then
                node%object_indices = object_indices
                if (node%expr_index <= 0) node%expr_index = object_indices(1)
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column

        call arena%push(node, "io_implied_do", parent_index)
        node_index = arena%size
    end function push_io_implied_do

    ! Create implicit statement node and add to stack
    function push_implicit_statement(arena, is_none, type_name, kind_value, has_kind, &
                                     length_value, has_length, letter_ranges, &
                                     line, column, parent_index, none_spec) &
        result(implicit_index)
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: is_none
        character(len=*), intent(in), optional :: type_name
        integer, intent(in), optional :: kind_value
        logical, intent(in), optional :: has_kind
        integer, intent(in), optional :: length_value
        logical, intent(in), optional :: has_length
        character(len=*), intent(in), optional :: letter_ranges(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: none_spec
        integer :: implicit_index
        type(implicit_statement_node) :: implicit_stmt

        implicit_stmt = create_implicit_statement(is_none, type_name, kind_value, &
                                                  has_kind, length_value, &
                                                  has_length, letter_ranges, &
                                                  line, column, none_spec)
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

    function push_import_statement(arena, import_list, has_double_colon, is_all, &
                                   is_none, line, column, parent_index) &
        result(import_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: import_list(:)
        logical, intent(in), optional :: has_double_colon, is_all, is_none
        integer, intent(in), optional :: line, column, parent_index
        integer :: import_index
        type(import_statement_node) :: import_stmt
        integer :: i

        import_stmt%uid = generate_uid()
        if (present(has_double_colon)) import_stmt%has_double_colon = has_double_colon
        if (present(is_all)) import_stmt%is_all = is_all
        if (present(is_none)) import_stmt%is_none = is_none

        if (present(import_list)) then
            if (size(import_list) > 0) then
                import_stmt%has_list = .true.
                allocate (import_stmt%import_list(size(import_list)))
                do i = 1, size(import_list)
                    import_stmt%import_list(i)%s = import_list(i)
                end do
            end if
        end if

        if (present(line)) import_stmt%line = line
        if (present(column)) import_stmt%column = column
        call arena%push(import_stmt, "import_statement", parent_index)
        import_index = arena%size
    end function push_import_statement

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

    ! Create ENTRY statement node and add to stack
    function push_entry(arena, name, params_text, param_indices, line, column, &
                        parent_index) result(entry_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: params_text
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: entry_index
        type(entry_node) :: entry_stmt
        entry_stmt%uid = generate_uid()
        entry_stmt%name = name
        if (present(params_text)) entry_stmt%params_text = params_text
        if (present(param_indices)) entry_stmt%param_indices = param_indices
        if (present(line)) entry_stmt%line = line
        if (present(column)) entry_stmt%column = column

        call arena%push(entry_stmt, "entry_node", parent_index)
        entry_index = arena%size
    end function push_entry

    function push_continue(arena, line, column, parent_index) result(continue_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: line, column, parent_index
        integer :: continue_index
        type(continue_node) :: continue_stmt

        continue_stmt%uid = generate_uid()
        if (present(line)) continue_stmt%line = line
        if (present(column)) continue_stmt%column = column

        call arena%push(continue_stmt, "continue_node", parent_index)
        continue_index = arena%size
    end function push_continue

    ! Create GOTO statement node and add to stack
    function push_goto(arena, label, label_list, selector_index, line, column, &
                       parent_index) result(goto_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: label_list
        integer, intent(in), optional :: selector_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: goto_index
        type(goto_node) :: goto_stmt
        goto_stmt%uid = generate_uid()
        if (present(label)) goto_stmt%label = label
        if (present(label_list)) goto_stmt%label_list = label_list
        if (present(selector_index)) goto_stmt%selector_index = selector_index
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

    function push_pause(arena, pause_code_index, pause_message, line, column, &
                        parent_index) result(pause_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: pause_code_index
        character(len=*), intent(in), optional :: pause_message
        integer, intent(in), optional :: line, column, parent_index
        integer :: pause_index
        type(pause_node) :: pause_stmt
        pause_stmt%uid = generate_uid()
        pause_stmt%pause_code_index = 0
        if (present(pause_code_index)) pause_stmt%pause_code_index = pause_code_index
        if (present(pause_message)) pause_stmt%pause_message = pause_message
        if (present(line)) pause_stmt%line = line
        if (present(column)) pause_stmt%column = column

        call arena%push(pause_stmt, "pause_node", parent_index)
        pause_index = arena%size
    end function push_pause

    ! Create NULLIFY statement node and add to stack
    function push_nullify(arena, pointer_indices, line, column, &
                          parent_index) result(nullify_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: pointer_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: nullify_index
        type(nullify_node) :: nullify_stmt
        nullify_stmt%uid = generate_uid()
        if (present(pointer_indices)) then
            if (size(pointer_indices) > 0) then
                nullify_stmt%pointer_indices = pointer_indices
            end if
        end if
        if (present(line)) nullify_stmt%line = line
        if (present(column)) nullify_stmt%column = column

        call arena%push(nullify_stmt, "nullify_node", parent_index)
        nullify_index = arena%size
    end function push_nullify

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
    function push_exit(arena, loop_label, line, column, parent_index) &
        result(exit_index)
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
                           line, column, parent_index, type_spec) result(alloc_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: var_indices(:)
        integer, intent(in), optional :: shape_indices(:)
        integer, intent(in), optional :: stat_var_index
        integer, intent(in), optional :: errmsg_var_index
        integer, intent(in), optional :: source_expr_index
        integer, intent(in), optional :: mold_expr_index
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: type_spec
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

        if (present(type_spec)) then
            if (len_trim(type_spec) > 0) then
                alloc_stmt%type_spec = trim(type_spec)
            end if
        end if

        if (present(stat_var_index)) alloc_stmt%stat_var_index = stat_var_index
        if (present(errmsg_var_index)) alloc_stmt%errmsg_var_index = errmsg_var_index
        if (present(source_expr_index)) alloc_stmt%source_expr_index = &
            source_expr_index
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
