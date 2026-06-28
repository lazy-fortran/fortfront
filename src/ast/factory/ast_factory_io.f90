module ast_factory_io
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
        read_statement_node, format_statement_node, &
        open_statement_node, close_statement_node, &
        inquire_statement_node, backspace_statement_node, &
        rewind_statement_node, endfile_statement_node
    implicit none
    private

    ! Public I/O statement node creation functions
    public :: push_print_statement, push_write_statement, push_read_statement
    public :: push_read_statement_with_err, push_read_statement_with_end
    public :: push_read_statement_with_all_specifiers
    public :: push_write_statement_with_iostat, push_write_statement_with_format
    public :: push_write_statement_with_runtime_format
    public :: push_format_statement
    public :: push_open_statement, push_close_statement
    public :: push_inquire_statement
    public :: push_backspace_statement, push_rewind_statement, push_endfile_statement

contains

    ! Create print statement node and add to stack
    function push_print_statement(arena, format_spec, arg_indices, line, &
            column, parent_index) result(print_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: format_spec
        integer, intent(in), optional :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: print_index
        type(print_statement_node) :: print_stmt

        print_stmt%format_spec = format_spec
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                print_stmt%expression_indices = arg_indices
            end if
        end if
        if (present(line)) print_stmt%line = line
        if (present(column)) print_stmt%column = column

        call arena%push(print_stmt, "print_statement", parent_index)
        print_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                call link_children_to_parent(arena, print_index, arg_indices)
            end if
        end if
    end function push_print_statement

    function push_write_statement(arena, unit_spec, arg_indices, format_spec, &
            namelist_group, io_control_list, &
            format_expr_index, line, column, &
            parent_index) result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: arg_indices(:)
        character(len=*), intent(in), optional :: format_spec
        character(len=*), intent(in), optional :: namelist_group
        character(len=*), intent(in), optional :: io_control_list
        integer, intent(in), optional :: format_expr_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                write_stmt%arg_indices = arg_indices
            end if
        end if
        if (present(format_spec)) then
            if (len(format_spec) > 0) write_stmt%format_spec = format_spec
        end if
        if (present(format_expr_index)) then
            if (format_expr_index > 0) write_stmt%format_expr_index = format_expr_index
        end if
        if (present(namelist_group)) then
            if (len(namelist_group) > 0) write_stmt%namelist_group = namelist_group
        end if
        if (present(io_control_list)) then
            if (len(io_control_list) > 0) write_stmt%io_control_list = io_control_list
        end if
        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size

        ! Link children to this parent for AST traversal
        if (write_stmt%format_expr_index > 0) then
            call link_children_to_parent(arena, write_index, &
                [write_stmt%format_expr_index])
        end if
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                call link_children_to_parent(arena, write_index, arg_indices)
            end if
        end if
    end function push_write_statement

    function push_read_statement(arena, unit_spec, var_indices, format_spec, &
            namelist_group, io_control_list, &
            format_expr_index, line, column, &
            parent_index) result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: var_indices(:)
        character(len=*), intent(in), optional :: format_spec
        character(len=*), intent(in), optional :: namelist_group
        character(len=*), intent(in), optional :: io_control_list
        integer, intent(in), optional :: format_expr_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        if (present(var_indices)) then
            if (size(var_indices) > 0) then
                read_stmt%var_indices = var_indices
            end if
        end if
        if (present(format_spec)) then
            if (len(format_spec) > 0) read_stmt%format_spec = format_spec
        end if
        if (present(format_expr_index)) then
            if (format_expr_index > 0) read_stmt%format_expr_index = format_expr_index
        end if
        if (present(namelist_group)) then
            if (len(namelist_group) > 0) read_stmt%namelist_group = namelist_group
        end if
        if (present(io_control_list)) then
            if (len(io_control_list) > 0) read_stmt%io_control_list = io_control_list
        end if
        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size

        ! Link children to this parent for AST traversal
        if (read_stmt%format_expr_index > 0) then
            call link_children_to_parent(arena, read_index, &
                [read_stmt%format_expr_index])
        end if
        if (present(var_indices)) then
            if (size(var_indices) > 0) then
                call link_children_to_parent(arena, read_index, var_indices)
            end if
        end if
    end function push_read_statement

    ! Extended I/O statement functions with iostat/err/end specifiers
    function push_write_statement_with_iostat(arena, unit_spec, arg_indices, &
            format_spec, &
            iostat_var, line, column, parent_index) &
            result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: arg_indices(:), iostat_var
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        write_stmt%format_spec = format_spec
        write_stmt%arg_indices = arg_indices
        write_stmt%iostat_var_index = iostat_var

        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size
    end function push_write_statement_with_iostat

    function push_read_statement_with_err(arena, unit_spec, var_indices, format_spec, &
            err_label, line, column, parent_index) &
            result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: var_indices(:), err_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        read_stmt%format_spec = format_spec
        read_stmt%var_indices = var_indices
        read_stmt%err_label_index = err_label

        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size
    end function push_read_statement_with_err

    function push_read_statement_with_end(arena, unit_spec, var_indices, format_spec, &
            end_label, line, column, parent_index) &
            result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: var_indices(:), end_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        read_stmt%format_spec = format_spec
        read_stmt%var_indices = var_indices
        read_stmt%end_label_index = end_label

        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size
    end function push_read_statement_with_end

    function push_read_statement_with_all_specifiers(arena, unit_spec, &
            var_indices, format_spec, &
            iostat_var, err_label, end_label, &
            line, column, parent_index) &
            result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: var_indices(:), iostat_var, err_label, end_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        read_stmt%format_spec = format_spec
        read_stmt%var_indices = var_indices
        read_stmt%iostat_var_index = iostat_var
        read_stmt%err_label_index = err_label
        read_stmt%end_label_index = end_label

        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size
    end function push_read_statement_with_all_specifiers

    ! Format descriptor support functions
    function push_write_statement_with_format(arena, unit_spec, arg_indices, &
            format_spec, &
            line, column, parent_index) &
            result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec, format_spec
        integer, intent(in) :: arg_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        write_stmt%format_spec = format_spec
        write_stmt%arg_indices = arg_indices
        write_stmt%is_formatted = .true.

        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size
    end function push_write_statement_with_format

    function push_write_statement_with_runtime_format(arena, unit_spec, &
            arg_indices, format_var, &
            line, column, parent_index) &
            result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in) :: arg_indices(:), format_var
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        write_stmt%arg_indices = arg_indices
        write_stmt%format_expr_index = format_var
        write_stmt%is_formatted = .true.

        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size
    end function push_write_statement_with_runtime_format

    function push_format_statement(arena, format_spec, line, column, &
            parent_index) result(format_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: format_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: format_index
        type(format_statement_node) :: format_stmt

        format_stmt%format_spec = format_spec
        if (present(line)) format_stmt%line = line
        if (present(column)) format_stmt%column = column

        call arena%push(format_stmt, "format_statement", parent_index)
        format_index = arena%size
    end function push_format_statement

    function push_open_statement(arena, spec_text, line, column, parent_index) &
            result(open_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: spec_text
        integer, intent(in), optional :: line, column, parent_index
        integer :: open_index
        type(open_statement_node) :: open_stmt

        open_stmt%unit_spec = spec_text
        if (present(line)) open_stmt%line = line
        if (present(column)) open_stmt%column = column

        call arena%push(open_stmt, "open_statement", parent_index)
        open_index = arena%size
    end function push_open_statement

    function push_close_statement(arena, spec_text, line, column, parent_index) &
            result(close_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: spec_text
        integer, intent(in), optional :: line, column, parent_index
        integer :: close_index
        type(close_statement_node) :: close_stmt

        close_stmt%unit_spec = spec_text
        if (present(line)) close_stmt%line = line
        if (present(column)) close_stmt%column = column

        call arena%push(close_stmt, "close_statement", parent_index)
        close_index = arena%size
    end function push_close_statement

    function push_inquire_statement(arena, spec_text, line, column, &
            parent_index) result(inquire_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: spec_text
        integer, intent(in), optional :: line, column, parent_index
        integer :: inquire_index
        type(inquire_statement_node) :: inquire_stmt

        inquire_stmt%spec_list = spec_text
        if (present(line)) inquire_stmt%line = line
        if (present(column)) inquire_stmt%column = column

        call arena%push(inquire_stmt, "inquire_statement", parent_index)
        inquire_index = arena%size
    end function push_inquire_statement

    function push_backspace_statement(arena, unit_spec, line, column, &
            parent_index) result(backspace_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: backspace_index
        type(backspace_statement_node) :: backspace_stmt

        backspace_stmt%unit_spec = unit_spec
        if (present(line)) backspace_stmt%line = line
        if (present(column)) backspace_stmt%column = column

        call arena%push(backspace_stmt, "backspace_statement", parent_index)
        backspace_index = arena%size
    end function push_backspace_statement

    function push_rewind_statement(arena, unit_spec, line, column, &
            parent_index) result(rewind_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: rewind_index
        type(rewind_statement_node) :: rewind_stmt

        rewind_stmt%unit_spec = unit_spec
        if (present(line)) rewind_stmt%line = line
        if (present(column)) rewind_stmt%column = column

        call arena%push(rewind_stmt, "rewind_statement", parent_index)
        rewind_index = arena%size
    end function push_rewind_statement

    function push_endfile_statement(arena, unit_spec, line, column, &
            parent_index) result(endfile_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: endfile_index
        type(endfile_statement_node) :: endfile_stmt

        endfile_stmt%unit_spec = unit_spec
        if (present(line)) endfile_stmt%line = line
        if (present(column)) endfile_stmt%column = column

        call arena%push(endfile_stmt, "endfile_statement", parent_index)
        endfile_index = arena%size
    end function push_endfile_statement

end module ast_factory_io
