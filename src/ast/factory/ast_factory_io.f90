module ast_factory_io
    use ast_core
    implicit none
    private

    ! Public I/O statement node creation functions
    public :: push_print_statement, push_write_statement, push_read_statement
    public :: push_read_statement_with_err, push_read_statement_with_end
    public :: push_read_statement_with_all_specifiers
    public :: push_write_statement_with_iostat, push_write_statement_with_format
    public :: push_write_statement_with_runtime_format

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
    end function push_print_statement

    function push_write_statement(arena, unit_spec, arg_indices, format_spec, &
                                 line, column, parent_index) result(write_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: arg_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: write_index
        type(write_statement_node) :: write_stmt

        write_stmt%unit_spec = unit_spec
        if (present(arg_indices)) then
            if (size(arg_indices) > 0) then
                write_stmt%arg_indices = arg_indices
            end if
        end if
        if (present(format_spec)) write_stmt%format_spec = format_spec
        if (present(line)) write_stmt%line = line
        if (present(column)) write_stmt%column = column

        call arena%push(write_stmt, "write_statement", parent_index)
        write_index = arena%size
    end function push_write_statement

    function push_read_statement(arena, unit_spec, var_indices, format_spec, &
                                line, column, parent_index) result(read_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: unit_spec
        integer, intent(in), optional :: var_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column, parent_index
        integer :: read_index
        type(read_statement_node) :: read_stmt

        read_stmt%unit_spec = unit_spec
        if (present(var_indices)) then
            if (size(var_indices) > 0) then
                read_stmt%var_indices = var_indices
            end if
        end if
        if (present(format_spec)) read_stmt%format_spec = format_spec
        if (present(line)) read_stmt%line = line
        if (present(column)) read_stmt%column = column

        call arena%push(read_stmt, "read_statement", parent_index)
        read_index = arena%size
    end function push_read_statement

    ! Extended I/O statement functions with iostat/err/end specifiers
    function push_write_statement_with_iostat(arena, unit_spec, arg_indices, format_spec, &
                             iostat_var, line, column, parent_index) result(write_index)
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
                               err_label, line, column, parent_index) result(read_index)
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
                               end_label, line, column, parent_index) result(read_index)
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
                                          line, column, parent_index) result(read_index)
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
    function push_write_statement_with_format(arena, unit_spec, arg_indices, format_spec, &
                                         line, column, parent_index) result(write_index)
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
                                         line, column, parent_index) result(write_index)
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

end module ast_factory_io