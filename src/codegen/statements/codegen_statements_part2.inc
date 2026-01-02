
    ! Generate code for deallocate statements
    function generate_code_deallocate_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(deallocate_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args
        integer :: i

        args = ""

        if (allocated(node%var_indices)) then
            do i = 1, size(node%var_indices)
                if (i > 1) args = args // ", "
                if (node%var_indices(i) > 0 .and. &
                    node%var_indices(i) <= arena%size) then
                    args = args // generate_code_from_arena(arena, node%var_indices(i))
                end if
            end do
        end if

        if (node%stat_var_index > 0 .and. &
            node%stat_var_index <= arena%size) then
            args = args // ", stat=" // &
                   generate_code_from_arena(arena, node%stat_var_index)
        end if
        if (node%errmsg_var_index > 0 .and. &
            node%errmsg_var_index <= arena%size) then
            args = args // ", errmsg=" // &
                   generate_code_from_arena(arena, node%errmsg_var_index)
        end if

        code = "deallocate(" // args // ")"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_deallocate_statement

    function generate_code_open_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(open_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "open(" // node%unit_spec // ")"
        else
            code = "open()"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_open_statement

    function generate_code_close_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(close_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "close(" // node%unit_spec // ")"
        else
            code = "close()"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_close_statement

    function generate_code_inquire_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(inquire_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%spec_list)) then
            code = "inquire(" // node%spec_list // ")"
        else
            code = "inquire()"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_inquire_statement

    function generate_code_backspace_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(backspace_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "backspace " // node%unit_spec
        else
            code = "backspace"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_backspace_statement

    function generate_code_rewind_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(rewind_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "rewind " // node%unit_spec
        else
            code = "rewind"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_rewind_statement

    function generate_code_endfile_statement(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(endfile_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (allocated(node%unit_spec)) then
            code = "endfile " // node%unit_spec
        else
            code = "endfile"
        end if
        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_endfile_statement

    function generate_code_pause_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(pause_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: code_expr

        if (allocated(node%pause_message)) then
            code = "pause " // node%pause_message
        else if (node%pause_code_index > 0 .and. &
                 node%pause_code_index <= arena%size) then
            code_expr = generate_code_from_arena(arena, node%pause_code_index)
            code = "pause " // code_expr
        else
            code = "pause"
        end if

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_pause_statement

    function generate_code_nullify_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(nullify_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: ptr_code
        integer :: i

        code = "nullify("

        if (allocated(node%pointer_indices)) then
            do i = 1, size(node%pointer_indices)
                if (node%pointer_indices(i) > 0 .and. &
                    node%pointer_indices(i) <= arena%size) then
                    ptr_code = generate_code_from_arena(arena, node%pointer_indices(i))
                    if (i > 1) code = code // ", "
                    code = code // ptr_code
                end if
            end do
        end if

        code = code // ")"

        call prepend_stmt_label(code, node%stmt_label)
    end function generate_code_nullify_statement
