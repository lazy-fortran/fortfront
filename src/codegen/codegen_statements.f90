module codegen_statements
    use iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_io
    use ast_nodes_misc
    use ast_nodes_procedure
    use ast_nodes_control
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private

    public :: generate_code_assignment
    public :: generate_code_subroutine_call
    public :: generate_code_print_statement
    public :: generate_code_write_statement
    public :: generate_code_read_statement
    public :: generate_code_termination
    public :: generate_code_return
    public :: generate_code_goto
    public :: generate_code_error_termination
    public :: generate_code_cycle
    public :: generate_code_exit
    public :: generate_code_use_statement
    public :: generate_code_implicit_statement
    public :: generate_code_comment
    public :: generate_code_blank_line
    public :: generate_code_allocate_statement
    public :: generate_code_deallocate_statement

contains
    ! Generate code for assignment statements
    function generate_code_assignment(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate left-hand side
        if (node%target_index > 0 .and. node%target_index <= arena%size) then
            left_code = generate_code_from_arena(arena, node%target_index)
        else
            left_code = ""
        end if

        ! Generate right-hand side
        if (node%value_index > 0 .and. node%value_index <= arena%size) then
            right_code = generate_code_from_arena(arena, node%value_index)
        else
            right_code = ""
        end if

        ! Build assignment
        if (allocated(node%operator) .and. node%operator == "=>") then
            code = left_code // " => " // right_code
        else
            code = left_code // " = " // right_code
        end if
    end function generate_code_assignment

    ! Generate code for subroutine calls
    function generate_code_subroutine_call(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_call_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        code = "call " // node%name

        ! Generate arguments
        if (allocated(node%arg_indices)) then
            args_code = ""
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                    args_code = args_code // generate_code_from_arena(arena, &
                                                                      node%arg_indices(i))
                end if
            end do
            code = code // "(" // args_code // ")"
        else
            code = code // "()"
        end if
    end function generate_code_subroutine_call

    ! Generate code for print statements
    function generate_code_print_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: format_code, args_code
        integer :: i

        ! Generate format specifier
        if (allocated(node%format_spec)) then
            format_code = node%format_spec
        else
            format_code = "*"
        end if

        ! Generate output list using recursive code generation for each expression
        args_code = ""
        if (allocated(node%expression_indices)) then
            do i = 1, size(node%expression_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%expression_indices(i) > 0 .and. node%expression_indices(i) <= &
                    arena%size) then
                    args_code = args_code // generate_code_from_arena(arena, &
                                                               node%expression_indices(i))
                end if
            end do
        end if

        code = "print " // format_code
        if (len(args_code) > 0) then
            code = code // ", " // args_code
        end if
    end function generate_code_print_statement

    ! Generate code for write statements
    function generate_code_write_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(write_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: unit_code, format_code, args_code
        integer :: i

        ! Generate unit specifier
        if (allocated(node%unit_spec)) then
            unit_code = node%unit_spec
        else
            unit_code = "*"
        end if

        ! Generate format specifier
        if (allocated(node%format_spec)) then
            format_code = node%format_spec
        else
            format_code = "*"
        end if

        ! Generate argument list using recursive code generation
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                    ! Use recursive code generation for proper expression handling
                    args_code = args_code // generate_code_from_arena(arena, &
                                                                      node%arg_indices(i))
                end if
            end do
        end if

        ! Assemble write statement: write(unit, format) args
        code = "write(" // unit_code // ", " // format_code // ")"
        if (len(args_code) > 0) then
            code = code // " " // args_code
        end if
    end function generate_code_write_statement

    ! Generate code for read statements
    function generate_code_read_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(read_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate read statement code
        code = "read(*, *)"  ! Basic read statement
    end function generate_code_read_statement

    ! Generate code for termination statements
    function generate_code_termination(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate proper termination with exit call
        if (allocated(node%stop_message)) then
            code = "call exit(1) ! " // node%stop_message
        else if (node%stop_code_index > 0) then
            code = "call exit(1) ! Termination with code"
        else
            code = "call exit(1) ! Program termination"
        end if
    end function generate_code_termination

    ! Generate code for return statements
    function generate_code_return(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(return_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "return"
    end function generate_code_return

    ! Generate code for goto statements
    function generate_code_goto(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(goto_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate goto statement code
        code = "go to 999"  ! Basic goto statement
    end function generate_code_goto

    ! Generate code for error termination statements
    function generate_code_error_termination(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(error_stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate proper error termination with exit call
        if (allocated(node%error_message)) then
            code = "call exit(2) ! " // node%error_message
        else if (node%error_code_index > 0) then
            code = "call exit(2) ! Error termination with code"
        else
            code = "call exit(2) ! Error termination"
        end if
    end function generate_code_error_termination

    ! Generate code for cycle statements
    function generate_code_cycle(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(cycle_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Simplified placeholder implementation
        code = "cycle"
    end function generate_code_cycle

    ! Generate code for exit statements
    function generate_code_exit(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(exit_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Simplified placeholder implementation
        code = "exit"
    end function generate_code_exit

    ! Generate code for use statements
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: only_clause
        character(len=:), allocatable :: rename_entry
        integer :: i
        logical :: needs_double_colon

        ! Build proper use statement with all components
        if (.not. allocated(node%module_name)) then
            code = "use ! ERROR: no module name"
            return
        end if

        code = "use"

        needs_double_colon = node%has_double_colon .or. node%is_intrinsic .or. &
                             node%is_non_intrinsic

        ! Add URL spec for Go-style imports if present
        if (allocated(node%url_spec)) then
            code = code // " " // node%url_spec
            if (needs_double_colon) code = code // " ::"
        else
            if (node%is_intrinsic) then
                code = code // ", intrinsic"
            else if (node%is_non_intrinsic) then
                code = code // ", non_intrinsic"
            end if
            if (needs_double_colon) code = code // " ::"
        end if

        ! Add module name
        code = code // " " // node%module_name

        ! Add only clause if present
        only_clause = ""
        if (node%has_only) then
            if (allocated(node%only_list)) then
                do i = 1, size(node%only_list)
                    if (.not. allocated(node%only_list(i)%s)) cycle
                    if (len_trim(only_clause) > 0) only_clause = only_clause // ", "
                    only_clause = only_clause // node%only_list(i)%s
                end do
            end if
            if (allocated(node%rename_list)) then
                do i = 1, size(node%rename_list), 2
                    if (i + 1 > size(node%rename_list)) exit
                    if (.not. allocated(node%rename_list(i)%s)) cycle
                    if (.not. allocated(node%rename_list(i + 1)%s)) cycle
                    rename_entry = node%rename_list(i)%s // " => " // &
                                   node%rename_list(i + 1)%s
                    if (len_trim(only_clause) > 0) only_clause = only_clause // ", "
                    only_clause = only_clause // rename_entry
                end do
            end if
            if (len_trim(only_clause) > 0) then
                code = code // ", only: " // trim(only_clause)
            else
                code = code // ", only:"
            end if
        else if (allocated(node%rename_list)) then
            ! Fallback: emit rename list under only clause even if flag missing
            do i = 1, size(node%rename_list), 2
                if (i + 1 > size(node%rename_list)) exit
                if (.not. allocated(node%rename_list(i)%s)) cycle
                if (.not. allocated(node%rename_list(i + 1)%s)) cycle
                rename_entry = node%rename_list(i)%s // " => " // &
                               node%rename_list(i + 1)%s
                if (len_trim(only_clause) > 0) only_clause = only_clause // ", "
                only_clause = only_clause // rename_entry
            end do
            if (len_trim(only_clause) > 0) then
                code = code // ", only: " // trim(only_clause)
            end if
        end if
    end function generate_code_use_statement

    ! Generate code for implicit statements
    function generate_code_implicit_statement(node) result(code)
        type(implicit_statement_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Generate implicit statement code
        code = "implicit none"  ! Standard implicit none statement
    end function generate_code_implicit_statement

    ! Generate code for comment nodes
    function generate_code_comment(node) result(code)
        type(comment_node), intent(in) :: node
        character(len=:), allocatable :: code

        if (allocated(node%text)) then
            code = "!" // node%text
        else
            code = "!"
        end if
    end function generate_code_comment

    ! Generate code for blank line nodes
    function generate_code_blank_line(node) result(code)
        type(blank_line_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        ! Generate the appropriate number of blank lines
        code = ""
        do i = 1, node%count
            code = code // new_line('A')
        end do
    end function generate_code_blank_line

    ! generate_code_from_arena is provided as an interface at the module level

    ! Generate code for allocate statements
    function generate_code_allocate_statement(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(allocate_statement_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args, var_code, shape_code
        integer :: i, j

        args = ""

        if (allocated(node%var_indices)) then
            do i = 1, size(node%var_indices)
                if (i > 1) args = args // ", "
                if (node%var_indices(i) > 0 .and. node%var_indices(i) <= arena%size) then
                    var_code = generate_code_from_arena(arena, node%var_indices(i))
                else
                    var_code = ""
                end if
                if (i == 1) then
                    ! Attach shape to the first variable if present
                    if (allocated(node%shape_indices)) then
                        if (size(node%shape_indices) > 0) then
                            shape_code = ""
                            do j = 1, size(node%shape_indices)
                                if (j > 1) shape_code = shape_code // ", "
                                if (node%shape_indices(j) > 0 .and. &
                                    node%shape_indices(j) <= arena%size) then
                                    shape_code = shape_code // &
                                                 generate_code_from_arena(arena, &
                                                                    node%shape_indices(j))
                                end if
                            end do
                            if (len(shape_code) > 0) then
                                var_code = var_code // "(" // shape_code // ")"
                            end if
                        end if
                    end if
                end if
                args = args // var_code
            end do
        end if

        ! Optional keyword arguments
        if (node%stat_var_index > 0 .and. node%stat_var_index <= arena%size) then
            args = args // ", stat=" // generate_code_from_arena(arena, &
                                                                 node%stat_var_index)
        end if
        if (node%errmsg_var_index > 0 .and. node%errmsg_var_index <= arena%size) then
            args = args // ", errmsg=" // generate_code_from_arena(arena, &
                                                                   node%errmsg_var_index)
        end if
        if (node%source_expr_index > 0 .and. node%source_expr_index <= arena%size) then
            args = args // ", source=" // generate_code_from_arena(arena, &
                                                                   node%source_expr_index)
        end if
        if (node%mold_expr_index > 0 .and. node%mold_expr_index <= arena%size) then
            args = args // ", mold=" // generate_code_from_arena(arena, &
                                                                 node%mold_expr_index)
        end if

        code = "allocate(" // args // ")"
    end function generate_code_allocate_statement

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
                if (node%var_indices(i) > 0 .and. node%var_indices(i) <= arena%size) then
                    args = args // generate_code_from_arena(arena, node%var_indices(i))
                end if
            end do
        end if

        if (node%stat_var_index > 0 .and. node%stat_var_index <= arena%size) then
            args = args // ", stat=" // generate_code_from_arena(arena, &
                                                                 node%stat_var_index)
        end if
        if (node%errmsg_var_index > 0 .and. node%errmsg_var_index <= arena%size) then
            args = args // ", errmsg=" // generate_code_from_arena(arena, &
                                                                   node%errmsg_var_index)
        end if

        code = "deallocate(" // args // ")"
    end function generate_code_deallocate_statement

end module codegen_statements
