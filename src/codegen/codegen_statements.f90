module codegen_statements
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_io
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_utilities, only: generate_code_from_arena
    implicit none
    private

    public :: generate_code_assignment
    public :: generate_code_subroutine_call
    public :: generate_code_print_statement
    public :: generate_code_write_statement
    public :: generate_code_read_statement
    public :: generate_code_stop
    public :: generate_code_return
    public :: generate_code_goto
    public :: generate_code_error_stop
    public :: generate_code_cycle
    public :: generate_code_exit
    public :: generate_code_use_statement
    public :: generate_code_implicit_statement
    public :: generate_code_comment
    public :: generate_code_blank_line

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
                    args_code = args_code // generate_code_from_arena(arena, node%arg_indices(i))
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

        ! Generate output list
        ! For now, generate arguments directly without recursive calls
        args_code = ""
        if (allocated(node%expression_indices)) then
            do i = 1, size(node%expression_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%expression_indices(i) > 0 .and. node%expression_indices(i) <= arena%size) then
                    ! Instead of calling the stub, generate inline code for common cases
                    block
                        select type (arg_node => arena%entries(node%expression_indices(i))%node)
                        type is (literal_node)
                            args_code = args_code // arg_node%value
                        type is (identifier_node)
                            args_code = args_code // arg_node%name
                        type is (binary_op_node)
                            ! For operators, need proper generation
                            args_code = args_code // "EXPR"
                        class default
                            ! For other types, use placeholder
                            args_code = args_code // "..."
                        end select
                    end block
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

        ! Generate argument list
        ! For now, generate arguments directly without recursive calls
        args_code = ""
        if (allocated(node%arg_indices)) then
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                    ! Instead of calling the stub, generate inline code for common cases
                    block
                        select type (arg_node => arena%entries(node%arg_indices(i))%node)
                        type is (literal_node)
                            args_code = args_code // arg_node%value
                        type is (identifier_node)
                            args_code = args_code // arg_node%name
                        type is (binary_op_node)
                            ! For operators, need proper generation
                            args_code = args_code // "EXPR"
                        class default
                            ! For other types, use placeholder
                            args_code = args_code // "..."
                        end select
                    end block
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

        ! Simplified placeholder implementation
        ! TODO: Implement proper read statement code generation
        code = "read(*, *) ! TODO: implement read statement"
    end function generate_code_read_statement

    ! Generate code for stop statements
    function generate_code_stop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Simplified placeholder implementation
        code = "stop"
    end function generate_code_stop

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

        ! Simplified placeholder implementation
        code = "go to 999 ! TODO: implement proper goto"
    end function generate_code_goto

    ! Generate code for error stop statements
    function generate_code_error_stop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(error_stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Simplified placeholder implementation
        code = "error stop"
    end function generate_code_error_stop

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

        ! Simplified placeholder implementation  
        if (allocated(node%module_name)) then
            code = "use " // node%module_name
        else
            code = "use ! TODO: implement proper use statement"
        end if
    end function generate_code_use_statement

    ! Generate code for implicit statements
    function generate_code_implicit_statement(node) result(code)
        type(implicit_statement_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Simplified placeholder implementation
        code = "implicit none ! TODO: implement proper implicit statement"
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

end module codegen_statements