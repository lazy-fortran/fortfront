module codegen_statements
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_io
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
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
        if (node%left > 0 .and. node%left <= arena%size) then
            left_code = generate_code_polymorphic_internal(arena, node%left)
        else
            left_code = ""
        end if

        ! Generate right-hand side
        if (node%right > 0 .and. node%right <= arena%size) then
            right_code = generate_code_polymorphic_internal(arena, node%right)
        else
            right_code = ""
        end if

        ! Build assignment
        if (node%is_pointer_assignment) then
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
        if (allocated(node%args)) then
            args_code = ""
            do i = 1, size(node%args)
                if (i > 1) args_code = args_code // ", "
                if (node%args(i) > 0 .and. node%args(i) <= arena%size) then
                    args_code = args_code // generate_code_polymorphic_internal(arena, node%args(i))
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
        if (node%format_expr > 0 .and. node%format_expr <= arena%size) then
            format_code = generate_code_polymorphic_internal(arena, node%format_expr)
        else
            format_code = "*"
        end if

        ! Generate output list
        args_code = ""
        if (allocated(node%args)) then
            do i = 1, size(node%args)
                if (i > 1) args_code = args_code // ", "
                if (node%args(i) > 0 .and. node%args(i) <= arena%size) then
                    args_code = args_code // generate_code_polymorphic_internal(arena, node%args(i))
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
        if (node%unit_expr > 0) then
            unit_code = generate_code_polymorphic_internal(arena, node%unit_expr)
        else
            unit_code = "*"
        end if

        ! Generate format specifier
        if (node%format_expr > 0) then
            format_code = generate_code_polymorphic_internal(arena, node%format_expr)
        else
            format_code = "*"
        end if

        ! Generate output list
        args_code = ""
        if (allocated(node%output_items)) then
            do i = 1, size(node%output_items)
                if (i > 1) args_code = args_code // ", "
                if (node%output_items(i) > 0) then
                    args_code = args_code // generate_code_polymorphic_internal(arena, node%output_items(i))
                end if
            end do
        end if

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
        character(len=:), allocatable :: unit_code, format_code, args_code
        integer :: i

        ! Generate unit specifier
        if (node%unit_expr > 0) then
            unit_code = generate_code_polymorphic_internal(arena, node%unit_expr)
        else
            unit_code = "*"
        end if

        ! Generate format specifier
        if (node%format_expr > 0) then
            format_code = generate_code_polymorphic_internal(arena, node%format_expr)
        else
            format_code = "*"
        end if

        ! Generate input list
        args_code = ""
        if (allocated(node%input_items)) then
            do i = 1, size(node%input_items)
                if (i > 1) args_code = args_code // ", "
                if (node%input_items(i) > 0) then
                    args_code = args_code // generate_code_polymorphic_internal(arena, node%input_items(i))
                end if
            end do
        end if

        code = "read(" // unit_code // ", " // format_code // ")"
        if (len(args_code) > 0) then
            code = code // " " // args_code
        end if
    end function generate_code_read_statement

    ! Generate code for stop statements
    function generate_code_stop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stop_code

        code = "stop"

        ! Add stop code if present
        if (node%stop_code > 0 .and. node%stop_code <= arena%size) then
            stop_code = generate_code_polymorphic_internal(arena, node%stop_code)
            code = code // " " // stop_code
        else if (allocated(node%message)) then
            code = code // ' "' // node%message // '"'
        end if
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
        character(len=32) :: label_str

        if (node%is_computed) then
            code = "go to ("
            ! Add label list (not implemented yet)
            code = code // ")"
        else
            write(label_str, '(I0)') node%label
            code = "go to " // trim(label_str)
        end if
    end function generate_code_goto

    ! Generate code for error stop statements
    function generate_code_error_stop(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(error_stop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stop_code

        code = "error stop"

        ! Add stop code or message
        if (node%stop_code > 0 .and. node%stop_code <= arena%size) then
            stop_code = generate_code_polymorphic_internal(arena, node%stop_code)
            code = code // " " // stop_code
        else if (allocated(node%message)) then
            code = code // ' "' // node%message // '"'
        end if
    end function generate_code_error_stop

    ! Generate code for cycle statements
    function generate_code_cycle(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(cycle_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "cycle"
        if (allocated(node%construct_name)) then
            code = code // " " // node%construct_name
        end if
    end function generate_code_cycle

    ! Generate code for exit statements
    function generate_code_exit(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(exit_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "exit"
        if (allocated(node%construct_name)) then
            code = code // " " // node%construct_name
        end if
    end function generate_code_exit

    ! Generate code for use statements
    function generate_code_use_statement(node) result(code)
        type(use_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i

        code = "use "
        if (node%intrinsic_module) then
            code = code // ", intrinsic :: "
        end if
        code = code // node%module_name

        if (allocated(node%only_list)) then
            code = code // ", only: "
            do i = 1, size(node%only_list)
                if (i > 1) code = code // ", "
                if (allocated(node%only_list(i)%local_name) .and. &
                    node%only_list(i)%local_name /= node%only_list(i)%module_name) then
                    code = code // node%only_list(i)%local_name // " => " // node%only_list(i)%module_name
                else
                    code = code // node%only_list(i)%module_name
                end if
            end do
        end if
    end function generate_code_use_statement

    ! Generate code for implicit statements
    function generate_code_implicit_statement(node) result(code)
        type(implicit_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i, j
        character(len=1) :: start_char, end_char

        if (node%implicit_none) then
            code = "implicit none"
            if (allocated(node%implicit_none_spec)) then
                code = code // " ("
                do i = 1, size(node%implicit_none_spec)
                    if (i > 1) code = code // ", "
                    code = code // node%implicit_none_spec(i)
                end do
                code = code // ")"
            end if
        else
            code = "implicit"
            if (allocated(node%type_mappings)) then
                do i = 1, size(node%type_mappings)
                    if (i > 1) code = code // ","
                    code = code // " " // node%type_mappings(i)%type_spec // " ("
                    
                    ! Generate letter ranges
                    do j = 1, size(node%type_mappings(i)%letter_ranges, 1)
                        if (j > 1) code = code // ", "
                        start_char = node%type_mappings(i)%letter_ranges(j, 1)
                        end_char = node%type_mappings(i)%letter_ranges(j, 2)
                        if (start_char == end_char) then
                            code = code // start_char
                        else
                            code = code // start_char // "-" // end_char
                        end if
                    end do
                    code = code // ")"
                end do
            end if
        end if
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

        ! Blank lines are represented as empty strings
        code = ""
    end function generate_code_blank_line

    ! Internal polymorphic code generator
    function generate_code_polymorphic_internal(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        
        ! Import the main dispatcher from codegen_core
        interface
            function generate_code_polymorphic(arena, node_index) result(code)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
                character(len=:), allocatable :: code
            end function generate_code_polymorphic
        end interface
        
        code = generate_code_polymorphic(arena, node_index)
    end function generate_code_polymorphic_internal

end module codegen_statements