module codegen_expressions
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_core
    use ast_nodes_data
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    implicit none
    private

    public :: generate_code_literal
    public :: generate_code_identifier
    public :: generate_code_binary_op
    public :: generate_code_component_access
    public :: generate_code_range_subscript
    public :: generate_code_call_or_subscript
    public :: generate_code_array_literal
    public :: generate_code_range_expression
    public :: generate_code_array_bounds
    public :: generate_code_array_slice
    public :: generate_code_array_operation
    public :: generate_code_implied_do
    public :: get_operator_precedence
    public :: needs_parentheses
    public :: int_to_string

    ! Stub implementation provided below to break circular dependencies

contains

    ! Simple stub implementation to break circular dependency
    function generate_code_from_arena(arena, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        code = "! TODO: implement proper codegen call"
    end function generate_code_from_arena

    ! Generate code for literal nodes
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Simplified implementation using the 'value' field
        if (allocated(node%value)) then
            code = node%value
        else
            code = "! TODO: implement literal generation"
        end if
    end function generate_code_literal

    ! Generate code for identifier nodes
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code

        if (allocated(node%name)) then
            code = node%name
        else
            code = "! TODO: implement identifier generation"
        end if
    end function generate_code_identifier

    ! Generate code for binary operations
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code

        ! Generate operands
        if (node%left_index > 0) then
            left_code = generate_code_from_arena(arena, node%left_index)
        else
            left_code = ""
        end if

        if (node%right_index > 0) then
            right_code = generate_code_from_arena(arena, node%right_index)
        else
            right_code = ""
        end if

        ! Generate operation
        if (allocated(node%operator)) then
            code = left_code // " " // node%operator // " " // right_code
        else
            code = left_code // " ! TODO: implement operator " // right_code
        end if
    end function generate_code_binary_op

    ! Generate code for component access
    function generate_code_component_access(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(component_access_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "! TODO: implement component access"
    end function generate_code_component_access

    ! Generate code for range subscripts
    function generate_code_range_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(range_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "! TODO: implement range subscript"
    end function generate_code_range_subscript

    ! Generate code for call or subscript nodes
    function generate_code_call_or_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: args_code
        integer :: i

        if (allocated(node%name)) then
            code = node%name
        else
            code = "unknown"
        end if

        ! Generate arguments
        if (allocated(node%arg_indices)) then
            args_code = ""
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0) then
                    args_code = args_code // generate_code_from_arena(arena, node%arg_indices(i))
                end if
            end do
            code = code // "(" // args_code // ")"
        end if
    end function generate_code_call_or_subscript

    ! Generate code for array literals
    function generate_code_array_literal(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = "(/ ! TODO: implement array literal /)"
    end function generate_code_array_literal

    ! Placeholder functions for missing implementations
    function generate_code_range_expression(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        code = "! TODO: range expression"
    end function generate_code_range_expression

    function generate_code_array_bounds(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        code = "! TODO: array bounds"
    end function generate_code_array_bounds

    function generate_code_array_slice(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        code = "! TODO: array slice"
    end function generate_code_array_slice

    function generate_code_array_operation(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        code = "! TODO: array operation"
    end function generate_code_array_operation

    function generate_code_implied_do(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        code = "! TODO: implied do"
    end function generate_code_implied_do

    ! Get operator precedence
    function get_operator_precedence(op) result(precedence)
        character(len=*), intent(in) :: op
        integer :: precedence

        ! Simplified precedence - all operators have same precedence
        precedence = 1
    end function get_operator_precedence

    ! Check if parentheses are needed
    function needs_parentheses(parent_op, child_op, is_left) result(needs_parens)
        character(len=*), intent(in) :: parent_op, child_op
        logical, intent(in) :: is_left
        logical :: needs_parens

        ! Simplified - always add parentheses for safety
        needs_parens = .true.
    end function needs_parentheses

    ! Convert integer to string
    function int_to_string(n) result(str)
        integer, intent(in) :: n
        character(len=:), allocatable :: str
        character(len=32) :: buffer

        write(buffer, '(I0)') n
        str = trim(buffer)
    end function int_to_string

    ! generate_code_from_arena is provided as an interface at the module level

end module codegen_expressions