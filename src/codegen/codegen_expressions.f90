module codegen_expressions
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_core
    use ast_nodes_data
    use ast_nodes_bounds, only: array_slice_node, range_expression_node
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_arena_interface, only: generate_code_from_arena
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

contains

    ! Generate code for literal nodes
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code

        ! Generate literal value - handle missing values gracefully
        if (allocated(node%value)) then
            ! Transform non-Fortran boolean literals to Fortran format
            if (node%value == "true") then
                code = ".true."
            else if (node%value == "false") then
                code = ".false."
            else
                code = node%value
            end if
        else
            ! Fallback for missing literal value
            code = "0"  ! Safe default literal
        end if
    end function generate_code_literal

    ! Generate code for identifier nodes
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code

        if (allocated(node%name)) then
            code = node%name
        else
            ! Fallback for missing identifier name
            code = "temp_var"  ! Safe default variable name
        end if
    end function generate_code_identifier

    ! Generate code for binary operations
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code
        character(len=:), allocatable :: fortran_operator

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

        ! Determine the correct Fortran operator
        if (allocated(node%operator)) then
            fortran_operator = node%operator
            
            ! Check for string concatenation: if operator is '+' and we're dealing with string literals
            if (node%operator == "+" .and. is_string_concatenation(left_code, right_code)) then
                fortran_operator = "//"  ! Use Fortran string concatenation operator
            end if
            
            code = left_code // " " // fortran_operator // " " // right_code
        else
            ! Fallback for missing operator
            code = left_code // " + " // right_code  ! Safe default operator
        end if
    end function generate_code_binary_op

    ! Generate code for component access
    function generate_code_component_access(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(component_access_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate component access code (e.g., object%component)
        code = "component_access"  ! Basic implementation - proper component access needs object and component name
    end function generate_code_component_access

    ! Generate code for range subscripts
    function generate_code_range_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(range_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        ! Generate range subscript code (e.g., array(start:end))
        code = "1:n"  ! Basic range implementation - needs proper bounds from node
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

        ! Generate array literal code (e.g., (/1, 2, 3/))
        code = "(/ /)"  ! Empty array literal - proper implementation needs element processing
    end function generate_code_array_literal

    ! CRITICAL FIX: Generate proper range expression code (e.g. 1:3, :5, 2:, ::2)
    function generate_code_range_expression(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, stride_code
        
        select type (range_node => node)
        type is (range_expression_node)
            ! Generate start expression
            if (range_node%start_index > 0 .and. range_node%start_index <= arena%size) then
                start_code = generate_code_from_arena(arena, range_node%start_index)
            else
                start_code = ""  ! Implicit start (e.g., :5)
            end if
            
            ! Generate end expression  
            if (range_node%end_index > 0 .and. range_node%end_index <= arena%size) then
                end_code = generate_code_from_arena(arena, range_node%end_index)
            else
                end_code = ""  ! Implicit end (e.g., 2:)
            end if
            
            ! Generate stride expression (optional)
            if (range_node%stride_index > 0 .and. range_node%stride_index <= arena%size) then
                stride_code = generate_code_from_arena(arena, range_node%stride_index)
            else
                stride_code = ""  ! No stride
            end if
            
            ! Combine into range expression: start:end or start:end:stride
            code = start_code // ":" // end_code
            if (len_trim(stride_code) > 0) then
                code = code // ":" // stride_code
            end if
        class default
            ! Fallback for non-range_expression_node
            code = "1:10"
        end select
    end function generate_code_range_expression

    function generate_code_array_bounds(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        ! Generate array bounds code
        code = "1:"  ! Basic bounds - needs proper dimension analysis
    end function generate_code_array_bounds

    function generate_code_array_slice(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: array_code, bounds_code
        integer :: i
        
        ! CRITICAL FIX: Implement proper array slice code generation
        select type (slice_node => node)
        type is (array_slice_node)
            ! Generate the array part (e.g., 'name' in name(1:3))
            if (slice_node%array_index > 0 .and. slice_node%array_index <= arena%size) then
                array_code = generate_code_from_arena(arena, slice_node%array_index)
            else
                array_code = "unknown_array"
            end if
            
            ! Generate the bounds part (e.g., '1:3' in name(1:3))
            bounds_code = ""
            do i = 1, slice_node%num_dimensions
                if (i > 1) bounds_code = bounds_code // ", "
                if (slice_node%bounds_indices(i) > 0 .and. &
                    slice_node%bounds_indices(i) <= arena%size) then
                    bounds_code = bounds_code // generate_code_from_arena(arena, slice_node%bounds_indices(i))
                else
                    bounds_code = bounds_code // ":"  ! Default to full range if bounds missing
                end if
            end do
            
            ! Combine array and bounds: array(bounds)
            code = array_code // "(" // bounds_code // ")"
        class default
            ! Fallback for non-array_slice_node
            code = ":"
        end select
    end function generate_code_array_slice

    function generate_code_array_operation(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        ! Generate array operation code
        code = "array_op"  ! Basic operation - needs proper element-wise processing
    end function generate_code_array_operation

    function generate_code_implied_do(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        ! Generate implied do code (e.g., (expr, var=start,end))
        code = "(expr, i=1,n)"  ! Basic implied do - needs variable and bounds extraction
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

    ! Check if we have string concatenation (both operands are string literals)
    function is_string_concatenation(left_code, right_code) result(is_string)
        character(len=*), intent(in) :: left_code, right_code
        logical :: is_string
        
        ! Check if both operands are string literals (enclosed in single or double quotes)
        is_string = is_string_literal(left_code) .and. is_string_literal(right_code)
    end function is_string_concatenation

    ! Check if a code fragment is a string literal
    function is_string_literal(code) result(is_string)
        character(len=*), intent(in) :: code
        logical :: is_string
        character(len=:), allocatable :: trimmed_code
        
        ! Trim whitespace
        trimmed_code = trim(adjustl(code))
        
        ! Check if it starts and ends with quotes
        is_string = .false.
        if (len(trimmed_code) >= 2) then
            ! Check for single quotes
            if (trimmed_code(1:1) == "'" .and. trimmed_code(len(trimmed_code):len(trimmed_code)) == "'") then
                is_string = .true.
            ! Check for double quotes
            else if (trimmed_code(1:1) == '"' .and. trimmed_code(len(trimmed_code):len(trimmed_code)) == '"') then
                is_string = .true.
            end if
        end if
    end function is_string_literal

    ! generate_code_from_arena is provided as an interface at the module level

end module codegen_expressions
