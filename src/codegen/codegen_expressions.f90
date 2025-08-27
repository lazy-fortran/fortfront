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

contains

    ! Generate code for literal nodes
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=32) :: buffer

        select case (node%literal_type)
        case (LITERAL_INT)
            write(buffer, '(I0)') node%int_value
            code = trim(buffer)
        case (LITERAL_REAL)
            ! Check if the real value has a specific string representation
            if (allocated(node%real_str)) then
                code = trim(node%real_str)
            else
                ! Use compact representation for standard values
                if (node%real_value == 0.0d0) then
                    code = "0.0"
                else if (abs(node%real_value - real(int(node%real_value))) < 1e-10) then
                    ! Integer-like real value
                    write(buffer, '(I0)') int(node%real_value)
                    code = trim(buffer) // ".0"
                else if (abs(node%real_value) >= 1e6 .or. abs(node%real_value) <= 1e-3) then
                    ! Use scientific notation for very large or small numbers
                    write(buffer, '(ES12.5E2)') node%real_value
                    code = trim(adjustl(buffer))
                else
                    ! Standard notation with precision control
                    write(buffer, '(F0.6)') node%real_value
                    code = trim(buffer)
                    ! Remove trailing zeros after decimal point
                    i = len(code)
                    do while (i > 1 .and. code(i:i) == '0')
                        i = i - 1
                    end do
                    if (code(i:i) == '.') then
                        code = code(1:i) // '0'
                    else
                        code = code(1:i)
                    end if
                end if
            end if
        case (LITERAL_STRING)
            ! Keep original string delimiters
            if (allocated(node%string_value)) then
                if (index(node%string_value, '"') > 0 .and. index(node%string_value, "'") == 0) then
                    code = "'" // node%string_value // "'"
                else
                    code = '"' // node%string_value // '"'
                end if
            else
                code = '""'
            end if
        case (LITERAL_LOGICAL)
            if (node%logical_value) then
                code = ".true."
            else
                code = ".false."
            end if
        case (LITERAL_COMPLEX)
            ! Format complex literal
            write(buffer, '(F0.6)') real(node%complex_value)
            code = "(" // trim(buffer)
            write(buffer, '(F0.6)') aimag(node%complex_value)
            code = code // ", " // trim(buffer) // ")"
        case default
            code = ""
        end select
    end function generate_code_literal

    ! Generate code for identifier nodes
    function generate_code_identifier(node) result(code)
        type(identifier_node), intent(in) :: node
        character(len=:), allocatable :: code
        
        if (allocated(node%name)) then
            code = node%name
        else
            code = ""
        end if
    end function generate_code_identifier

    ! Get operator precedence for expression generation
    function get_operator_precedence(operator) result(precedence)
        integer, intent(in) :: operator
        integer :: precedence

        select case (operator)
        case (OP_POWER)
            precedence = 8
        case (OP_MULTIPLY, OP_DIVIDE)
            precedence = 7
        case (OP_PLUS, OP_MINUS)
            precedence = 6
        case (OP_CONCAT)
            precedence = 5
        case (OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE, OP_EQV, OP_NEQV)
            precedence = 4
        case (OP_NOT)
            precedence = 3
        case (OP_AND)
            precedence = 2
        case (OP_OR)
            precedence = 1
        case default
            precedence = 0
        end select
    end function get_operator_precedence

    ! Check if parentheses are needed for an operand
    function needs_parentheses(arena, operand_index, parent_op, is_left_operand) result(needs)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: operand_index
        integer, intent(in) :: parent_op
        logical, intent(in) :: is_left_operand
        logical :: needs

        integer :: operand_precedence, parent_precedence

        needs = .false.
        if (operand_index <= 0 .or. operand_index > arena%size) return
        if (.not. allocated(arena%entries(operand_index)%node)) return

        select type (operand => arena%entries(operand_index)%node)
        type is (binary_op_node)
            operand_precedence = get_operator_precedence(operand%op)
            parent_precedence = get_operator_precedence(parent_op)
            
            if (operand_precedence < parent_precedence) then
                needs = .true.
            else if (operand_precedence == parent_precedence) then
                ! Right-associative operators (power) need parentheses on left side
                if (parent_op == OP_POWER .and. is_left_operand) then
                    needs = .true.
                ! Left-associative operators need parentheses on right side for different ops
                else if (.not. is_left_operand .and. operand%op /= parent_op) then
                    needs = .true.
                end if
            end if
        end select
    end function needs_parentheses

    ! Generate code for binary operations
    function generate_code_binary_op(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: left_code, right_code, op_str

        ! Generate left operand
        if (node%left > 0 .and. node%left <= arena%size) then
            left_code = generate_code_polymorphic_internal(arena, node%left)
            if (needs_parentheses(arena, node%left, node%op, .true.)) then
                left_code = "(" // left_code // ")"
            end if
        else
            left_code = ""
        end if

        ! Generate right operand
        if (node%right > 0 .and. node%right <= arena%size) then
            right_code = generate_code_polymorphic_internal(arena, node%right)
            if (needs_parentheses(arena, node%right, node%op, .false.)) then
                right_code = "(" // right_code // ")"
            end if
        else
            right_code = ""
        end if

        ! Get operator string
        select case (node%op)
        case (OP_PLUS);     op_str = " + "
        case (OP_MINUS);    op_str = " - "
        case (OP_MULTIPLY); op_str = " * "
        case (OP_DIVIDE);   op_str = " / "
        case (OP_POWER);    op_str = " ** "
        case (OP_CONCAT);   op_str = " // "
        case (OP_EQ);       op_str = " == "
        case (OP_NE);       op_str = " /= "
        case (OP_LT);       op_str = " < "
        case (OP_LE);       op_str = " <= "
        case (OP_GT);       op_str = " > "
        case (OP_GE);       op_str = " >= "
        case (OP_AND);      op_str = " .and. "
        case (OP_OR);       op_str = " .or. "
        case (OP_NOT);      op_str = ".not. "
        case (OP_EQV);      op_str = " .eqv. "
        case (OP_NEQV);     op_str = " .neqv. "
        case default;       op_str = " "
        end select

        ! Build expression
        if (node%op == OP_NOT) then
            code = op_str // right_code
        else
            code = left_code // op_str // right_code
        end if
    end function generate_code_binary_op

    ! Generate code for component access (%)
    function generate_code_component_access(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(component_access_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: obj_code

        ! Generate object code
        if (node%object > 0 .and. node%object <= arena%size) then
            obj_code = generate_code_polymorphic_internal(arena, node%object)
        else
            obj_code = ""
        end if

        ! Build component access
        if (allocated(node%component)) then
            code = obj_code // "%" // node%component
        else
            code = obj_code
        end if
    end function generate_code_component_access

    ! Generate code for range subscript (colon notation)
    function generate_code_range_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(range_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, stride_code

        ! Generate start index
        if (node%start_idx > 0 .and. node%start_idx <= arena%size) then
            start_code = generate_code_polymorphic_internal(arena, node%start_idx)
        else
            start_code = ""
        end if

        ! Generate end index  
        if (node%end_idx > 0 .and. node%end_idx <= arena%size) then
            end_code = generate_code_polymorphic_internal(arena, node%end_idx)
        else
            end_code = ""
        end if

        ! Generate stride
        if (node%stride > 0 .and. node%stride <= arena%size) then
            stride_code = generate_code_polymorphic_internal(arena, node%stride)
        else
            stride_code = ""
        end if

        ! Build range expression
        code = start_code // ":" // end_code
        if (len(stride_code) > 0) then
            code = code // ":" // stride_code
        end if
    end function generate_code_range_subscript

    ! Generate code for function calls or array subscripts
    function generate_code_call_or_subscript(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: target_code, args_code
        integer :: i

        ! Generate target (function name or array name)
        if (node%target > 0 .and. node%target <= arena%size) then
            target_code = generate_code_polymorphic_internal(arena, node%target)
        else if (allocated(node%name)) then
            target_code = node%name
        else
            target_code = ""
        end if

        ! Generate arguments
        args_code = ""
        if (allocated(node%args)) then
            do i = 1, size(node%args)
                if (i > 1) args_code = args_code // ", "
                if (node%args(i) > 0 .and. node%args(i) <= arena%size) then
                    args_code = args_code // generate_code_polymorphic_internal(arena, node%args(i))
                end if
            end do
        end if

        ! Build call/subscript
        code = target_code // "(" // args_code // ")"
    end function generate_code_call_or_subscript

    ! Generate code for array literals
    function generate_code_array_literal(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: elem_code
        integer :: i
        logical :: is_multiline
        character(len=:), allocatable :: indent_str

        ! Check if we should use multiline format
        is_multiline = .false.
        if (allocated(node%elements)) then
            ! Use multiline for large arrays or complex elements
            if (size(node%elements) > 5) then
                is_multiline = .true.
            else
                ! Check if any element is complex
                do i = 1, size(node%elements)
                    if (node%elements(i) > 0 .and. node%elements(i) <= arena%size) then
                        ! Check element type
                        select type (elem => arena%entries(node%elements(i))%node)
                        type is (literal_node)
                            ! Literals are simple
                        type is (do_loop_node)
                            ! Implied do loops force multiline
                            is_multiline = .true.
                            exit
                        class default
                            ! Other expressions might be complex
                            elem_code = generate_code_polymorphic_internal(arena, node%elements(i))
                            if (len(elem_code) > 20) then
                                is_multiline = .true.
                                exit
                            end if
                        end select
                    end if
                end do
            end if
        end if

        ! Generate array literal
        if (node%is_slash_form) then
            code = "/"
        else if (node%is_bracket_form) then
            code = "["
        else
            code = "(/"
        end if

        if (allocated(node%elements)) then
            if (is_multiline .and. size(node%elements) > 1) then
                ! Multiline format
                indent_str = "    "
                do i = 1, size(node%elements)
                    if (i == 1) then
                        code = code // " &" // new_line('A')
                    else
                        code = code // ", &" // new_line('A')
                    end if
                    code = code // indent_str
                    
                    if (node%elements(i) > 0 .and. node%elements(i) <= arena%size) then
                        select type (elem => arena%entries(node%elements(i))%node)
                        type is (do_loop_node)
                            ! Handle implied do loop
                            elem_code = generate_code_implied_do(arena, elem, node%elements(i))
                            code = code // elem_code
                        class default
                            elem_code = generate_code_polymorphic_internal(arena, node%elements(i))
                            code = code // elem_code
                        end select
                    end if
                end do
            else
                ! Single line format
                do i = 1, size(node%elements)
                    if (i > 1) code = code // ", "
                    
                    if (node%elements(i) > 0 .and. node%elements(i) <= arena%size) then
                        select type (elem => arena%entries(node%elements(i))%node)
                        type is (do_loop_node)
                            ! Handle implied do loop
                            elem_code = generate_code_implied_do(arena, elem, node%elements(i))
                            code = code // elem_code
                        class default
                            elem_code = generate_code_polymorphic_internal(arena, node%elements(i))
                            code = code // elem_code
                        end select
                    end if
                end do
            end if
        end if

        if (node%is_slash_form) then
            code = code // "/"
        else if (node%is_bracket_form) then
            code = code // "]"
        else
            code = code // "/)"
        end if
    end function generate_code_array_literal

    ! Generate code for implied do loops
    function generate_implied_do(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code, var_code
        character(len=:), allocatable :: start_code, end_code, step_code
        integer :: i

        ! Generate body expressions
        body_code = ""
        if (allocated(node%body)) then
            do i = 1, size(node%body)
                if (i > 1) body_code = body_code // ", "
                if (node%body(i) > 0) then
                    body_code = body_code // generate_code_polymorphic_internal(arena, node%body(i))
                end if
            end do
        end if

        ! Generate loop variable
        if (allocated(node%var_name)) then
            var_code = node%var_name
        else
            var_code = "i"
        end if

        ! Generate range
        start_code = generate_code_polymorphic_internal(arena, node%start_expr)
        end_code = generate_code_polymorphic_internal(arena, node%end_expr)
        if (node%step_expr > 0) then
            step_code = generate_code_polymorphic_internal(arena, node%step_expr)
            code = "(" // body_code // ", " // var_code // " = " // &
                   start_code // ", " // end_code // ", " // step_code // ")"
        else
            code = "(" // body_code // ", " // var_code // " = " // &
                   start_code // ", " // end_code // ")"
        end if
    end function generate_implied_do

    ! Generate code for range expressions
    function generate_code_range_expression(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(range_expression_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: start_code, end_code, stride_code

        ! Generate start
        if (node%start_expr > 0) then
            start_code = generate_code_polymorphic_internal(arena, node%start_expr)
        else
            start_code = ""
        end if

        ! Generate end
        if (node%end_expr > 0) then
            end_code = generate_code_polymorphic_internal(arena, node%end_expr)
        else
            end_code = ""
        end if

        ! Generate stride
        if (node%stride_expr > 0) then
            stride_code = generate_code_polymorphic_internal(arena, node%stride_expr)
        else
            stride_code = ""
        end if

        ! Build range
        code = start_code // ":" // end_code
        if (len(stride_code) > 0) then
            code = code // ":" // stride_code
        end if
    end function generate_code_range_expression

    ! Generate code for array bounds
    function generate_code_array_bounds(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_bounds_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: dim_code
        integer :: i

        code = "("
        if (allocated(node%dims)) then
            do i = 1, size(node%dims)
                if (i > 1) code = code // ", "
                
                ! Generate dimension bounds
                if (node%dims(i) > 0) then
                    dim_code = generate_code_polymorphic_internal(arena, node%dims(i))
                    code = code // dim_code
                else
                    ! Assumed size/shape
                    if (node%is_assumed_size .and. i == size(node%dims)) then
                        code = code // "*"
                    else if (node%is_assumed_shape) then
                        code = code // ":"
                    else if (node%is_deferred) then
                        code = code // ":"
                    else
                        code = code // "1"
                    end if
                end if
            end do
        end if
        code = code // ")"
    end function generate_code_array_bounds

    ! Generate code for array slices
    function generate_code_array_slice(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: array_code, indices_code
        integer :: i

        ! Generate array reference
        if (node%array_ref > 0) then
            array_code = generate_code_polymorphic_internal(arena, node%array_ref)
        else
            array_code = ""
        end if

        ! Generate indices
        indices_code = ""
        if (allocated(node%indices)) then
            do i = 1, size(node%indices)
                if (i > 1) indices_code = indices_code // ", "
                if (node%indices(i) > 0) then
                    indices_code = indices_code // generate_code_polymorphic_internal(arena, node%indices(i))
                else
                    indices_code = indices_code // ":"
                end if
            end do
        end if

        code = array_code // "(" // indices_code // ")"
    end function generate_code_array_slice

    ! Generate code for array operations
    function generate_code_array_operation(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_operation_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: op_name, args_code
        integer :: i

        ! Get operation name
        select case (node%op_type)
        case (ARRAY_OP_SIZE)
            op_name = "size"
        case (ARRAY_OP_SHAPE)
            op_name = "shape"
        case (ARRAY_OP_RESHAPE)
            op_name = "reshape"
        case (ARRAY_OP_TRANSPOSE)
            op_name = "transpose"
        case (ARRAY_OP_MATMUL)
            op_name = "matmul"
        case (ARRAY_OP_DOT_PRODUCT)
            op_name = "dot_product"
        case (ARRAY_OP_SUM)
            op_name = "sum"
        case (ARRAY_OP_PRODUCT)
            op_name = "product"
        case (ARRAY_OP_MAXVAL)
            op_name = "maxval"
        case (ARRAY_OP_MINVAL)
            op_name = "minval"
        case (ARRAY_OP_MAXLOC)
            op_name = "maxloc"
        case (ARRAY_OP_MINLOC)
            op_name = "minloc"
        case (ARRAY_OP_ANY)
            op_name = "any"
        case (ARRAY_OP_ALL)
            op_name = "all"
        case (ARRAY_OP_COUNT)
            op_name = "count"
        case (ARRAY_OP_MERGE)
            op_name = "merge"
        case (ARRAY_OP_SPREAD)
            op_name = "spread"
        case (ARRAY_OP_PACK)
            op_name = "pack"
        case (ARRAY_OP_UNPACK)
            op_name = "unpack"
        case (ARRAY_OP_CSHIFT)
            op_name = "cshift"
        case (ARRAY_OP_EOSHIFT)
            op_name = "eoshift"
        case default
            op_name = "unknown"
        end select

        ! Generate arguments
        args_code = ""
        if (allocated(node%operands)) then
            do i = 1, size(node%operands)
                if (i > 1) args_code = args_code // ", "
                if (node%operands(i) > 0) then
                    args_code = args_code // generate_code_polymorphic_internal(arena, node%operands(i))
                end if
            end do
        end if

        ! Add dimension if specified
        if (node%dim_arg > 0) then
            if (len(args_code) > 0) args_code = args_code // ", "
            args_code = args_code // "dim=" // generate_code_polymorphic_internal(arena, node%dim_arg)
        end if

        ! Add mask if specified
        if (node%mask_arg > 0) then
            if (len(args_code) > 0) args_code = args_code // ", "
            args_code = args_code // "mask=" // generate_code_polymorphic_internal(arena, node%mask_arg)
        end if

        code = op_name // "(" // args_code // ")"
    end function generate_code_array_operation

    ! Helper function to convert integer to string
    function int_to_string(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        write(buffer, '(I0)') num
        str = trim(buffer)
    end function int_to_string

    ! Internal polymorphic code generator (to avoid circular dependency)
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

end module codegen_expressions