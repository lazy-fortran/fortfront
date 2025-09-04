module codegen_expressions
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_core
    use ast_nodes_data
    use ast_nodes_bounds, only: array_slice_node, range_expression_node
    use ast_nodes_misc, only: complex_literal_node
    use ast_nodes_loops, only: do_loop_node
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use codegen_type_utils, only: get_type_standardization
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
    public :: generate_code_complex_literal
    public :: generate_code_range_expression
    public :: generate_code_array_bounds
    public :: generate_code_array_slice
    public :: generate_code_array_operation
    public :: generate_code_implied_do
    public :: get_operator_precedence
    public :: needs_parentheses
    public :: get_node_operator
    public :: int_to_string

contains

    ! Generate code for literal nodes
    function generate_code_literal(node) result(code)
        type(literal_node), intent(in) :: node
        character(len=:), allocatable :: code
        logical :: standardize_types_enabled

        ! Generate literal value - handle missing values gracefully
        if (allocated(node%value)) then
            ! Transform non-Fortran boolean literals to Fortran format
            if (node%value == "true") then
                code = ".true."
            else if (node%value == "false") then
                code = ".false."
            else
                code = node%value
                ! If this is a real literal and type standardization is enabled,
                ! emit double precision literal with d0 suffix (e.g., 3.14d0)
                call get_type_standardization(standardize_types_enabled)
                if (standardize_types_enabled) then
                    if (node%literal_kind == LITERAL_REAL) then
                        block
                            character(len=:), allocatable :: lc
                            lc = code
                            ! Only append if not already having exponent/kind/d-suffix
                            if (index(lc, 'e') == 0 .and. index(lc, 'E') == 0 .and. &
                                index(lc, 'd') == 0 .and. index(lc, 'D') == 0) then
                                if (index(lc, '.') > 0) then
                                    code = trim(lc) // 'd0'
                                end if
                            end if
                        end block
                    end if
                end if
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
        character(len=:), allocatable :: left_op, right_op
        logical :: left_paren, right_paren

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

            ! Determine child operators to decide on parentheses
            left_op = get_node_operator(arena, node%left_index)
            right_op = get_node_operator(arena, node%right_index)
            
            left_paren = .false.
            right_paren = .false.
            if (len_trim(left_op) > 0) left_paren = needs_parentheses(trim(fortran_operator), trim(left_op), .true.)
            if (len_trim(right_op) > 0) right_paren = needs_parentheses(trim(fortran_operator), trim(right_op), .false.)

            if (left_paren) left_code = "(" // left_code // ")"
            if (right_paren) right_code = "(" // right_code // ")"

            select case (trim(fortran_operator))
            case ('*','/','**')
                ! For multiplication, division, and exponentiation, no spaces per style/tests
                code = left_code // fortran_operator // right_code
            case default
                ! For all other operators, include spaces around
                code = left_code // " " // fortran_operator // " " // right_code
            end select
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

        character(len=:), allocatable :: base_code, start_code, end_code

        ! Base expression (e.g., array or variable name)
        if (node%base_expr_index > 0) then
            base_code = generate_code_from_arena(arena, node%base_expr_index)
        else
            base_code = ""
        end if

        ! Start index (optional)
        if (node%start_index > 0) then
            start_code = generate_code_from_arena(arena, node%start_index)
        else
            start_code = ""
        end if

        ! End index (optional)
        if (node%end_index > 0) then
            end_code = generate_code_from_arena(arena, node%end_index)
        else
            end_code = ""
        end if

        ! Assemble range: base(start:end) with optional bounds
        code = base_code // "("
        if (len(start_code) > 0) code = code // start_code
        code = code // ":"
        if (len(end_code) > 0) code = code // end_code
        code = code // ")"
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

    ! Helper function to generate comma-separated element code from indices
    function generate_elements_code_from_indices(arena, element_indices) result(elements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: element_indices(:)
        character(len=:), allocatable :: elements_code
        integer :: i
        
        elements_code = ""
        do i = 1, size(element_indices)
            if (i > 1) elements_code = elements_code // ", "
            if (element_indices(i) > 0) then
                elements_code = elements_code // &
                    generate_code_from_arena(arena, element_indices(i))
            end if
        end do
    end function generate_elements_code_from_indices

    ! Generate code for array literals
    function generate_code_array_literal(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: elements_code
        
        ! Handle different syntax styles
        if (allocated(node%syntax_style)) then
            if (node%syntax_style == "modern") then
                ! Modern syntax: [1, 2, 3]
                if (allocated(node%element_indices)) then
                    elements_code = generate_elements_code_from_indices(arena, node%element_indices)
                    code = "[" // elements_code // "]"
                else
                    code = "[]"  ! Empty array
                end if
            else if (node%syntax_style == "implied_do") then
                ! Implied do loop syntax: generate actual implied do loop
                if (allocated(node%element_indices) .and. size(node%element_indices) > 0) then
                    ! The element should be a do loop node
                    code = generate_implied_do_array(arena, node%element_indices(1))
                else
                    code = "[]"  ! Fallback
                end if
            else
                ! Legacy syntax: (/ 1, 2, 3 /)
                if (allocated(node%element_indices)) then
                    elements_code = generate_elements_code_from_indices(arena, node%element_indices)
                    code = "(/ " // elements_code // " /)"
                else
                    code = "(/ /)"  ! Empty array
                end if
            end if
        else
            ! Default to legacy syntax
            if (allocated(node%element_indices)) then
                elements_code = generate_elements_code_from_indices(arena, node%element_indices)
                code = "(/ " // elements_code // " /)"
            else
                code = "(/ /)"  ! Empty array
            end if
        end if
    end function generate_code_array_literal

    ! Generate code for complex literal nodes (e.g., (1.0, 2.0))
    function generate_code_complex_literal(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(complex_literal_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: real_part, imag_part
        
        ! Initialize to empty complex literal if indices are invalid
        code = "(0.0, 0.0)"
        
        ! Generate real part
        if (node%real_index > 0 .and. node%real_index <= arena%size) then
            real_part = generate_code_from_arena(arena, node%real_index)
        else
            real_part = "0.0"
        end if
        
        ! Generate imaginary part
        if (node%imag_index > 0 .and. node%imag_index <= arena%size) then
            imag_part = generate_code_from_arena(arena, node%imag_index)
        else
            imag_part = "0.0"
        end if
        
        ! Construct complex literal
        code = "(" // real_part // ", " // imag_part // ")"
        
    end function generate_code_complex_literal

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
    
    ! Generate implied do array constructor from do loop node
    function generate_implied_do_array(arena, do_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: do_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, start_code, end_code, step_code
        
        ! Get the do loop node
        select type (node => arena%entries(do_index)%node)
        type is (do_loop_node)
            ! Generate the expression (body)
            if (allocated(node%body_indices) .and. size(node%body_indices) > 0) then
                expr_code = generate_code_from_arena(arena, node%body_indices(1))
            else
                expr_code = "0"  ! Fallback
            end if
            
            ! Generate start expression
            if (node%start_expr_index > 0) then
                start_code = generate_code_from_arena(arena, node%start_expr_index)
            else
                start_code = "1"
            end if
            
            ! Generate end expression
            if (node%end_expr_index > 0) then
                end_code = generate_code_from_arena(arena, node%end_expr_index)
            else
                end_code = "n"
            end if
            
            ! Generate step expression if present
            ! Use legacy syntax (/ /) for implied do to work with implicit typing
            if (node%step_expr_index > 0) then
                step_code = generate_code_from_arena(arena, node%step_expr_index)
                if (allocated(node%var_name)) then
                    code = "(/ (" // expr_code // ", " // node%var_name // "=" // &
                           start_code // ", " // end_code // ", " // step_code // ") /)"
                else
                    code = "(/ (" // expr_code // ", i=" // &
                           start_code // ", " // end_code // ", " // step_code // ") /)"
                end if
            else
                if (allocated(node%var_name)) then
                    code = "(/ (" // expr_code // ", " // node%var_name // "=" // &
                           start_code // ", " // end_code // ") /)"
                else
                    code = "(/ (" // expr_code // ", i=" // &
                           start_code // ", " // end_code // ") /)"
                end if
            end if
        class default
            ! Not a do loop node - fallback
            code = "[]"
        end select
    end function generate_implied_do_array

    ! Get operator precedence (higher number = higher precedence)
    function get_operator_precedence(op) result(precedence)
        character(len=*), intent(in) :: op
        integer :: precedence

        select case (trim(op))
        case ('**')
            precedence = 9
        case ('*','/')
            precedence = 8
        case ('+','-')
            precedence = 7
        case ('//')
            precedence = 6
        ! Relational operators (map syntactic variants to same precedence)
        case ('.lt.', '.le.', '.gt.', '.ge.', '.eq.', '.ne.', '<', '<=', '>', '>=', '==', '/=')
            precedence = 5
        ! Logical NOT (unary) binds tighter than AND/OR
        case ('.not.')
            precedence = 4
        case ('.and.')
            precedence = 3
        case ('.or.')
            precedence = 2
        case ('.eqv.', '.neqv.')
            precedence = 1
        case default
            precedence = 1
        end select
    end function get_operator_precedence

    ! Check if parentheses are needed
    function needs_parentheses(parent_op, child_op, is_left) result(needs_parens)
        character(len=*), intent(in) :: parent_op, child_op
        logical, intent(in) :: is_left
        logical :: needs_parens

        integer :: p_prec, c_prec
        p_prec = get_operator_precedence(parent_op)
        c_prec = get_operator_precedence(child_op)

        if (c_prec < p_prec) then
            needs_parens = .true.
            return
        end if

        if (c_prec > p_prec) then
            needs_parens = .false.
            return
        end if

        ! Equal precedence: handle associativity and non-associative cases
        select case (trim(parent_op))
        case ('**')
            ! Exponentiation is right-associative.
            ! Parentheses needed for left child when it is also '**' to preserve explicit left-grouping.
            if (is_left .and. trim(child_op) == '**') then
                needs_parens = .true.
            else
                needs_parens = .false.
            end if
        case ('-')
            ! Subtraction is left-associative but not associative.
            ! Be conservative: for right child at equal precedence, add parentheses.
            needs_parens = .not. is_left
        case ('/')
            ! Division is left-associative but not associative.
            ! Be conservative: for right child at equal precedence, add parentheses.
            needs_parens = .not. is_left
        case default
            ! Most others are left-associative: no parentheses needed for equal precedence
            needs_parens = .false.
        end select
    end function needs_parentheses

    ! Get operator for a node index if it's a binary operation; empty otherwise
    function get_node_operator(arena, node_index) result(op)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: op

        op = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (n => arena%entries(node_index)%node)
        type is (binary_op_node)
            if (allocated(n%operator)) then
                op = n%operator
            end if
        class default
            op = ""
        end select
    end function get_node_operator

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
