module codegen_expressions
    use iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_data
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node, &
                                range_expression_node
    use ast_nodes_misc, only: complex_literal_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_io, only: io_implied_do_node
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
    public :: generate_code_complex_literal
    public :: generate_code_range_expression
    public :: generate_code_array_bounds
    public :: generate_code_array_slice
    public :: generate_code_array_operation
    public :: generate_code_io_implied_do
    public :: generate_code_implied_do
    public :: get_operator_precedence
    public :: needs_parentheses
    public :: get_node_operator

contains

    ! Remove spaces immediately before string literals only
    ! Fixes issue #2065 where space is inserted before string literals
    ! IMPORTANT: Preserves all content inside string literals including spaces
    function remove_spaces_from_string_literals(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, j, len_input
        character :: quote_char
        logical :: in_string, next_is_quote
        character(len=:), allocatable :: temp

        len_input = len(input)
        if (len_input == 0) then
            output = input
            return
        end if

        allocate(character(len=len_input) :: temp)

        in_string = .false.
        quote_char = ' '
        j = 0

        do i = 1, len_input
            ! Check if next character is a quote (to skip leading space)
            next_is_quote = .false.
            if (i < len_input .and. .not. in_string) then
                if (input(i+1:i+1) == "'" .or. input(i+1:i+1) == '"') then
                    next_is_quote = .true.
                end if
            end if

            if (.not. in_string) then
                ! Not in string
                if (input(i:i) == "'" .or. input(i:i) == '"') then
                    ! Start of string
                    j = j + 1
                    temp(j:j) = input(i:i)
                    in_string = .true.
                    quote_char = input(i:i)
                else if (input(i:i) == ' ' .and. next_is_quote) then
                    ! Skip space immediately before quote
                    continue
                else
                    ! Copy everything else
                    j = j + 1
                    temp(j:j) = input(i:i)
                end if
            else
                ! Inside string - preserve ALL characters including spaces
                if (input(i:i) == quote_char) then
                    ! End of string
                    j = j + 1
                    temp(j:j) = input(i:i)
                    in_string = .false.
                else
                    ! Copy all characters inside string including spaces
                    j = j + 1
                    temp(j:j) = input(i:i)
                end if
            end if
        end do

        output = temp(1:j)
    end function remove_spaces_from_string_literals

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
                ! Do NOT automatically promote real literals to double precision
                ! because it breaks generic interface resolution that depends on
                ! exact type matching (real vs real(8)). Users should explicitly
                ! write 5.0d0 or use kind parameters if they want double precision.
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

    ! Check if a node is a zero literal (used to detect unary minus as 0 - x)
    function is_zero_literal(arena, node_index) result(is_zero)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_zero

        is_zero = .false.
        if (node_index <= 0 .or. node_index > arena%compat_size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (n => arena%entries(node_index)%node)
        type is (literal_node)
            if (allocated(n%value)) then
                is_zero = (trim(n%value) == "0")
            end if
        end select
    end function is_zero_literal

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
        logical :: unary_minus

        ! Generate operands
        if (node%left_index > 0) then
            left_code = generate_code_from_arena(arena, node%left_index)
            left_code = remove_spaces_from_string_literals(left_code)
        else
            left_code = ""
        end if

        if (node%right_index > 0) then
            right_code = generate_code_from_arena(arena, node%right_index)
            ! WORKAROUND for issue #2065: Remove spurious spaces inside string literals
            ! caused by nested expression parsing
            right_code = remove_spaces_from_string_literals(right_code)
        else
            right_code = ""
        end if

        if (len_trim(left_code) == 0 .and. len_trim(right_code) == 0) then
            code = ""
            return
        end if

        ! Determine the correct Fortran operator
        if (allocated(node%operator)) then
            fortran_operator = node%operator

            ! Recover from missing operands in concatenation nodes (issue #1386)
            if (is_missing_concat_operand(trim(fortran_operator), .false., left_code, &
                                          right_code)) then
                code = right_code
                return
            end if
            if (is_missing_concat_operand(trim(fortran_operator), .true., left_code, &
                                          right_code)) then
                code = left_code
                return
            end if

            ! Check for string concatenation: if operator is '+' and we're dealing with string literals
            if (node%operator == "+" .and. is_string_concatenation(left_code, &
                                                                   right_code)) then
                fortran_operator = "//"  ! Use Fortran string concatenation operator
            end if

            unary_minus = (trim(fortran_operator) == "-") .and. &
                          is_zero_literal(arena, node%left_index)
            if (unary_minus) then
                right_paren = .false.
                right_op = get_node_operator(arena, node%right_index)
                if (len_trim(right_op) > 0) right_paren = &
                    needs_parentheses("-", trim(right_op), .false.)
                right_code = trim(adjustl(right_code))
                if (right_paren .and. len_trim(right_code) > 0) then
                    right_code = "(" // right_code // ")"
                end if
                code = "-" // right_code
                return
            end if

            ! Determine child operators to decide on parentheses
            left_op = get_node_operator(arena, node%left_index)
            right_op = get_node_operator(arena, node%right_index)

            left_paren = .false.
            right_paren = .false.
            if (len_trim(left_op) > 0) left_paren = &
                & needs_parentheses(trim(fortran_operator), trim(left_op), .true.)
            if (len_trim(right_op) > 0) right_paren = &
                & needs_parentheses(trim(fortran_operator), trim(right_op), .false.)

            if (left_paren) left_code = "(" // left_code // ")"
            if (right_paren) right_code = "(" // right_code // ")"

            if (trim(fortran_operator) == '.not.' .and. len_trim(left_code) == 0) then
                if (len_trim(right_code) > 0) then
                    code = trim(fortran_operator) // ' ' // right_code
                else
                    code = trim(fortran_operator)
                end if
                return
            end if

            select case (trim(fortran_operator))
            case ('*', '/', '**')
                ! Maintain compact form for high-precedence arithmetic
                code = left_code // fortran_operator // right_code
            case ('=')
                ! For keyword arguments in function calls, don't add space before string literals
                ! to prevent "file = '/tmp/test.dat'" from becoming "file = ' /tmp/ test.dat'"
                if (len(right_code) > 0) then
                    if (right_code(1:1) == "'" .or. right_code(1:1) == '"') then
                        ! Right side is a string literal - use compact form
                        code = left_code // " " // fortran_operator // right_code
                    else
                        ! Normal case with spaces around '='
                        code = left_code // " " // fortran_operator // " " // right_code
                    end if
                else
                    code = left_code // " " // fortran_operator // " " // right_code
                end if
            case default
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
        character(len=:), allocatable :: base_code

        ! Generate component access code (e.g., object%component)
        if (node%base_expr_index > 0 .and. node%base_expr_index <= arena%size) then
            base_code = generate_code_from_arena(arena, node%base_expr_index)
        else
            base_code = ""
        end if

        if (len_trim(base_code) > 0) then
            code = trim(adjustl(base_code)) // "%" // trim(node%component_name)
        else
            code = trim(node%component_name)
        end if
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
            base_code = trim(adjustl(base_code))
        else
            base_code = ""
        end if

        ! Start index (optional)
        if (node%start_index > 0) then
            start_code = generate_code_from_arena(arena, node%start_index)
            start_code = trim(adjustl(start_code))
        else
            start_code = ""
        end if

        ! End index (optional)
        if (node%end_index > 0) then
            end_code = generate_code_from_arena(arena, node%end_index)
            end_code = trim(adjustl(end_code))
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
        character(len=:), allocatable :: arg_code
        character(len=:), allocatable :: base_code
        integer :: i

        if (node%base_expr_index > 0 .and. &
            node%base_expr_index <= arena%size) then
            base_code = generate_code_from_arena(arena, node%base_expr_index)
            code = trim(adjustl(base_code))
        else if (allocated(node%name)) then
            code = trim(node%name)
        else
            code = "unknown"
        end if

        ! Generate arguments
        if (allocated(node%arg_indices)) then
            args_code = ""
            do i = 1, size(node%arg_indices)
                if (i > 1) args_code = args_code // ", "
                if (node%arg_indices(i) > 0) then
                    arg_code = generate_code_from_arena(arena, node%arg_indices(i))
                    args_code = args_code // trim(arg_code)
                end if
            end do
            code = code // "(" // args_code // ")"
        else
            code = code // "()"
        end if
    end function generate_code_call_or_subscript

    ! Helper function to generate comma-separated element code from indices
    function generate_elements_code_from_indices(arena, element_indices) &
        result(elements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: element_indices(:)
        character(len=:), allocatable :: elements_code
        character(len=:), allocatable :: element_code
        integer :: i

        elements_code = ""
        do i = 1, size(element_indices)
            if (i > 1) elements_code = elements_code // ", "
            if (element_indices(i) > 0) then
                element_code = generate_code_from_arena(arena, element_indices(i))
                elements_code = elements_code // trim(element_code)
            end if
        end do
    end function generate_elements_code_from_indices

    ! Generate code for real array elements.
    ! Integer literals must become default real so mixed constructors compile.
    function generate_real_elements_code(arena, element_indices) result(elements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: element_indices(:)
        character(len=:), allocatable :: elements_code
        character(len=:), allocatable :: elem_code
        integer :: i

        elements_code = ""
        do i = 1, size(element_indices)
            if (i > 1) elements_code = elements_code // ", "
            if (element_indices(i) <= 0) cycle
            elem_code = generate_code_from_arena(arena, element_indices(i))
            elem_code = trim(elem_code)

            select type (node => arena%entries(element_indices(i))%node)
            type is (literal_node)
                if (node%literal_kind == LITERAL_INTEGER) then
                    elem_code = promote_integer_literal_to_real(elem_code)
                end if
            end select

            elements_code = elements_code // elem_code
        end do
    end function generate_real_elements_code

    ! Generate code for character array elements.
    ! String literals must be padded to consistent target_len.
    function generate_char_elements_code(arena, element_indices, target_len) &
        result(elements_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: element_indices(:)
        integer, intent(in) :: target_len
        character(len=:), allocatable :: elements_code
        character(len=:), allocatable :: elem_code
        integer :: i, str_len, pad_len
        character(len=:), allocatable :: padding
        logical :: is_string_lit

        elements_code = ""
        do i = 1, size(element_indices)
            if (i > 1) elements_code = elements_code // ", "
            if (element_indices(i) <= 0) cycle
            elem_code = generate_code_from_arena(arena, element_indices(i))
            elem_code = trim(elem_code)

            ! Check if this is a string literal that needs padding
            is_string_lit = .false.
            if (len(elem_code) >= 2) then
                if (elem_code(1:1) == '"' .or. elem_code(1:1) == "'") then
                    is_string_lit = .true.
                end if
            end if

            if (is_string_lit) then
                str_len = len(elem_code) - 2
                if (str_len < target_len) then
                    pad_len = target_len - str_len
                    allocate (character(len=pad_len) :: padding)
                    padding = repeat(" ", pad_len)
                    elem_code = elem_code(1:len(elem_code) - 1) // &
                                padding // elem_code(len(elem_code):len(elem_code))
                    deallocate (padding)
                end if
            end if

            elements_code = elements_code // elem_code
        end do
    end function generate_char_elements_code

    ! Generate code for array literals
    function nested_array_shape(arena, node, num_rows, num_cols) result(is_rectangular)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(out) :: num_rows
        integer, intent(out) :: num_cols
        logical :: is_rectangular
        integer :: i, j, row_index, current_cols, elem_index

        is_rectangular = .false.
        num_rows = 0
        num_cols = 0

        if (.not. allocated(node%element_indices)) return
        num_rows = size(node%element_indices)
        if (num_rows == 0) return

        do i = 1, num_rows
            row_index = node%element_indices(i)
            if (row_index <= 0 .or. row_index > arena%size) return

            select type (row => arena%entries(row_index)%node)
            type is (array_literal_node)
                if (.not. allocated(row%element_indices)) return
                current_cols = size(row%element_indices)
                if (current_cols == 0) return

                do j = 1, current_cols
                    elem_index = row%element_indices(j)
                    if (elem_index <= 0 .or. elem_index > arena%size) return
                    select type (inner => arena%entries(elem_index)%node)
                    type is (array_literal_node)
                        return
                    end select
                end do
            class default
                return
            end select

            if (i == 1) then
                num_cols = current_cols
            else
                if (current_cols /= num_cols) return
            end if
        end do

        is_rectangular = (num_cols > 0)
    end function nested_array_shape

    function generate_reshape_from_nested(arena, node, num_rows, num_cols) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(in) :: num_rows
        integer, intent(in) :: num_cols
        character(len=:), allocatable :: code
        character(len=:), allocatable :: flat_elements
        character(len=:), allocatable :: elem_code
        integer :: row_idx, col_idx, row_index, elem_index
        logical :: first_element

        flat_elements = ""
        first_element = .true.

        if (.not. allocated(node%element_indices)) then
            code = "[integer ::]"
            return
        end if

        do row_idx = 1, num_rows
            row_index = node%element_indices(row_idx)
            if (row_index <= 0 .or. row_index > arena%size) cycle

            select type (row => arena%entries(row_index)%node)
            type is (array_literal_node)
                if (.not. allocated(row%element_indices)) cycle
                do col_idx = 1, num_cols
                    if (col_idx > size(row%element_indices)) cycle
                    elem_index = row%element_indices(col_idx)
                    if (elem_index <= 0 .or. elem_index > arena%size) cycle

                    elem_code = generate_code_from_arena(arena, elem_index)
                    elem_code = trim(adjustl(elem_code))
                    if (len_trim(elem_code) == 0) cycle

                    if (first_element) then
                        flat_elements = elem_code
                        first_element = .false.
                    else
                        flat_elements = flat_elements // ", " // elem_code
                    end if
                end do
            end select
        end do

        if (first_element) then
            code = "[integer ::]"
            return
        end if

        code = "reshape([" // flat_elements // "], [" // &
               trim(adjustl(int_to_string(num_rows))) // ", " // &
               trim(adjustl(int_to_string(num_cols))) // "], order=[2, 1])"
    end function generate_reshape_from_nested

    function int_to_string(val) result(str)
        integer, intent(in) :: val
        character(len=:), allocatable :: str
        character(len=20) :: buffer
        write (buffer, '(I0)') val
        str = trim(adjustl(buffer))
    end function int_to_string

    function generate_code_array_literal(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: elements_code
        type(mono_type_t) :: array_type
        logical :: is_real_array
        integer :: nested_rows, nested_cols

        if (allocated(node%syntax_style) .and. node%syntax_style == "modern") then
            if (nested_array_shape(arena, node, nested_rows, nested_cols)) then
                code = generate_reshape_from_nested(arena, node, nested_rows, &
                    & nested_cols)
                return
            end if
        end if

        ! Check array type to handle type-specific code generation
        is_real_array = .false.
        if (node%inferred_type%kind == TARRAY) then
            if (node%inferred_type%has_args() .and. &
                node%inferred_type%get_args_count() > 0) then
                array_type = node%inferred_type%get_arg(1)
                is_real_array = (array_type%kind == TREAL)
            else
                ! If no type info, check if any element is real
                if (allocated(node%element_indices)) then
                    block
                        integer :: j
                        do j = 1, size(node%element_indices)
                            if (node%element_indices(j) > 0) then
                                associate (entry => &
                                           arena%entries(node%element_indices(j)))
                                    select type (elem_node => entry%node)
                                    type is (literal_node)
                                        if (elem_node%literal_kind == &
                                            LITERAL_REAL) then
                                            is_real_array = .true.
                                            exit
                                        end if
                                    end select
                                end associate
                            end if
                        end do
                    end block
                end if
            end if
        end if

        ! Generate elements code based on array type
        if (allocated(node%element_indices) .and. size(node%element_indices) > 0) then
            if (is_real_array) then
                elements_code = generate_real_elements_code(arena, &
                                                            node%element_indices)
            else if (node%inferred_type%kind == TARRAY .and. &
                     node%inferred_type%has_args() .and. &
                     node%inferred_type%get_args_count() > 0) then
                array_type = node%inferred_type%get_arg(1)
                if (array_type%kind == TCHAR .and. array_type%size > 0) then
                    elements_code = generate_char_elements_code( &
                                    arena, node%element_indices, array_type%size)
                else
                    elements_code = generate_elements_code_from_indices(arena, &
                        & node%element_indices)
                end if
            else
                ! Fallback: check if elements are character literals
                block
                    integer :: max_len_fallback, j, str_len_val
                    logical :: all_char
                    max_len_fallback = 0
                    all_char = .true.
                    do j = 1, size(node%element_indices)
                        if (node%element_indices(j) <= 0) cycle
                        associate (entry => arena%entries(node%element_indices(j)))
                            if (.not. allocated(entry%node)) then
                                all_char = .false.
                                exit
                            end if
                            select type (elem => entry%node)
                            type is (literal_node)
                                if (allocated(elem%value) .and. &
                                    len(elem%value) >= 2) then
                                    str_len_val = len(elem%value) - 2
                                    max_len_fallback = max(max_len_fallback, &
                                                           str_len_val)
                                else
                                    all_char = .false.
                                    exit
                                end if
                            class default
                                all_char = .false.
                                exit
                            end select
                        end associate
                    end do
                    if (all_char .and. max_len_fallback > 0) then
                        elements_code = generate_char_elements_code( &
                                        arena, node%element_indices, max_len_fallback)
                    else
                        elements_code = generate_elements_code_from_indices(arena, &
                            & node%element_indices)
                    end if
                end block
            end if
        else
            elements_code = ""
        end if

        ! Handle different syntax styles
        if (allocated(node%syntax_style)) then
            if (node%syntax_style == "modern") then
                ! Modern syntax: [1, 2, 3]
                if (allocated(node%element_indices) .and. &
                    size(node%element_indices) > 0) then
                    code = "[" // elements_code // "]"
                else
                    ! Empty array - needs type spec, default to integer
                    code = "[integer ::]"
                end if
            else if (node%syntax_style == "implied_do") then
                ! Implied do loop syntax: generate actual implied do loop
                if (allocated(node%element_indices) .and. &
                    size(node%element_indices) > 0) then
                    ! The element should be a do loop node
                    if (allocated(node%type_spec)) then
                        code = generate_implied_do_array(arena, &
                                                         node%element_indices(1), &
                                                         node%type_spec)
                    else
                        code = generate_implied_do_array(arena, &
                                                         node%element_indices(1))
                    end if
                else
                    ! Empty array - needs type spec, default to integer
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: /)"
                    else
                        code = "[integer ::]"
                    end if
                end if
            else
                ! Legacy syntax: (/ 1, 2, 3 /)
                if (allocated(node%element_indices) .and. &
                    size(node%element_indices) > 0) then
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: " // &
                               trim(elements_code) // " /)"
                    else
                        code = "(/ " // elements_code // " /)"
                    end if
                else
                    ! Empty array - needs type spec, default to integer
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: /)"
                    else
                        code = "[integer ::]"
                    end if
                end if
            end if
        else
            ! Default to legacy syntax
            if (allocated(node%element_indices) .and. &
                size(node%element_indices) > 0) then
                if (allocated(node%type_spec)) then
                    code = "(/ " // trim(node%type_spec) // " :: " // &
                           trim(elements_code) // " /)"
                else
                    code = "(/ " // elements_code // " /)"
                end if
            else
                if (allocated(node%type_spec)) then
                    code = "(/ " // trim(node%type_spec) // " :: /)"
                else
                    code = "[integer ::]"  ! Empty array constructor with type specification
                end if
            end if
        end if
    end function generate_code_array_literal

    ! Promote integer literals to default real form and keep explicit kinds intact.
    function promote_integer_literal_to_real(code) result(promoted)
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: promoted
        character(len=:), allocatable :: trimmed

        trimmed = trim(code)
        promoted = trimmed

        if (len_trim(promoted) == 0) return

        ! Respect existing explicit kind suffixes and exponential forms
        if (index(promoted, '_') /= 0) then
            promoted = "real(" // promoted // ")"
            return
        end if
        if (index(promoted, '.') /= 0) return
        if (index(promoted, 'e') /= 0 .or. index(promoted, 'E') /= 0) return
        if (index(promoted, 'd') /= 0 .or. index(promoted, 'D') /= 0) return

        promoted = promoted // ".0"
    end function promote_integer_literal_to_real

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
            if (range_node%start_index > 0 .and. range_node%start_index <= &
                arena%size) then
                start_code = generate_code_from_arena(arena, range_node%start_index)
                start_code = trim(adjustl(start_code))
            else
                start_code = ""  ! Implicit start (e.g., :5)
            end if

            ! Generate end expression
            if (range_node%end_index > 0 .and. range_node%end_index <= arena%size) then
                end_code = generate_code_from_arena(arena, range_node%end_index)
                end_code = trim(adjustl(end_code))
            else
                end_code = ""  ! Implicit end (e.g., 2:)
            end if

            ! Generate stride expression (optional)
            if (range_node%stride_index > 0 .and. range_node%stride_index <= &
                arena%size) then
                stride_code = generate_code_from_arena(arena, range_node%stride_index)
                stride_code = trim(adjustl(stride_code))
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
        character(len=:), allocatable :: lower_code, upper_code, stride_code
        logical :: has_lower, has_upper, has_stride

        select type (bounds_node => node)
        type is (array_bounds_node)
            ! Assumed-size takes precedence over other flags
            if (bounds_node%is_assumed_size) then
                code = "*"
                return
            end if

            ! Fast-path for assumed/deferred shape with no explicit bounds
            if ((bounds_node%is_assumed_shape .or. &
                 bounds_node%is_deferred_shape) .and. &
                bounds_node%lower_bound_index <= 0 .and. &
                bounds_node%upper_bound_index &
                <= 0 .and. &
                bounds_node%stride_index <= 0) then
                code = ":"
                return
            end if

            ! Lower bound (optional)
            if (bounds_node%lower_bound_index > 0 .and. &
                bounds_node%lower_bound_index <= &
                & arena%size) then
                lower_code = trim(adjustl(generate_code_from_arena( &
                                          arena, bounds_node%lower_bound_index)))
                has_lower = len_trim(lower_code) > 0
            else
                lower_code = ""
                has_lower = .false.
            end if

            ! Upper bound (optional)
            if (bounds_node%upper_bound_index > 0 .and. &
                bounds_node%upper_bound_index <= &
                & arena%size) then
                upper_code = trim(adjustl(generate_code_from_arena( &
                                          arena, bounds_node%upper_bound_index)))
                has_upper = len_trim(upper_code) > 0
            else
                upper_code = ""
                has_upper = .false.
            end if

            ! Stride (optional)
            if (bounds_node%stride_index > 0 .and. bounds_node%stride_index <= &
                arena%size) then
                stride_code = trim(adjustl(generate_code_from_arena( &
                                           arena, bounds_node%stride_index)))
                has_stride = len_trim(stride_code) > 0
            else
                stride_code = ""
                has_stride = .false.
            end if

            ! Compose the slice text
            if (.not. has_lower .and. .not. has_upper) then
                code = ":"
            else
                code = lower_code // ":" // upper_code
            end if

            if (has_stride) code = code // ":" // stride_code
        class default
            code = ":"
        end select
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
            if (slice_node%array_index > 0 .and. slice_node%array_index <= &
                arena%size) then
                array_code = generate_code_from_arena(arena, slice_node%array_index)
                array_code = trim(adjustl(array_code))
            else
                array_code = "unknown_array"
            end if

            ! Generate the bounds part (e.g., '1:3' in name(1:3))
            bounds_code = ""
            do i = 1, slice_node%num_dimensions
                if (i > 1) bounds_code = bounds_code // ", "
                if (slice_node%bounds_indices(i) > 0 .and. &
                    slice_node%bounds_indices(i) <= arena%size) then
                    bounds_code = bounds_code // &
                        & trim(adjustl(generate_code_from_arena(arena, &
                        & slice_node%bounds_indices(i))))
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

    ! Generate inner implied do without outer (/ /) wrapper
    recursive function generate_implied_do_inner(arena, do_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: do_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, start_code, end_code, step_code

        select type (node => arena%entries(do_index)%node)
        type is (do_loop_node)
            if (allocated(node%body_indices) .and. size(node%body_indices) > 0) then
                if (node%body_indices(1) > 0 .and. &
                    node%body_indices(1) <= arena%size) then
                    if (allocated(arena%entries(node%body_indices(1))%node)) then
                        select type (body_node => &
                                     arena%entries(node%body_indices(1))%node)
                        type is (do_loop_node)
                            expr_code = generate_implied_do_inner(arena, &
                                                                  node%body_indices(1))
                        class default
                            expr_code = generate_code_from_arena(arena, &
                                                                 node%body_indices(1))
                        end select
                        expr_code = trim(expr_code)
                    else
                        expr_code = "0"
                    end if
                else
                    expr_code = "0"
                end if
            else
                expr_code = "0"
            end if

            if (node%start_expr_index > 0) then
                start_code = generate_code_from_arena(arena, node%start_expr_index)
                start_code = trim(start_code)
            else
                start_code = "1"
            end if

            if (node%end_expr_index > 0) then
                end_code = generate_code_from_arena(arena, node%end_expr_index)
                end_code = trim(end_code)
            else
                end_code = "n"
            end if

            if (node%step_expr_index > 0) then
                step_code = generate_code_from_arena(arena, node%step_expr_index)
                step_code = trim(step_code)
                if (allocated(node%var_name)) then
                    code = "(" // expr_code // ", " // node%var_name // "=" // &
                           start_code // ", " // end_code // ", " // &
                           step_code // ")"
                else
                    code = "(" // expr_code // ", i=" // &
                           start_code // ", " // end_code // ", " // &
                           step_code // ")"
                end if
            else
                if (allocated(node%var_name)) then
                    code = "(" // expr_code // ", " // node%var_name // "=" // &
                           start_code // ", " // end_code // ")"
                else
                    code = "(" // expr_code // ", i=" // &
                           start_code // ", " // end_code // ")"
                end if
            end if
        class default
            code = "0"
        end select
    end function generate_implied_do_inner

    ! Generate implied do array constructor from do loop node
    function generate_implied_do_array(arena, do_index, type_spec) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: do_index
        character(len=*), intent(in), optional :: type_spec
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code, start_code, end_code, step_code
        character(len=:), allocatable :: inner

        ! Get the do loop node
        select type (node => arena%entries(do_index)%node)
        type is (do_loop_node)
            ! Generate the expression (body)
            if (allocated(node%body_indices) .and. size(node%body_indices) > 0) then
                if (node%body_indices(1) > 0 .and. &
                    node%body_indices(1) <= arena%size) then
                    if (allocated(arena%entries(node%body_indices(1))%node)) then
                        select type (body_node => &
                                     arena%entries(node%body_indices(1))%node)
                        type is (do_loop_node)
                            expr_code = generate_implied_do_inner(arena, &
                                                                  node%body_indices(1))
                            expr_code = trim(expr_code)
                        class default
                            expr_code = generate_code_from_arena(arena, &
                                                                 node%body_indices(1))
                            expr_code = trim(expr_code)
                        end select
                    else
                        expr_code = "0"
                    end if
                else
                    expr_code = "0"
                end if
            else
                expr_code = "0"  ! Fallback
            end if

            ! Generate start expression
            if (node%start_expr_index > 0) then
                start_code = generate_code_from_arena(arena, node%start_expr_index)
                start_code = trim(start_code)
            else
                start_code = "1"
            end if

            ! Generate end expression
            if (node%end_expr_index > 0) then
                end_code = generate_code_from_arena(arena, node%end_expr_index)
                end_code = trim(end_code)
            else
                end_code = "n"
            end if

            ! Generate step expression if present
            ! Use legacy (/ /) syntax for compatibility
            if (node%step_expr_index > 0) then
                step_code = generate_code_from_arena(arena, node%step_expr_index)
                step_code = trim(step_code)
                if (allocated(node%var_name)) then
                    inner = "(" // expr_code // ", " // node%var_name // "=" // &
                            start_code // ", " // end_code // ", " // step_code // ")"
                else
                    inner = "(" // expr_code // ", i=" // start_code // ", " // &
                            end_code // ", " // step_code // ")"
                end if
            else
                if (allocated(node%var_name)) then
                    inner = "(" // expr_code // ", " // node%var_name // "=" // &
                            start_code // ", " // end_code // ")"
                else
                    inner = "(" // expr_code // ", i=" // start_code // ", " // &
                            end_code // ")"
                end if
            end if

            if (present(type_spec)) then
                if (len_trim(type_spec) > 0) then
                    code = "(/ " // trim(type_spec) // " :: " // trim(inner) // " /)"
                else
                    code = "(/ " // trim(inner) // " /)"
                end if
            else
                code = "(/ " // trim(inner) // " /)"
            end if
        class default
            ! Not a do loop node - fallback
            ! Empty array needs type specification
            code = "[integer ::]"
        end select
    end function generate_implied_do_array

    function generate_code_io_implied_do(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(io_implied_do_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: expr_code
        character(len=:), allocatable :: start_code, end_code, step_code
        character(len=:), allocatable :: elem_code
        integer :: obj_idx

        expr_code = ""
        if (allocated(node%object_indices)) then
            do obj_idx = 1, size(node%object_indices)
                if (node%object_indices(obj_idx) <= 0) cycle
                elem_code = generate_code_from_arena(arena, &
                                                     node%object_indices(obj_idx))
                elem_code = trim(elem_code)
                if (len_trim(elem_code) == 0) cycle
                if (len_trim(expr_code) == 0) then
                    expr_code = elem_code
                else
                    expr_code = trim(expr_code) // ", " // elem_code
                end if
            end do
        else if (node%expr_index > 0) then
            expr_code = generate_code_from_arena(arena, node%expr_index)
            expr_code = trim(expr_code)
        end if

        if (node%start_expr_index > 0) then
            start_code = generate_code_from_arena(arena, node%start_expr_index)
            start_code = trim(start_code)
        else
            start_code = "1"
        end if

        if (node%end_expr_index > 0) then
            end_code = generate_code_from_arena(arena, node%end_expr_index)
            end_code = trim(end_code)
        else
            end_code = "1"
        end if

        if (node%step_expr_index > 0) then
            step_code = generate_code_from_arena(arena, node%step_expr_index)
            step_code = trim(step_code)
            code = "(" // trim(expr_code) // ", " // trim(node%var_name) // " = " // &
                   trim(start_code) // ", " // trim(end_code) // ", " // &
                   trim(step_code) // ")"
        else
            code = "(" // trim(expr_code) // ", " // trim(node%var_name) // " = " // &
                   trim(start_code) // ", " // trim(end_code) // ")"
        end if
    end function generate_code_io_implied_do

    ! Get operator precedence (higher number = higher precedence)
    function get_operator_precedence(op) result(precedence)
        character(len=*), intent(in) :: op
        integer :: precedence

        select case (trim(op))
        case ('**')
            precedence = 10
        case ('*', '/')
            precedence = 8
        case ('+', '-')
            precedence = 7
        case ('//')
            precedence = 6
            ! Relational operators (map syntactic variants to same precedence)
        case ('.lt.', '.le.', '.gt.', '.ge.', '.eq.', '.ne.', '<', '<=', '>', '>=', &
              '==', '/=')
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
            precedence = 0
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

    ! Check if we have string concatenation (both operands are string literals)
    pure function is_string_concatenation(left_code, right_code) result(is_string)
        character(len=*), intent(in) :: left_code, right_code
        logical :: is_string

        ! Check if both operands are string literals (enclosed in single or double quotes)
        is_string = is_string_literal(left_code) .and. is_string_literal(right_code)
    end function is_string_concatenation

    ! Check if a code fragment is a string literal
    pure function is_string_literal(code) result(is_string)
        character(len=*), intent(in) :: code
        logical :: is_string
        character(len=:), allocatable :: trimmed_code

        ! Trim whitespace
        trimmed_code = trim(adjustl(code))

        ! Check if it starts and ends with quotes
        is_string = .false.
        if (len(trimmed_code) >= 2) then
            ! Check for single quotes
            if (trimmed_code(1:1) == "'" .and. &
                & trimmed_code(len(trimmed_code):len(trimmed_code)) == "'") then
                is_string = .true.
                ! Check for double quotes
            else if (trimmed_code(1:1) == '"' .and. &
                & trimmed_code(len(trimmed_code):len(trimmed_code)) == '"') then
                is_string = .true.
            end if
        end if
    end function is_string_literal

    pure logical function is_missing_concat_operand(operator, checking_left, &
                                                    left_code, &
                                                    right_code) result(is_missing)
        character(len=*), intent(in) :: operator
        logical, intent(in) :: checking_left
        character(len=*), intent(in) :: left_code
        character(len=*), intent(in) :: right_code
        logical :: is_concat_op

        is_concat_op = (operator == '//') .or. &
                       (operator == '+' .and. (is_string_literal(left_code) .or. &
                                               is_string_literal(right_code)))

        if (.not. is_concat_op) then
            is_missing = .false.
            return
        end if

        if (checking_left) then
            is_missing = len_trim(right_code) == 0
        else
            is_missing = len_trim(left_code) == 0
        end if
    end function is_missing_concat_operand

    ! generate_code_from_arena is provided as an interface at the module level

end module codegen_expressions
