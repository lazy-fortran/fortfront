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

    ! Validate that operands of logical operators are TLOGICAL type
    ! ISO/IEC 1539-1:2018 Section 10.1.5.2: Logical operators require LOGICAL operands
    ! Fixes issue #2457: Detect and report type mismatches in logical operations
    subroutine validate_logical_operator_types(arena, node, operator_str)
        use, intrinsic :: iso_fortran_env, only: error_unit
        type(ast_arena_t), intent(in) :: arena
        type(binary_op_node), intent(in) :: node
        character(len=*), intent(in) :: operator_str
        integer :: left_type, right_type
        character(len=20) :: left_type_str, right_type_str

        ! Get left operand type
        left_type = 0
        if (node%left_index > 0 .and. node%left_index <= arena%size) then
            if (allocated(arena%entries(node%left_index)%node)) then
                left_type = arena%entries(node%left_index)%node%inferred_type%kind
            end if
        end if

        ! Get right operand type
        right_type = 0
        if (node%right_index > 0 .and. node%right_index <= arena%size) then
            if (allocated(arena%entries(node%right_index)%node)) then
                right_type = arena%entries(node%right_index)%node%inferred_type%kind
            end if
        end if

        ! Check if both operands are TLOGICAL (or unknown/TVAR which is acceptable)
        ! TVAR (1) is unknown type variable, which is OK during type inference
        if (left_type /= TLOGICAL .and. left_type /= 0 .and. left_type /= TVAR) then
            call type_kind_to_string(left_type, left_type_str)
            write (error_unit, '(A,I0,A,A,A,A)') &
                "WARNING (issue #2457): Logical operator '", node%line, ":", &
                trim(operator_str), "' has non-logical left operand of type ", &
                trim(left_type_str)
        end if

        if (right_type /= TLOGICAL .and. right_type /= 0 .and. right_type /= TVAR) &
            then
            call type_kind_to_string(right_type, right_type_str)
            write (error_unit, '(A,I0,A,A,A,A)') &
                "WARNING (issue #2457): Logical operator '", node%line, ":", &
                trim(operator_str), "' has non-logical right operand of type ", &
                trim(right_type_str)
        end if
    end subroutine validate_logical_operator_types

    ! Helper to convert type kind to string for error messages
    subroutine type_kind_to_string(kind, str)
        integer, intent(in) :: kind
        character(len=*), intent(out) :: str

        select case (kind)
        case (TINT)
            str = "INTEGER"
        case (TREAL)
            str = "REAL"
        case (TCHAR)
            str = "CHARACTER"
        case (TLOGICAL)
            str = "LOGICAL"
        case (TVAR)
            str = "TVAR"
        case (TFUN)
            str = "FUNCTION"
        case (TARRAY)
            str = "ARRAY"
        case (TCOMPLEX)
            str = "COMPLEX"
        case (TDOUBLE)
            str = "DOUBLE"
        case (TDERIVED)
            str = "DERIVED"
        case default
            write (str, '(A,I0,A)') "UNKNOWN(", kind, ")"
        end select
    end subroutine type_kind_to_string

    ! Generate code for binary operations
    recursive function generate_code_binary_op(arena, node, node_index) result(code)
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
            left_code = trim(adjustl(left_code))
        else
            left_code = ""
        end if

        if (node%right_index > 0) then
            right_code = generate_code_from_arena(arena, node%right_index)
            ! Workaround for issue #2065: Remove leading whitespace that can
            ! appear before string literals when expressions are nested.
            right_code = trim(adjustl(right_code))
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

            ! Check for string concatenation: if operator is '+' and we're
            ! dealing with string literals
            if (node%operator == "+" .and. &
                is_string_concatenation(left_code, right_code)) then
                fortran_operator = "//"  ! Use Fortran string concat operator
            end if

            ! ISO/IEC 1539-1:2018 Section 10.1.5.2: Logical operators require
            ! LOGICAL operands. Detect non-logical operands before emitting.
            if (trim(fortran_operator) == ".and." .or. &
                trim(fortran_operator) == ".or." .or. &
                trim(fortran_operator) == ".eqv." .or. &
                trim(fortran_operator) == ".neqv.") then
                call validate_logical_operator_types(arena, node, fortran_operator)
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
            case ('=')
                ! For keyword arguments in function calls, don't add space before
                ! string literals to prevent introducing stray spaces
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
                ! Use spaces around all operators for readability
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
                else if (node%literal_kind == LITERAL_REAL) then
                    elem_code = promote_real_literal_to_double(elem_code)
                end if
            end select

            elements_code = elements_code // elem_code
        end do
    end function generate_real_elements_code

    ! Generate code for character array elements.
    ! String literals are padded to consistent target_len (max actual length).
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
        logical :: is_real_array
        integer :: nested_rows, nested_cols

        if (allocated(node%syntax_style) .and. node%syntax_style == "modern") then
            if (nested_array_shape(arena, node, nested_rows, nested_cols)) then
                code = generate_reshape_from_nested(arena, node, nested_rows, &
                    & nested_cols)
                return
            end if
        end if

        is_real_array = is_real_array_literal(arena, node)
        elements_code = build_array_elements_code(arena, node, is_real_array)
        code = format_array_literal_code(arena, node, elements_code)
    end function generate_code_array_literal

    function is_real_array_literal(arena, node) result(is_real_array)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        logical :: is_real_array
        type(mono_type_t) :: array_type
        integer :: j

        is_real_array = .false.

        if (node%inferred_type%kind /= TARRAY) return

        if (node%inferred_type%has_args() .and. &
            node%inferred_type%get_args_count() > 0) then
            array_type = node%inferred_type%get_arg(1)
            is_real_array = (array_type%kind == TREAL) .or. &
                            (array_type%kind == TDOUBLE)
            return
        end if

        if (.not. allocated(node%element_indices)) return

        do j = 1, size(node%element_indices)
            if (node%element_indices(j) > 0) then
                associate (entry => arena%entries(node%element_indices(j)))
                    select type (elem_node => entry%node)
                    type is (literal_node)
                        if (elem_node%literal_kind == LITERAL_REAL) then
                            is_real_array = .true.
                            return
                        end if
                    end select
                end associate
            end if
        end do
    end function is_real_array_literal

    function build_array_elements_code(arena, node, is_real_array) result(elements_code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        logical, intent(in) :: is_real_array
        character(len=:), allocatable :: elements_code
        integer :: max_actual_len
        logical :: all_char

        if (.not. allocated(node%element_indices)) then
            elements_code = ""
            return
        end if

        if (size(node%element_indices) == 0) then
            elements_code = ""
            return
        end if

        if (is_real_array) then
            elements_code = generate_real_elements_code(arena, node%element_indices)
            return
        end if

        call compute_char_element_length(arena, node%element_indices, max_actual_len, &
                                         all_char)

        if (all_char .and. max_actual_len > 0) then
            elements_code = generate_char_elements_code(arena, node%element_indices, &
                                                        max_actual_len)
        else
            elements_code = generate_elements_code_from_indices(arena, &
                                                                node%element_indices)
        end if
    end function build_array_elements_code

    subroutine compute_char_element_length(arena, element_indices, max_actual_len, &
                                           all_char)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: element_indices(:)
        integer, intent(out) :: max_actual_len
        logical, intent(out) :: all_char
        integer :: j, str_len_val

        max_actual_len = 0
        all_char = .true.

        do j = 1, size(element_indices)
            if (element_indices(j) <= 0) cycle
            associate (entry => arena%entries(element_indices(j)))
                if (.not. allocated(entry%node)) then
                    all_char = .false.
                    exit
                end if
                select type (elem => entry%node)
                type is (literal_node)
                    if (allocated(elem%value) .and. len(elem%value) >= 2) then
                        str_len_val = len_trim(elem%value) - 2
                        max_actual_len = max(max_actual_len, str_len_val)
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
    end subroutine compute_char_element_length

    function format_array_literal_code(arena, node, elements_code) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: node
        character(len=*), intent(in) :: elements_code
        character(len=:), allocatable :: code

        if (allocated(node%syntax_style)) then
            if (node%syntax_style == "modern") then
                if (allocated(node%element_indices) .and. &
                    size(node%element_indices) > 0) then
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: " // &
                               trim(elements_code) // " /)"
                    else
                        code = "(/ " // trim(elements_code) // " /)"
                    end if
                else
                    code = "(/ integer :: /)"
                end if
            else if (node%syntax_style == "implied_do") then
                if (allocated(node%element_indices) .and. &
                    size(node%element_indices) > 0) then
                    if (allocated(node%type_spec)) then
                        code = generate_implied_do_array(arena, &
                                                         node%element_indices(1), &
                                                         node%type_spec)
                    else
                        code = generate_implied_do_array(arena, &
                                                         node%element_indices(1))
                    end if
                else
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: /)"
                    else
                        code = "(/ integer :: /)"
                    end if
                end if
            else
                if (allocated(node%element_indices) .and. &
                    size(node%element_indices) > 0) then
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: " // &
                               trim(elements_code) // " /)"
                    else
                        code = "(/ " // trim(elements_code) // " /)"
                    end if
                else
                    if (allocated(node%type_spec)) then
                        code = "(/ " // trim(node%type_spec) // " :: /)"
                    else
                        code = "(/ integer :: /)"
                    end if
                end if
            end if
        else
            if (allocated(node%element_indices) .and. &
                size(node%element_indices) > 0) then
                if (allocated(node%type_spec)) then
                    code = "(/ " // trim(node%type_spec) // " :: " // &
                           trim(elements_code) // " /)"
                else
                    code = "(/ " // trim(elements_code) // " /)"
                end if
            else
                if (allocated(node%type_spec)) then
                    code = "(/ " // trim(node%type_spec) // " :: /)"
                else
                    code = "(/ integer :: /)"
                end if
            end if
        end if
    end function format_array_literal_code

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

        promoted = promoted // ".0d0"
    end function promote_integer_literal_to_real

    ! Promote default real literals to double precision form (d exponent) while
    ! preserving explicit kind suffixes.
    function promote_real_literal_to_double(code) result(promoted)
        character(len=*), intent(in) :: code
        character(len=:), allocatable :: promoted
        integer :: epos

        promoted = trim(code)
        if (len_trim(promoted) == 0) return
        if (index(promoted, '_') /= 0) return
        if (index(promoted, 'd') /= 0 .or. index(promoted, 'D') /= 0) return

        epos = index(promoted, 'e')
        if (epos == 0) epos = index(promoted, 'E')
        if (epos > 0) then
            promoted = promoted(1:epos - 1) // 'd' // promoted(epos + 1:)
            return
        end if

        promoted = promoted // 'd0'
    end function promote_real_literal_to_double

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
