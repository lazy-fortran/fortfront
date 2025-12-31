
    function generate_code_array_bounds(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        class(*), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: lower_code, upper_code, stride_code
        logical :: has_lower, has_upper, has_stride

        select type (bounds_node => node)
        type is (array_bounds_node)
            ! Assumed-rank takes highest precedence (Fortran 2018 ..)
            if (bounds_node%is_assumed_rank) then
                code = ".."
                return
            end if

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
                    inner = expr_code // ", " // node%var_name // "=" // &
                            start_code // ", " // end_code // ", " // step_code
                else
                    inner = expr_code // ", i=" // start_code // ", " // &
                            end_code // ", " // step_code
                end if
            else
                if (allocated(node%var_name)) then
                    inner = expr_code // ", " // node%var_name // "=" // &
                            start_code // ", " // end_code
                else
                    inner = expr_code // ", i=" // start_code // ", " // &
                            end_code
                end if
            end if

            if (present(type_spec)) then
                if (len_trim(type_spec) > 0) then
                    code = "[" // trim(type_spec) // " :: (" // trim(inner) // ")]"
                else
                    code = "[(" // trim(inner) // ")]"
                end if
            else
                code = "[(" // trim(inner) // ")]"
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
