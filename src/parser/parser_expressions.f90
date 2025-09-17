module parser_expressions_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, &
                          TK_OPERATOR, TK_KEYWORD, to_lower
    use ast_core
    use ast_nodes_core, only: component_access_node, identifier_node, &
                               range_subscript_node
    use ast_nodes_loops, only: do_loop_node
    use ast_factory, only: push_binary_op, push_literal, push_identifier, &
                           push_call_or_subscript, push_array_literal, &
                           push_range_expression, &
                           push_call_or_subscript_with_slice_detection, &
                           push_component_access, push_range_subscript, push_do_loop
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expression_helpers_module, only: parse_number_literal, &
                           parse_string_literal, parse_boolean_literal, &
                           parse_component_access_postfix
    implicit none
    private

    ! Pratt parser precedence levels (lower number = lower precedence)
    integer, parameter :: PREC_RANGE = 10
    integer, parameter :: PREC_LOGICAL_EQV = 20
    integer, parameter :: PREC_LOGICAL_OR = 30
    integer, parameter :: PREC_LOGICAL_AND = 40
    integer, parameter :: PREC_COMPARISON = 50
    integer, parameter :: PREC_CONCAT = 60
    integer, parameter :: PREC_TERM = 70
    integer, parameter :: PREC_FACTOR = 80
    integer, parameter :: PREC_POWER = 90
    integer, parameter :: PREC_UNARY = 95
    integer, parameter :: PREC_POSTFIX = 110

    integer, parameter :: MAX_OPERATOR_LEN = 16
    integer, parameter :: STACK_DEFAULT_CAPACITY = 32

    type :: operator_entry_t
        character(len=MAX_OPERATOR_LEN) :: symbol = ""
        integer :: precedence = 0
        logical :: right_associative = .false.
        logical :: is_group = .false.
        type(token_t) :: token
    end type operator_entry_t

    type :: operator_stack_t
        type(operator_entry_t), allocatable :: values(:)
        integer :: size = 0
    end type operator_stack_t

    type :: operand_stack_t
        integer, allocatable :: values(:)
        integer :: size = 0
    end type operand_stack_t

    type :: token_stack_t
        type(token_t), allocatable :: values(:)
        integer :: size = 0
    end type token_stack_t

    type :: token_view_t
        character(len=:), allocatable :: text(:)
        character(len=:), allocatable :: lower(:)
        integer, allocatable :: kind(:)
        integer, allocatable :: line(:)
        integer, allocatable :: column(:)
        integer :: base_index = 1
        integer :: count = 0
    end type token_view_t

    ! Public expression parsing interface
    public :: parse_expression
    public :: parse_range, parse_logical_eqv, parse_logical_or, parse_logical_and, parse_comparison
    public :: parse_concatenation, parse_term, parse_factor, parse_power, parse_unary, parse_primary
    public :: parse_expression_until, parse_postfix_chain

contains

    subroutine build_token_view(view, parser)
        type(token_view_t), intent(inout) :: view
        type(parser_state_t), intent(in) :: parser
        integer :: start_idx
        integer :: end_idx
        integer :: count
        integer :: idx
        integer :: max_len
        type(token_t) :: token

        if (.not. allocated(parser%tokens)) then
            view%count = 0
            view%base_index = parser%current_token
            if (allocated(view%text)) deallocate(view%text)
            if (allocated(view%lower)) deallocate(view%lower)
            if (allocated(view%kind)) deallocate(view%kind)
            if (allocated(view%line)) deallocate(view%line)
            if (allocated(view%column)) deallocate(view%column)
            return
        end if

        start_idx = max(parser%current_token, 1)
        end_idx = size(parser%tokens)
        count = end_idx - start_idx + 1
        if (count < 1) then
            count = 1
        end if

        max_len = 1
        do idx = start_idx, end_idx
            token = parser%tokens(idx)
            max_len = max(max_len, len_trim(token%text))
        end do
        if (max_len < 1) max_len = 1

        if (allocated(view%text)) deallocate(view%text)
        if (allocated(view%lower)) deallocate(view%lower)
        if (allocated(view%kind)) deallocate(view%kind)
        if (allocated(view%line)) deallocate(view%line)
        if (allocated(view%column)) deallocate(view%column)

        allocate(character(len=max_len) :: view%text(count))
        allocate(character(len=max_len) :: view%lower(count))
        allocate(view%kind(count))
        allocate(view%line(count))
        allocate(view%column(count))

        do idx = 1, count
            token = parser%tokens(start_idx + idx - 1)
            view%text(idx) = token%text
            view%lower(idx) = to_lower(token%text)
            view%kind(idx) = token%kind
            view%line(idx) = token%line
            view%column(idx) = token%column
        end do

        view%base_index = start_idx
        view%count = count
    end subroutine build_token_view

    function view_peek_token(view, parser) result(token)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(in) :: parser
        type(token_t) :: token
        integer :: idx

        idx = parser%current_token - view%base_index + 1
        if (idx < 1 .or. idx > view%count) then
            token = parser%peek()
            return
        end if

        token%text = view%text(idx)
        token%kind = view%kind(idx)
        token%line = view%line(idx)
        token%column = view%column(idx)
    end function view_peek_token

    function view_lower_token(view, parser, offset) result(lowered)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(in) :: parser
        integer, intent(in), optional :: offset
        character(len=:), allocatable :: lowered
        integer :: idx
        integer :: off

        off = 0
        if (present(offset)) off = offset
        idx = parser%current_token - view%base_index + 1 + off

        if (idx < 1 .or. idx > view%count) then
            block
                type(token_t) :: fallback_token
                fallback_token = view_peek_token(view, parser)
                lowered = to_lower(fallback_token%text)
            end block
            return
        end if

        lowered = trim(view%lower(idx))
    end function view_lower_token

    function view_consume_token(view, parser) result(token)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        token = view_peek_token(view, parser)
        if (.not. parser%is_at_end()) then
            block
                type(token_t) :: discarded_token
                discarded_token = parser%consume()
            end block
        end if
    end function view_consume_token

    function view_lookahead_token(view, parser, offset) result(token)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: offset
        type(token_t) :: token
        integer :: idx

        idx = parser%current_token - view%base_index + 1 + offset
        if (idx < 1 .or. idx > view%count) then
            token = parser%peek()
            return
        end if

        token%text = view%text(idx)
        token%kind = view%kind(idx)
        token%line = view%line(idx)
        token%column = view%column(idx)
    end function view_lookahead_token

    !=================================================================================
    ! STACK UTILITIES FOR PRATT PARSER
    !=================================================================================

    subroutine operator_stack_clear(stack)
        type(operator_stack_t), intent(inout) :: stack

        stack%size = 0
    end subroutine operator_stack_clear

    subroutine operator_stack_ensure_capacity(stack, desired)
        type(operator_stack_t), intent(inout) :: stack
        integer, intent(in) :: desired
        type(operator_entry_t), allocatable :: new_values(:)
        integer :: new_capacity

        if (.not. allocated(stack%values)) then
            allocate(stack%values(max(STACK_DEFAULT_CAPACITY, desired)))
            stack%size = 0
            return
        end if

        if (size(stack%values) >= desired) return

        new_capacity = max(size(stack%values) * 2, desired)
        allocate(new_values(new_capacity))
        if (stack%size > 0) then
            new_values(1:stack%size) = stack%values(1:stack%size)
        end if
        call move_alloc(new_values, stack%values)
    end subroutine operator_stack_ensure_capacity

    subroutine operator_stack_push(stack, entry)
        type(operator_stack_t), intent(inout) :: stack
        type(operator_entry_t), intent(in) :: entry

        call operator_stack_ensure_capacity(stack, stack%size + 1)
        stack%size = stack%size + 1
        stack%values(stack%size) = entry
    end subroutine operator_stack_push

    function operator_stack_pop(stack) result(entry)
        type(operator_stack_t), intent(inout) :: stack
        type(operator_entry_t) :: entry

        if (stack%size <= 0) then
            entry = operator_entry_t()
            return
        end if

        entry = stack%values(stack%size)
        stack%size = stack%size - 1
    end function operator_stack_pop

    function operator_stack_peek(stack) result(entry)
        type(operator_stack_t), intent(in) :: stack
        type(operator_entry_t) :: entry

        if (stack%size <= 0) then
            entry = operator_entry_t()
        else
            entry = stack%values(stack%size)
        end if
    end function operator_stack_peek

    logical function operator_stack_is_empty(stack)
        type(operator_stack_t), intent(in) :: stack
        operator_stack_is_empty = (stack%size <= 0)
    end function operator_stack_is_empty

    logical function operator_stack_has_open_group(stack)
        type(operator_stack_t), intent(in) :: stack
        integer :: idx

        operator_stack_has_open_group = .false.
        if (.not. allocated(stack%values)) return

        do idx = stack%size, 1, -1
            if (stack%values(idx)%is_group) then
                operator_stack_has_open_group = .true.
                return
            end if
        end do
    end function operator_stack_has_open_group

    subroutine operand_stack_clear(stack)
        type(operand_stack_t), intent(inout) :: stack

        stack%size = 0
    end subroutine operand_stack_clear

    subroutine operand_stack_ensure_capacity(stack, desired)
        type(operand_stack_t), intent(inout) :: stack
        integer, intent(in) :: desired
        integer, allocatable :: new_values(:)
        integer :: new_capacity

        if (.not. allocated(stack%values)) then
            allocate(stack%values(max(STACK_DEFAULT_CAPACITY, desired)))
            stack%size = 0
            return
        end if

        if (size(stack%values) >= desired) return

        new_capacity = max(size(stack%values) * 2, desired)
        allocate(new_values(new_capacity))
        if (stack%size > 0) then
            new_values(1:stack%size) = stack%values(1:stack%size)
        end if
        call move_alloc(new_values, stack%values)
    end subroutine operand_stack_ensure_capacity

    subroutine operand_stack_push(stack, value)
        type(operand_stack_t), intent(inout) :: stack
        integer, intent(in) :: value

        call operand_stack_ensure_capacity(stack, stack%size + 1)
        stack%size = stack%size + 1
        stack%values(stack%size) = value
    end subroutine operand_stack_push

    integer function operand_stack_pop(stack)
        type(operand_stack_t), intent(inout) :: stack

        if (stack%size <= 0) then
            operand_stack_pop = 0
            return
        end if

        operand_stack_pop = stack%values(stack%size)
        stack%size = stack%size - 1
    end function operand_stack_pop

    integer function operand_stack_peek(stack)
        type(operand_stack_t), intent(in) :: stack

        if (stack%size <= 0) then
            operand_stack_peek = 0
        else
            operand_stack_peek = stack%values(stack%size)
        end if
    end function operand_stack_peek

    logical function operand_stack_is_empty(stack)
        type(operand_stack_t), intent(in) :: stack
        operand_stack_is_empty = (stack%size <= 0)
    end function operand_stack_is_empty

    subroutine token_stack_clear(stack)
        type(token_stack_t), intent(inout) :: stack

        stack%size = 0
    end subroutine token_stack_clear

    subroutine token_stack_ensure_capacity(stack, desired)
        type(token_stack_t), intent(inout) :: stack
        integer, intent(in) :: desired
        type(token_t), allocatable :: new_values(:)
        integer :: new_capacity

        if (.not. allocated(stack%values)) then
            allocate(stack%values(max(STACK_DEFAULT_CAPACITY, desired)))
            stack%size = 0
            return
        end if

        if (size(stack%values) >= desired) return

        new_capacity = max(size(stack%values) * 2, desired)
        allocate(new_values(new_capacity))
        if (stack%size > 0) then
            new_values(1:stack%size) = stack%values(1:stack%size)
        end if
        call move_alloc(new_values, stack%values)
    end subroutine token_stack_ensure_capacity

    subroutine token_stack_push(stack, value)
        type(token_stack_t), intent(inout) :: stack
        type(token_t), intent(in) :: value

        call token_stack_ensure_capacity(stack, stack%size + 1)
        stack%size = stack%size + 1
        stack%values(stack%size) = value
    end subroutine token_stack_push

    function token_stack_pop(stack) result(token)
        type(token_stack_t), intent(inout) :: stack
        type(token_t) :: token

        if (stack%size <= 0) then
            token%text = ""
            token%line = 0
            token%column = 0
            token%kind = 0
            return
        end if

        token = stack%values(stack%size)
        stack%size = stack%size - 1
    end function token_stack_pop

    logical function token_stack_is_empty(stack)
        type(token_stack_t), intent(in) :: stack
        token_stack_is_empty = (stack%size <= 0)
    end function token_stack_is_empty

    logical function token_matches(token, text)
        type(token_t), intent(in) :: token
        character(len=*), intent(in) :: text
        token_matches = trim(token%text) == trim(text)
    end function token_matches

    logical function token_is_boolean_literal(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        lowered = to_lower(token%text)
        token_is_boolean_literal = (lowered == ".true." .or. lowered == ".false." .or. &
            lowered == "true" .or. lowered == "false")
    end function token_is_boolean_literal

    logical function is_prefix_operator_token(token)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        if (token%kind /= TK_OPERATOR) then
            is_prefix_operator_token = .false.
            return
        end if

        lowered = to_lower(token%text)
        is_prefix_operator_token = (lowered == "+" .or. lowered == "-" .or. lowered == ".not.")
    end function is_prefix_operator_token

    logical function is_range_operator(token)
        type(token_t), intent(in) :: token
        is_range_operator = (token%kind == TK_OPERATOR .and. trim(token%text) == ":")
    end function is_range_operator

    function get_infix_operator_entry(token) result(entry)
        type(token_t), intent(in) :: token
        type(operator_entry_t) :: entry
        character(len=:), allocatable :: lowered

        entry = operator_entry_t()
        if (token%kind /= TK_OPERATOR) return

        lowered = to_lower(token%text)

        select case (lowered)
        case (".eqv.", ".neqv.")
            entry%symbol = token%text
            entry%precedence = PREC_LOGICAL_EQV
        case (".or.")
            entry%symbol = token%text
            entry%precedence = PREC_LOGICAL_OR
        case (".and.")
            entry%symbol = token%text
            entry%precedence = PREC_LOGICAL_AND
        case ("==", "/=", "<=", ">=", "<", ">")
            entry%symbol = token%text
            entry%precedence = PREC_COMPARISON
        case ("//")
            entry%symbol = token%text
            entry%precedence = PREC_CONCAT
        case ("+", "-")
            entry%symbol = token%text
            entry%precedence = PREC_TERM
        case ("*", "/")
            entry%symbol = token%text
            entry%precedence = PREC_FACTOR
        case ("**")
            entry%symbol = token%text
            entry%precedence = PREC_POWER
            entry%right_associative = .true.
        case default
            ! Not an infix operator handled here
            return
        end select

        entry%token = token
    end function get_infix_operator_entry

    logical function comparison_active(stack)
        type(operator_stack_t), intent(in) :: stack
        integer :: idx

        comparison_active = .false.
        if (.not. allocated(stack%values)) return

        do idx = stack%size, 1, -1
            if (stack%values(idx)%is_group) then
                return
            end if
            if (stack%values(idx)%precedence == PREC_COMPARISON) then
                comparison_active = .true.
                return
            end if
        end do
    end function comparison_active

    function create_zero_literal(arena, reference_token) result(zero_index)
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: reference_token
        integer :: zero_index

        zero_index = push_literal(arena, "0", LITERAL_INTEGER, &
            reference_token%line, reference_token%column)
    end function create_zero_literal

    function apply_prefix_stack(arena, prefix_stack, expr_index) result(result_index)
        type(ast_arena_t), intent(inout) :: arena
        type(token_stack_t), intent(inout) :: prefix_stack
        integer, intent(in) :: expr_index
        integer :: result_index
        type(token_t) :: token
        character(len=:), allocatable :: lowered
        integer :: zero_index

        result_index = expr_index
        do while (.not. token_stack_is_empty(prefix_stack))
            token = token_stack_pop(prefix_stack)
            lowered = to_lower(token%text)
            select case (lowered)
            case ("-")
                zero_index = create_zero_literal(arena, token)
                result_index = push_binary_op(arena, zero_index, result_index, &
                    token%text, token%line, token%column)
            case ("+")
                cycle
            case (".not.")
                result_index = push_binary_op(arena, 0, result_index, token%text, &
                    token%line, token%column)
            case default
                cycle
            end select
        end do
    end function apply_prefix_stack

    subroutine reduce_single_operator(operators, operands, arena)
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena
        type(operator_entry_t) :: op_entry
        integer :: right_index, left_index

        op_entry = operator_stack_pop(operators)
        if (op_entry%is_group) return

        right_index = operand_stack_pop(operands)
        left_index = operand_stack_pop(operands)

        if (right_index <= 0 .or. left_index < 0) then
            call operand_stack_push(operands, 0)
            return
        end if

        call operand_stack_push(operands, push_binary_op(arena, left_index, right_index, &
            op_entry%symbol, op_entry%token%line, op_entry%token%column))
    end subroutine reduce_single_operator

    subroutine reduce_operators_for_incoming(operators, operands, arena, incoming)
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena
        type(operator_entry_t), intent(in) :: incoming
        type(operator_entry_t) :: top_entry

        do while (.not. operator_stack_is_empty(operators))
            top_entry = operator_stack_peek(operators)
            if (top_entry%is_group) exit
            if (top_entry%precedence < incoming%precedence) exit
            if (top_entry%precedence == incoming%precedence) then
                if (incoming%right_associative) exit
                if (incoming%precedence == PREC_COMPARISON) exit
            end if
            call reduce_single_operator(operators, operands, arena)
        end do
    end subroutine reduce_operators_for_incoming

    subroutine reduce_all_operators(operators, operands, arena)
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena

        do while (.not. operator_stack_is_empty(operators))
            call reduce_single_operator(operators, operands, arena)
        end do
    end subroutine reduce_all_operators

    subroutine reduce_until_group(operators, operands, arena)
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena
        type(operator_entry_t) :: top_entry

        do while (.not. operator_stack_is_empty(operators))
            top_entry = operator_stack_peek(operators)
            if (top_entry%is_group) exit
            call reduce_single_operator(operators, operands, arena)
        end do
    end subroutine reduce_until_group

    subroutine push_group_marker(operators, token)
        type(operator_stack_t), intent(inout) :: operators
        type(token_t), intent(in) :: token
        type(operator_entry_t) :: entry

        entry = operator_entry_t(symbol="(", precedence=0, is_group=.true., token=token)
        call operator_stack_push(operators, entry)
    end subroutine push_group_marker

    logical function token_is_terminator(token, terminators)
        type(token_t), intent(in) :: token
        character(len=*), intent(in), optional :: terminators(:)
        integer :: idx

        if (.not. present(terminators)) then
            token_is_terminator = .false.
            return
        end if

        token_is_terminator = .false.
        do idx = 1, size(terminators)
            if (trim(token%text) == trim(terminators(idx))) then
                token_is_terminator = .true.
                return
            end if
        end do
    end function token_is_terminator

    integer function terminator_count(terminators)
        character(len=*), intent(in), optional :: terminators(:)

        if (present(terminators)) then
            terminator_count = size(terminators)
        else
            terminator_count = 0
        end if
    end function terminator_count

    logical function is_legacy_array_literal_start(parser, view)
        type(parser_state_t), intent(in) :: parser
        type(token_view_t), intent(in) :: view
        type(token_t) :: current
        type(token_t) :: next_token

        is_legacy_array_literal_start = .false.
        current = view_peek_token(view, parser)
        if (current%kind /= TK_OPERATOR) return
        if (trim(current%text) /= "(") return

        next_token = view_lookahead_token(view, parser, 1)
        if (next_token%kind == TK_OPERATOR .and. trim(next_token%text) == "/") then
            is_legacy_array_literal_start = .true.
        end if
    end function is_legacy_array_literal_start

    function parse_operand_base(parser, arena, view) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        integer :: expr_index
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        expr_index = 0
        token = view_peek_token(view, parser)

        select case (token%kind)
        case (TK_NUMBER)
            token = view_consume_token(view, parser)
            expr_index = parse_number_literal(token, arena)

        case (TK_STRING)
            token = view_consume_token(view, parser)
            expr_index = parse_string_literal(token, arena)

        case (TK_IDENTIFIER)
            token = view_consume_token(view, parser)
            lowered = to_lower(token%text)
            if (lowered == "true" .or. lowered == "false") then
                expr_index = push_literal(arena, token%text, LITERAL_LOGICAL, &
                    token%line, token%column)
            else
                expr_index = push_identifier(arena, token%text, token%line, token%column)
            end if

        case (TK_OPERATOR)
            lowered = view_lower_token(view, parser)
            if (lowered == "[") then
                token = view_consume_token(view, parser)
                expr_index = parse_modern_array_literal(parser, arena)
            else if (token_is_boolean_literal(token)) then
                token = view_consume_token(view, parser)
                expr_index = parse_boolean_literal(token, arena)
            else
                ! Defer handling of parentheses to Pratt core; do not consume here
                expr_index = 0
            end if

        case (TK_KEYWORD)
            token = view_consume_token(view, parser)
            lowered = to_lower(token%text)
            if (lowered == ".true." .or. lowered == ".false.") then
                expr_index = parse_boolean_literal(token, arena)
            else
                expr_index = push_identifier(arena, token%text, token%line, token%column)
            end if

        case default
            expr_index = push_literal(arena, &
                "!ERROR: Unexpected token '"//trim(token%text)//"'", &
                LITERAL_STRING, token%line, token%column)
            token = view_consume_token(view, parser)
        end select
    end function parse_operand_base

    function parse_range_with_lower(parser, arena, view, lower_index, colon_token, terminators) &
        result(range_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        integer, intent(in) :: lower_index
        type(token_t), intent(in) :: colon_token
        character(len=*), intent(in), optional :: terminators(:)
        integer :: range_index
        integer :: upper_index
        integer :: stride_index
        type(token_t) :: lookahead
        integer :: base_count, total_terms
        character(len=MAX_OPERATOR_LEN), allocatable :: local_terms(:)

        upper_index = 0
        stride_index = 0

        if (.not. parser%is_at_end()) then
            lookahead = view_peek_token(view, parser)
            if (.not. (lookahead%kind == TK_OPERATOR .and. &
                (trim(lookahead%text) == ":" .or. trim(lookahead%text) == ")" .or. &
                 trim(lookahead%text) == "]" .or. trim(lookahead%text) == "," .or. &
                 trim(lookahead%text) == ";" .or. trim(lookahead%text) == "/"))) then
                base_count = terminator_count(terminators)
                total_terms = base_count + 1
                allocate(character(len=MAX_OPERATOR_LEN) :: local_terms(total_terms))
                if (base_count > 0) then
                    local_terms(1:base_count) = terminators(1:base_count)
                end if
                local_terms(total_terms) = ":"
                upper_index = parse_expression_with_precedence(parser, arena, &
                    PREC_LOGICAL_EQV, local_terms)
                deallocate(local_terms)
            end if
        end if

        if (.not. parser%is_at_end()) then
            lookahead = view_peek_token(view, parser)
            if (lookahead%kind == TK_OPERATOR .and. trim(lookahead%text) == ":") then
                lookahead = view_consume_token(view, parser)
                base_count = terminator_count(terminators)
                if (base_count > 0) then
                    allocate(character(len=MAX_OPERATOR_LEN) :: local_terms(base_count))
                    local_terms = terminators(1:base_count)
                    stride_index = parse_expression_with_precedence(parser, arena, &
                        PREC_LOGICAL_EQV, local_terms)
                    deallocate(local_terms)
                else
                    stride_index = parse_expression_with_precedence(parser, arena, &
                        PREC_LOGICAL_EQV)
                end if
            end if
        end if

        range_index = push_range_expression(arena, lower_index, upper_index, stride_index, &
            colon_token%line, colon_token%column)
    end function parse_range_with_lower

    function parse_range_from_missing_lower(parser, arena, view, colon_token, terminators) &
        result(range_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        type(token_t), intent(in) :: colon_token
        character(len=*), intent(in), optional :: terminators(:)
        integer :: range_index

        range_index = parse_range_with_lower(parser, arena, view, 0, colon_token, terminators)
    end function parse_range_from_missing_lower

    recursive function parse_expression_with_precedence(parser, arena, minimum_precedence, &
            terminators) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: minimum_precedence
        character(len=*), intent(in), optional :: terminators(:)
        integer :: expr_index

        type(operator_stack_t) :: operators
        type(operand_stack_t) :: operands
        type(token_stack_t) :: prefix_stack
        type(token_view_t) :: view
        logical :: expect_operand
        type(token_t) :: token
        type(operator_entry_t) :: op_entry
        integer :: current_index
        integer :: lower_index

        call operator_stack_clear(operators)
        call operand_stack_clear(operands)
        call token_stack_clear(prefix_stack)
        call build_token_view(view, parser)

        expr_index = 0
        expect_operand = .true.

        main_loop: do while (.true.)
            token = view_peek_token(view, parser)

            if (token%kind == TK_EOF) exit main_loop

            if (.not. expect_operand) then
                if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                    if (operator_stack_has_open_group(operators)) then
                        call reduce_until_group(operators, operands, arena)
                        op_entry = operator_stack_pop(operators)
                        token = view_consume_token(view, parser)
                        expect_operand = .false.
                        cycle
                    else
                        exit main_loop
                    end if
                end if
            end if

            if (token_is_terminator(token, terminators)) exit main_loop

            if (expect_operand) then
                if (is_prefix_operator_token(token)) then
                    call token_stack_push(prefix_stack, view_consume_token(view, parser))
                    cycle
                end if

                if (is_legacy_array_literal_start(parser, view)) then
                    current_index = parse_legacy_array_literal(parser, arena)
                    current_index = apply_prefix_stack(arena, prefix_stack, current_index)
                    current_index = parse_postfix_ops(parser, arena, view, current_index)
                    call operand_stack_push(operands, current_index)
                    expect_operand = .false.
                    cycle
                end if

                if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                    call push_group_marker(operators, view_consume_token(view, parser))
                    expect_operand = .true.
                    cycle
                end if

                if (is_range_operator(token) .and. minimum_precedence <= PREC_RANGE) then
                    token = view_consume_token(view, parser)
                    current_index = parse_range_from_missing_lower(parser, arena, view, token, terminators)
                    current_index = apply_prefix_stack(arena, prefix_stack, current_index)
                    call operand_stack_push(operands, current_index)
                    expect_operand = .false.
                    cycle
                end if

                current_index = parse_operand_base(parser, arena, view)
                if (current_index > 0) then
                    current_index = apply_prefix_stack(arena, prefix_stack, current_index)
                    current_index = parse_postfix_ops(parser, arena, view, current_index)
                    call operand_stack_push(operands, current_index)
                else
                    call token_stack_clear(prefix_stack)
                end if
                expect_operand = .false.
                cycle
            else
                if (is_range_operator(token) .and. minimum_precedence <= PREC_RANGE) then
                    lower_index = operand_stack_pop(operands)
                    token = view_consume_token(view, parser)
                    current_index = parse_range_with_lower(parser, arena, view, lower_index, token, terminators)
                    call operand_stack_push(operands, current_index)
                    expect_operand = .false.
                    cycle
                end if

                op_entry = get_infix_operator_entry(token)
                if (op_entry%symbol == "") exit main_loop

                if (op_entry%precedence < minimum_precedence) exit main_loop

                if (op_entry%precedence == PREC_COMPARISON .and. comparison_active(operators)) exit main_loop

                call reduce_operators_for_incoming(operators, operands, arena, op_entry)
                call operator_stack_push(operators, op_entry)
                token = view_consume_token(view, parser)
                expect_operand = .true.
                cycle
            end if
        end do main_loop

        call reduce_all_operators(operators, operands, arena)
        expr_index = operand_stack_pop(operands)
    end function parse_expression_with_precedence







    !=================================================================================
    ! MAIN ENTRY POINT
    !=================================================================================


    ! Main expression parsing entry point with stack
    function parse_expression(tokens, arena) result(expr_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(parser_state_t) :: parser

        parser = create_parser_state(tokens)
        expr_index = parse_range(parser, arena)
    end function parse_expression



    ! Parse logical EQV/NEQV operators (lowest precedence)
    function parse_logical_eqv(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_LOGICAL_EQV)
    end function parse_logical_eqv

    ! Parse logical OR operators
    function parse_logical_or(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_LOGICAL_OR)
    end function parse_logical_or

    ! Parse logical AND operators
    function parse_logical_and(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_LOGICAL_AND)
    end function parse_logical_and

    ! Parse comparison operators
    function parse_comparison(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_COMPARISON)
    end function parse_comparison

    ! Parse string concatenation operator (//) - Issue #214
    function parse_concatenation(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_CONCAT)
    end function parse_concatenation
    ! Parse addition and subtraction
    function parse_term(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_TERM)
    end function parse_term

    ! Parse multiplication and division
    function parse_factor(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_FACTOR)
    end function parse_factor

    ! Parse exponentiation (**) - right-associative
    function parse_power(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_POWER)
    end function parse_power

    ! Parse unary operators (+, -, .NOT.) - Issue #215
    function parse_unary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_UNARY)
    end function parse_unary

    ! Parse primary expressions (literals, identifiers, parentheses)  
    function parse_primary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = parse_expression_with_precedence(parser, arena, PREC_POSTFIX)
    end function parse_primary

    function parse_expression_until(parser, arena, terminators) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in), optional :: terminators(:)
        integer :: expr_index
        character(len=MAX_OPERATOR_LEN), allocatable :: local_terms(:)

        if (present(terminators)) then
            if (size(terminators) > 0) then
                allocate(character(len=MAX_OPERATOR_LEN) :: local_terms(size(terminators)))
                local_terms = terminators
                expr_index = parse_expression_with_precedence(parser, arena, &
                    PREC_RANGE + 1, local_terms)
                deallocate(local_terms)
            else
                expr_index = parse_expression_with_precedence(parser, arena, &
                    PREC_RANGE + 1)
            end if
        else
            expr_index = parse_expression_with_precedence(parser, arena, &
                PREC_RANGE + 1)
        end if
    end function parse_expression_until

    function parse_postfix_chain(parser, arena, base_expr) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        integer :: expr_index
        type(token_view_t) :: view

        expr_index = base_expr
        if (expr_index <= 0) return

        call build_token_view(view, parser)
        expr_index = parse_postfix_ops(parser, arena, view, expr_index)
    end function parse_postfix_chain

    !=================================================================================
    ! RANGE EXPRESSION PARSING SECTION
    !=================================================================================

    ! Parse range/slice operator (:) - lowest precedence after logical operators
    function parse_range(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        character(len=MAX_OPERATOR_LEN) :: range_terms(5)

        range_terms = [character(len=MAX_OPERATOR_LEN) :: ")", "]", ",", ";", "/"]

        expr_index = parse_expression_with_precedence(parser, arena, PREC_RANGE, range_terms)
    end function parse_range
    !=================================================================================
    ! HELPER FUNCTIONS FOR ARRAY PARSING
    !=================================================================================
    

    
    ! Simplified array element parsing 
    function parse_simple_array_elements(parser, arena, terminator, style, start_token) &
                                         result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: terminator, style
        type(token_t), intent(in) :: start_token
        integer :: expr_index
        
        integer, allocatable :: element_indices(:), temp_indices(:)
        integer :: element_count
        type(token_t) :: current
        
        element_count = 0
        allocate (temp_indices(20))
        
        do
            element_count = element_count + 1
            if (element_count > size(temp_indices)) then
                block
                    integer, allocatable :: new_indices(:)
                    allocate (new_indices(size(temp_indices) + 20))
                    new_indices(1:size(temp_indices)) = temp_indices
                    call move_alloc(new_indices, temp_indices)
                end block
            end if
            
            ! Parse element based on style
            if (style == "modern") then
                temp_indices(element_count) = parse_comparison(parser, arena)
            else
                temp_indices(element_count) = parse_unary(parser, arena)
            end if
            
            if (temp_indices(element_count) <= 0) then
                expr_index = 0
                return
            end if
            
            current = parser%peek()
            if (current%text == ",") then
                current = parser%consume()
            else if (current%text == terminator) then
                current = parser%consume()
                exit
            else
                expr_index = 0
                return
            end if
        end do
        
        allocate (element_indices(element_count))
        element_indices = temp_indices(1:element_count)
        expr_index = push_array_literal(arena, element_indices, &
                                         start_token%line, start_token%column, &
                                         syntax_style=style)
    end function parse_simple_array_elements

    !=================================================================================
    ! RANGE AND SLICE EXPRESSION PARSING SECTION
    !=================================================================================

    ! Helper function to parse stride (third component of range expression)
    function parse_stride_component(parser, arena) result(stride_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stride_index
        type(token_t) :: op_token, next_tok
        
        stride_index = 0
        if (.not. parser%is_at_end()) then
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ":") then
                op_token = parser%consume()
                if (.not. parser%is_at_end()) then
                    next_tok = parser%peek()
                    if (.not. (next_tok%kind == TK_OPERATOR .and. &
                             (next_tok%text == ")" .or. next_tok%text == "," .or. &
                              next_tok%text == "]" .or. next_tok%text == ";"))) then
                        stride_index = parse_logical_eqv(parser, arena)
                    end if
                end if
            end if
        end if
    end function parse_stride_component

    !=================================================================================
    ! ARRAY LITERAL PARSING SECTION
    !=================================================================================

    ! Parse legacy array literal: (/ ... /)
    function parse_legacy_array_literal(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        
        block
            type(token_t) :: paren_token, current
            integer, allocatable :: element_indices(:)

            paren_token = parser%consume()
            current = parser%consume()  ! consume '/'
            current = parser%peek()
            
            ! Check for empty array (//)
            if (current%text == "/") then
                current = parser%consume()
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()
                    allocate (element_indices(0))
                    expr_index = push_array_literal(arena, element_indices, &
                                                     paren_token%line, paren_token%column, &
                                                     syntax_style="legacy")
                else
                    expr_index = 0
                end if
                return
            end if

            ! Parse elements with legacy syntax (closing /))
            expr_index = parse_simple_array_elements(parser, arena, "/", "legacy", paren_token)
            
            ! Consume final closing paren for legacy syntax
            if (expr_index > 0) then
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()
                else
                    expr_index = 0  ! Error: missing closing paren
                end if
            end if
        end block
    end function parse_legacy_array_literal

    ! Parse modern array literal: [1, 2, 3] with implied do loop support
    function parse_modern_array_literal(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        
        block
            type(token_t) :: bracket_token, current, next_token
            integer, allocatable :: element_indices(:), temp_indices(:)
            integer :: element_count
            logical :: is_implied_do

            bracket_token = parser%consume()
            current = parser%peek()
            
            ! Check for empty array []
            if (current%text == "]") then
                current = parser%consume()
                allocate (element_indices(0))
                expr_index = push_array_literal(arena, element_indices, &
                                                bracket_token%line, bracket_token%column, &
                                                syntax_style="modern")
                return
            end if

            ! Check for implied do loop: [(expr, var=start,end)]
            is_implied_do = .false.
            if (current%kind == TK_OPERATOR .and. current%text == "(") then
                ! Potential implied do - check for pattern
                is_implied_do = .true.
            end if
            
            if (is_implied_do) then
                ! Parse implied do loop
                expr_index = parse_implied_do_constructor(parser, arena, bracket_token)
            else
                ! Parse regular array elements
                expr_index = parse_simple_array_elements(parser, arena, "]", "modern", bracket_token)
            end if
        end block
    end function parse_modern_array_literal

    ! Parse implied do constructor: [(expr, var=start,end[,step])]
    function parse_implied_do_constructor(parser, arena, bracket_token) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: bracket_token
        integer :: expr_index
        
        type(token_t) :: current
        integer :: expr_elem_index, start_index, end_index, step_index
        character(len=:), allocatable :: var_name
        integer, allocatable :: element_indices(:)
        
        ! Consume opening parenthesis
        current = parser%consume()  ! (
        
        ! Parse the expression part (e.g., i*2)
        expr_elem_index = parse_comparison(parser, arena)
        
        ! Expect comma
        current = parser%peek()
        if (current%text /= ",") then
            expr_index = 0
            return
        end if
        current = parser%consume()
        
        ! Parse loop variable (e.g., i)
        current = parser%peek()
        if (current%kind /= TK_IDENTIFIER) then
            expr_index = 0
            return
        end if
        var_name = current%text
        current = parser%consume()
        
        ! Expect '='
        current = parser%peek()
        if (current%text /= "=") then
            expr_index = 0
            return
        end if
        current = parser%consume()
        
        ! Parse start expression
        start_index = parse_comparison(parser, arena)
        
        ! Expect comma
        current = parser%peek()
        if (current%text /= ",") then
            expr_index = 0
            return
        end if
        current = parser%consume()
        
        ! Parse end expression
        end_index = parse_comparison(parser, arena)
        
        ! Check for optional step (comma followed by step expression)
        step_index = 0
        current = parser%peek()
        if (current%text == ",") then
            current = parser%consume()
            step_index = parse_comparison(parser, arena)
        end if
        
        ! Expect closing parenthesis
        current = parser%peek()
        if (current%text /= ")") then
            expr_index = 0
            return
        end if
        current = parser%consume()
        
        ! Expect closing bracket
        current = parser%peek()
        if (current%text /= "]") then
            expr_index = 0
            return
        end if
        current = parser%consume()
        
        ! Create do loop node for the implied do
        block
            type(do_loop_node) :: do_node
            integer :: do_index
            
            do_node%var_name = var_name
            do_node%start_expr_index = start_index
            do_node%end_expr_index = end_index
            do_node%step_expr_index = step_index
            
            ! The body of the implied do is the expression itself
            allocate(do_node%body_indices(1))
            do_node%body_indices(1) = expr_elem_index
            
            do_index = push_do_loop(arena, var_name, start_index, end_index, &
                                   step_index, do_node%body_indices, &
                                   "", bracket_token%line, bracket_token%column)
            
            ! Wrap in array literal with the do loop as element
            allocate(element_indices(1))
            element_indices(1) = do_index
            expr_index = push_array_literal(arena, element_indices, &
                                           bracket_token%line, bracket_token%column, &
                                           syntax_style="implied_do")
        end block
        
    end function parse_implied_do_constructor

    ! Simplified implied do loop parsing (placeholder for full implementation)
    function try_parse_implied_do_loop(parser, arena, temp_indices, element_count, &
                                       bracket_token) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: temp_indices(:)
        integer, intent(in) :: element_count
        type(token_t), intent(in) :: bracket_token
        integer :: expr_index
        
        ! Simplified: only check for basic implied do pattern
        type(token_t) :: next1, next2
        integer :: saved_pos
        
        next1 = parser%peek()
        if (next1%kind == TK_IDENTIFIER) then
            saved_pos = parser%current_token
            next1 = parser%consume()
            next2 = parser%peek()
            
            if (next2%kind == TK_OPERATOR .and. next2%text == "=") then
                ! Simplified implied do: create basic array for now
                ! TODO: Full implied do loop implementation
                block
                    integer, allocatable :: element_indices(:)
                    allocate (element_indices(element_count))
                    element_indices = temp_indices(1:element_count)
                    expr_index = push_array_literal(arena, element_indices, &
                                                   bracket_token%line, bracket_token%column, &
                                                   syntax_style="modern")
                end block
                return
            else
                parser%current_token = saved_pos
            end if
        end if
        
        expr_index = 0  ! Not an implied do loop
    end function try_parse_implied_do_loop

    !=================================================================================
    ! POSTFIX OPERATIONS SECTION
    !=================================================================================

    ! Parse array indexing or function call postfix operator using parentheses: (...)
    function parse_array_indexing_postfix(parser, arena, base_expr) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        integer :: expr_index
        
        block
            integer, allocatable :: arg_indices(:)
            type(token_t) :: paren, op_token
            integer :: arg_count
            character(len=:), allocatable :: name_for_call
            
            arg_count = 0
            expr_index = base_expr
            
            ! Consume opening paren
            paren = parser%consume()
            
            ! Parse arguments
            op_token = parser%peek()
            if (op_token%kind /= TK_OPERATOR .or. op_token%text /= ")") then
                ! Parse first argument
                block
                    integer :: arg_index
                    arg_index = parse_range(parser, arena)
                    if (arg_index > 0) then
                        arg_count = 1
                        allocate (arg_indices(1))
                        arg_indices(1) = arg_index
                        
                        ! Parse additional arguments
                        do
                            op_token = parser%peek()
                            if (op_token%kind /= TK_OPERATOR .or. &
                                op_token%text /= ",") exit
                            
                            ! Consume comma
                            op_token = parser%consume()
                            
                            ! Parse next argument
                            arg_index = parse_range(parser, arena)
                            if (arg_index > 0) then
                                arg_indices = [arg_indices, arg_index]
                                arg_count = arg_count + 1
                            else
                                exit
                            end if
                        end do
                    end if
                end block
            end if
            
            ! Consume closing paren if present
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ")") then
                paren = parser%consume()
            end if
            
            ! Create call_or_subscript node with slice detection
            if (allocated(arg_indices)) then
                
                select type (node => arena%entries(expr_index)%node)
                type is (component_access_node)
                    name_for_call = node%component_name
                type is (identifier_node)
                    name_for_call = node%name
                class default
                    if (allocated(arg_indices)) deallocate(arg_indices)
                    return
                end select
                
                if (allocated(name_for_call)) then
                    expr_index = &
                        push_call_or_subscript_with_slice_detection(arena, &
                        name_for_call, arg_indices, &
                        paren%line, paren%column)
                end if
            end if
        end block
    end function parse_array_indexing_postfix
    
    ! Parse array indexing postfix operator using square brackets: [...]
    function parse_square_indexing_postfix(parser, arena, base_expr) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        integer :: expr_index
        
        block
            integer, allocatable :: arg_indices(:)
            type(token_t) :: bracket, op_token
            integer :: arg_count
            character(len=:), allocatable :: name_for_call
            
            arg_count = 0
            expr_index = base_expr
            
            ! Consume opening bracket
            bracket = parser%consume()
            
            ! Parse indices (use same range parser as for parentheses)
            op_token = parser%peek()
            if (op_token%kind /= TK_OPERATOR .or. op_token%text /= "]") then
                block
                    integer :: arg_index
                    arg_index = parse_range(parser, arena)
                    if (arg_index > 0) then
                        arg_count = 1
                        allocate (arg_indices(1))
                        arg_indices(1) = arg_index
                        
                        do
                            op_token = parser%peek()
                            if (op_token%kind /= TK_OPERATOR .or. &
                                op_token%text /= ",") exit
                            
                            ! Consume comma
                            op_token = parser%consume()
                            
                            ! Parse next index
                            arg_index = parse_range(parser, arena)
                            if (arg_index > 0) then
                                arg_indices = [arg_indices, arg_index]
                                arg_count = arg_count + 1
                            else
                                exit
                            end if
                        end do
                    end if
                end block
            end if
            
            ! Consume closing bracket if present
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "]") then
                bracket = parser%consume()
            end if
            
            ! Create call_or_subscript node with slice detection (same as parentheses)
            if (allocated(arg_indices)) then
                select type (node => arena%entries(expr_index)%node)
                type is (component_access_node)
                    name_for_call = node%component_name
                type is (identifier_node)
                    name_for_call = node%name
                class default
                    if (allocated(arg_indices)) deallocate(arg_indices)
                    return
                end select
                
                if (allocated(name_for_call)) then
                    expr_index = &
                        push_call_or_subscript_with_slice_detection(arena, &
                        name_for_call, arg_indices, &
                        bracket%line, bracket%column)
                end if
            end if
        end block
    end function parse_square_indexing_postfix
     
    ! Parse postfix operators on an expression
    function parse_postfix_ops(parser, arena, view, base_expr) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        integer, intent(in) :: base_expr
        integer :: expr_index
        type(token_t) :: op_token
        integer :: loop_count
        
        expr_index = base_expr
        loop_count = 0
        
        ! Handle postfix operators in a loop
        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = view_peek_token(view, parser)

            if (op_token%kind == TK_OPERATOR .and. op_token%text == "%") then
                op_token = view_consume_token(view, parser)
                expr_index = parse_component_access_postfix(parser, arena, expr_index, op_token)
                if (expr_index <= 0) exit
            else if (op_token%kind == TK_OPERATOR .and. op_token%text == "(") then
                expr_index = parse_array_indexing_postfix(parser, arena, expr_index)
            else if (op_token%kind == TK_OPERATOR .and. op_token%text == "[") then
                expr_index = parse_square_indexing_postfix(parser, arena, expr_index)
            else
                exit
            end if
        end do
    end function parse_postfix_ops

    !=================================================================================
    ! PRIMARY EXPRESSION PARSING SECTION  
    !=================================================================================

end module parser_expressions_module
