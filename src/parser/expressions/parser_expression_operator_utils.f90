module parser_expression_operator_utils_module
    use lexer_core, only: token_t, TK_OPERATOR, to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_binary_op, push_literal
    use ast_types, only: LITERAL_INTEGER
    use parser_expression_stacks_module, only: operator_entry_t, operator_stack_t, &
        operand_stack_t, token_stack_t, &
        operator_stack_push, &
        operator_stack_pop, &
        operator_stack_peek, &
        operator_stack_is_empty, &
        operand_stack_push, &
        operand_stack_pop, &
        operand_stack_peek, &
        token_stack_pop
    implicit none
    private

    integer, parameter, public :: PREC_RANGE = 10
    integer, parameter, public :: PREC_ASSIGNMENT = 15
    integer, parameter, public :: PREC_LOGICAL_EQV = 20
    integer, parameter, public :: PREC_LOGICAL_OR = 30
    integer, parameter, public :: PREC_LOGICAL_AND = 40
    ! ISO/IEC 1539-1:2018 Table 10.1: .not. has lower precedence than comparison
    integer, parameter, public :: PREC_NOT = 45
    integer, parameter, public :: PREC_COMPARISON = 50
    integer, parameter, public :: PREC_CONCAT = 60
    integer, parameter, public :: PREC_TERM = 70
    integer, parameter, public :: PREC_FACTOR = 80
    integer, parameter, public :: PREC_POWER = 90
    integer, parameter, public :: PREC_UNARY = 95
    integer, parameter, public :: PREC_POSTFIX = 110

    integer, parameter, public :: PREFIX_MARKER_KIND = -999

    public :: get_infix_operator_entry
    public :: comparison_active
    public :: prepare_chained_comparison
    public :: apply_prefix_stack
    public :: reduce_operators_for_incoming
    public :: reduce_all_operators
    public :: reduce_until_group
    public :: push_group_marker

contains

    function get_infix_operator_entry(token) result(entry)
        type(token_t), intent(in) :: token
        type(operator_entry_t) :: entry
        character(len=:), allocatable :: lowered
        integer :: last_char
        integer :: idx
        logical :: is_defined_op

        entry = operator_entry_t()
        if (token%kind /= TK_OPERATOR) return

        lowered = to_lower(token%text)
        is_defined_op = .false.
        last_char = len_trim(lowered)

        select case (lowered)
        case ("=")
            entry%symbol = token%text
            entry%precedence = PREC_ASSIGNMENT
            entry%right_associative = .true.
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
            ! ISO/IEC 1539-1:2018 Table 10.1: old-style comparison operators
            ! have same precedence as modern comparison operators
        case (".eq.", ".ne.", ".lt.", ".le.", ".gt.", ".ge.")
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
            if (last_char >= 4) then
                if (lowered(1:1) == '.' .and. &
                    lowered(last_char:last_char) == '.') then
                    is_defined_op = .true.
                    do idx = 2, last_char - 1
                        select case (lowered(idx:idx))
                        case ('a':'z', '0':'9', '_')
                        case default
                            is_defined_op = .false.
                            exit
                        end select
                    end do
                end if
            end if
            if (.not. is_defined_op) return
            entry%symbol = token%text
            entry%precedence = PREC_LOGICAL_AND
        end select

        entry%token_line = token%line
        entry%token_column = token%column
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

    subroutine prepare_chained_comparison(operators, operands, arena, token, prepared)
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        logical, intent(out) :: prepared
        type(operator_entry_t) :: top_entry
        integer :: shared_operand
        type(token_t) :: logical_token
        type(operator_entry_t) :: logical_entry

        prepared = .false.

        if (.not. comparison_active(operators)) return
        if (operators%size <= 0) return
        top_entry = operator_stack_peek(operators)
        if (top_entry%precedence /= PREC_COMPARISON) return
        if (operands%size < 2) return

        shared_operand = operand_stack_peek(operands)
        if (shared_operand <= 0) return

        call reduce_single_operator(operators, operands, arena)

        logical_token = token
        logical_token%text = ".and."
        logical_token%kind = TK_OPERATOR
        logical_entry = get_infix_operator_entry(logical_token)
        logical_entry%token_line = logical_token%line
        logical_entry%token_column = logical_token%column

        call reduce_operators_for_incoming(operators, operands, arena, logical_entry)
        call operator_stack_push(operators, logical_entry)
        call operand_stack_push(operands, shared_operand)

        prepared = .true.
    end subroutine prepare_chained_comparison

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
        do while (prefix_stack%size > 0)
            token = prefix_stack%values(prefix_stack%size)
            if (token%kind == PREFIX_MARKER_KIND) exit

            token = token_stack_pop(prefix_stack)
            lowered = to_lower(token%text)
            select case (lowered)
            case ("-")
                zero_index = create_zero_literal(arena, token)
                result_index = push_binary_op(arena, zero_index, result_index, &
                    token%text, token%line, token%column)
            case ("+")
                cycle
                ! Note: .not. is now handled as an operator with PREC_NOT precedence
                ! in parse_expression_with_precedence, not as a prefix operator
            case default
                cycle
            end select
        end do
    end function apply_prefix_stack

    subroutine reduce_single_operator(operators, operands, arena)
        use ast_factory, only: push_assignment
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena
        type(operator_entry_t) :: op_entry
        integer :: right_index, left_index, result_index

        op_entry = operator_stack_pop(operators)
        if (op_entry%is_group) return

        right_index = operand_stack_pop(operands)
        left_index = operand_stack_pop(operands)

        if (right_index <= 0 .or. left_index < 0) then
            call operand_stack_push(operands, 0)
            return
        end if

        ! Handle = operator specially - create assignment node instead of binary_op
        if (trim(op_entry%symbol) == "=") then
            result_index = push_assignment(arena, left_index, right_index, &
                op_entry%token_line, op_entry%token_column)
        else
            result_index = push_binary_op(arena, left_index, right_index, &
                op_entry%symbol, &
                op_entry%token_line, op_entry%token_column)
        end if

        call operand_stack_push(operands, result_index)
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

        entry = operator_entry_t(symbol="(", precedence=0, is_group=.true., &
            token_line=token%line, token_column=token%column)
        call operator_stack_push(operators, entry)
    end subroutine push_group_marker

end module parser_expression_operator_utils_module
