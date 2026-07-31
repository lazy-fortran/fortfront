module parser_expressions_module
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, &
        TK_OPERATOR, TK_KEYWORD, TK_WHITESPACE, TK_COMMENT, &
        TK_NEWLINE, to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: component_access_node, identifier_node, &
        range_subscript_node
    use ast_types, only: LITERAL_INTEGER, LITERAL_LOGICAL
    use ast_factory, only: push_binary_op, push_literal, push_identifier, &
        push_range_expression, push_complex_literal, &
        push_assumed_size_bounds, push_assumed_rank_bounds
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expression_helpers_module, only: parse_number_literal, &
        parse_string_literal, &
        parse_boolean_literal, &
        parse_component_access_postfix
    use parser_expression_arrays_module, only: array_parse_helpers_t, &
        parse_modern_array_literal, &
        parse_legacy_array_literal, &
        parse_array_indexing_postfix, &
        parse_square_indexing_postfix
    use parser_expression_stacks_module, only: MAX_OPERATOR_LEN, operator_entry_t, &
        operator_stack_t, operand_stack_t, &
        token_stack_t, operator_stack_clear, &
        operator_stack_push, &
        operator_stack_pop, &
        operator_stack_peek, &
        operator_stack_is_empty, &
        operator_stack_has_open_group, &
        operand_stack_clear, &
        operand_stack_push, &
        operand_stack_pop, &
        operand_stack_peek, &
        operand_stack_is_empty, &
        token_stack_clear, token_stack_push, &
        token_stack_pop
    use parser_expression_tokens_module, only: &
        token_is_boolean_literal, &
        is_prefix_operator_token, &
        is_immediate_prefix_token, &
        is_not_operator_token, &
        is_defined_operator_token, &
        token_is_terminator, &
        is_legacy_array_literal_start
    use parser_expression_operator_utils_module, only: PREC_ASSIGNMENT, PREC_RANGE, &
        PREC_LOGICAL_EQV, &
        PREC_LOGICAL_OR, &
        PREC_LOGICAL_AND, &
        PREC_NOT, &
        PREC_COMPARISON, &
        PREC_CONCAT, PREC_TERM, &
        PREC_FACTOR, PREC_POWER, &
        PREC_UNARY, PREC_POSTFIX, &
        PREFIX_MARKER_KIND, &
        get_infix_operator_entry, &
        comparison_active, &
        prepare_chained_comparison, &
        apply_prefix_stack, &
        reduce_operators_for_incoming, &
        reduce_all_operators, &
        reduce_until_group, &
        push_group_marker
    use parser_inline_instantiation_module, only: consume_inline_instantiation
    use parser_token_views_module, only: token_view_t, build_token_view, &
        view_peek_token, view_lower_token, &
        view_consume_token, view_lookahead_token
    implicit none
    private

    ! Public expression parsing interface
    public :: parse_expression
    public :: parse_range, parse_logical_eqv, parse_logical_or, parse_logical_and, &
        parse_comparison
    public :: parse_concatenation, parse_term, parse_factor, parse_power, &
        parse_unary, &
        parse_primary
    public :: parse_expression_until, parse_postfix_chain

contains

    recursive function try_parse_complex_literal(parser, arena, view) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        integer :: expr_index
        integer :: saved_pos
        type(token_t) :: open_paren
        type(token_t) :: comma_token
        type(token_t) :: close_paren
        type(token_t) :: next_token
        integer :: real_part_index
        integer :: imag_part_index

        expr_index = 0
        saved_pos = parser%current_token

        open_paren = view_peek_token(view, parser)
        if (open_paren%kind /= TK_OPERATOR) return
        if (trim(open_paren%text) /= "(") return

        open_paren = view_consume_token(view, parser)

        next_token = view_peek_token(view, parser)
        if (next_token%kind == TK_OPERATOR .and. trim(next_token%text) == "(") then
            parser%current_token = saved_pos
            return
        end if

        real_part_index = parse_comparison(parser, arena)
        if (real_part_index <= 0) then
            parser%current_token = saved_pos
            return
        end if

        comma_token = view_peek_token(view, parser)
        if (comma_token%kind /= TK_OPERATOR) then
            parser%current_token = saved_pos
            return
        end if
        if (trim(comma_token%text) /= ",") then
            parser%current_token = saved_pos
            return
        end if

        comma_token = view_consume_token(view, parser)
        imag_part_index = parse_comparison(parser, arena)
        if (imag_part_index <= 0) then
            parser%current_token = saved_pos
            return
        end if

        close_paren = view_peek_token(view, parser)
        if (close_paren%kind /= TK_OPERATOR) then
            parser%current_token = saved_pos
            return
        end if
        if (trim(close_paren%text) /= ")") then
            parser%current_token = saved_pos
            return
        end if

        close_paren = view_consume_token(view, parser)
        expr_index = push_complex_literal(arena, real_part_index, imag_part_index, &
            open_paren%line, open_paren%column)
    end function try_parse_complex_literal

    recursive function parse_operand_base(parser, arena, view) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        integer :: expr_index
        type(token_t) :: token
        character(len=:), allocatable :: lowered
        type(array_parse_helpers_t) :: array_helpers

        array_helpers%parse_comparison => parse_comparison
        array_helpers%parse_unary => parse_unary
        array_helpers%parse_logical_eqv => parse_logical_eqv
        array_helpers%parse_range => parse_range

        expr_index = 0
        token = view_peek_token(view, parser)

        select case (token%kind)
        case (TK_NUMBER)
            token = view_consume_token(view, parser)
            expr_index = parse_number_literal(token, arena)

        case (TK_STRING)
            expr_index = try_parse_postfix_boz_literal(parser, arena, view)
            if (expr_index > 0) return
            token = view_consume_token(view, parser)
            expr_index = parse_string_literal(token, arena)

        case (TK_IDENTIFIER)
            expr_index = try_parse_prefix_boz_literal(parser, arena, view)
            if (expr_index > 0) return
            token = view_consume_token(view, parser)
            lowered = to_lower(token%text)
            if (lowered == "true" .or. lowered == "false") then
                expr_index = push_literal(arena, token%text, LITERAL_LOGICAL, &
                    token%line, token%column)
            else
                expr_index = push_identifier(arena, token%text, token%line, &
                    token%column)
            end if

        case (TK_OPERATOR)
            lowered = view_lower_token(view, parser)
            if (lowered == "[") then
                token = view_consume_token(view, parser)
                expr_index = parse_modern_array_literal(parser, arena, token, &
                    array_helpers)
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
                expr_index = push_identifier(arena, token%text, token%line, &
                    token%column)
            end if

        case default
            call parser%error("Unexpected token '"//trim(token%text)//"' in expression")
            expr_index = 0
            token = view_consume_token(view, parser)
        end select
    end function parse_operand_base

    integer function try_parse_prefix_boz_literal(parser, arena, view) &
            result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        type(token_t) :: prefix_token
        type(token_t) :: literal_token
        type(token_t) :: lookahead
        character(len=:), allocatable :: literal_text

        expr_index = 0

        prefix_token = view_peek_token(view, parser)
        if (.not. is_boz_designator_token(prefix_token)) return

        lookahead = view_lookahead_token(view, parser, 1)
        if (lookahead%kind /= TK_STRING) return

        prefix_token = view_consume_token(view, parser)
        literal_token = view_consume_token(view, parser)

        literal_text = combine_boz_literal_text(prefix_token%text, &
            literal_token%text)
        expr_index = push_literal(arena, literal_text, LITERAL_INTEGER, &
            prefix_token%line, prefix_token%column)
    end function try_parse_prefix_boz_literal

    integer function try_parse_postfix_boz_literal(parser, arena, view) &
            result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        type(token_t) :: literal_token
        type(token_t) :: suffix_token
        type(token_t) :: lookahead
        character(len=:), allocatable :: literal_text

        expr_index = 0

        literal_token = view_peek_token(view, parser)
        if (literal_token%kind /= TK_STRING) return

        lookahead = view_lookahead_token(view, parser, 1)
        if (.not. is_boz_designator_token(lookahead)) return

        literal_token = view_consume_token(view, parser)
        suffix_token = view_consume_token(view, parser)

        literal_text = combine_boz_literal_text(literal_token%text, &
            suffix_token%text)
        expr_index = push_literal(arena, literal_text, LITERAL_INTEGER, &
            literal_token%line, literal_token%column)
    end function try_parse_postfix_boz_literal

    logical function is_boz_designator_token(token) result(is_boz)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_boz = .false.
        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))
        if (len_trim(lowered) /= 1) return

        select case (lowered(1:1))
        case ("b", "o", "z", "x")
            is_boz = .true.
        case default
            is_boz = .false.
        end select
    end function is_boz_designator_token

    function combine_boz_literal_text(first_text, second_text) result(literal)
        character(len=*), intent(in) :: first_text
        character(len=*), intent(in) :: second_text
        character(len=:), allocatable :: literal

        literal = trim(first_text)//trim(second_text)
    end function combine_boz_literal_text

    subroutine handle_closing_paren(parser, arena, view, operators, operands, &
            prefix_stack, token, expect_operand)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(token_stack_t), intent(inout) :: prefix_stack
        type(token_t), intent(inout) :: token
        logical, intent(inout) :: expect_operand
        type(operator_entry_t) :: op_entry
        integer :: current_index

        call reduce_until_group(operators, operands, arena)
        op_entry = operator_stack_pop(operators)
        if (prefix_stack%size > 0) then
            if (prefix_stack%values(prefix_stack%size)%kind == PREFIX_MARKER_KIND) then
                block
                    type(token_t) :: discarded_marker
                    discarded_marker = token_stack_pop(prefix_stack)
                end block
                if (.not. operand_stack_is_empty(operands)) then
                    current_index = operand_stack_pop(operands)
                    current_index = apply_prefix_stack(arena, prefix_stack, &
                        current_index)
                    call operand_stack_push(operands, current_index)
                end if
            end if
        end if
        token = view_consume_token(view, parser)
        expect_operand = .false.
    end subroutine handle_closing_paren

    recursive subroutine handle_operand_token(parser, arena, view, operands, &
            prefix_stack, token, &
            expect_operand)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        type(operand_stack_t), intent(inout) :: operands
        type(token_stack_t), intent(inout) :: prefix_stack
        type(token_t), intent(in) :: token
        logical, intent(inout) :: expect_operand
        integer :: current_index
        type(array_parse_helpers_t) :: array_helpers

        array_helpers%parse_comparison => parse_comparison
        array_helpers%parse_unary => parse_unary
        array_helpers%parse_logical_eqv => parse_logical_eqv
        array_helpers%parse_range => parse_range

        if (is_legacy_array_literal_start(parser, view)) then
            current_index = parse_legacy_array_literal(parser, arena, array_helpers)
            current_index = apply_prefix_stack(arena, prefix_stack, current_index)
            current_index = parse_postfix_ops(parser, arena, view, current_index)
            call operand_stack_push(operands, current_index)
            expect_operand = .false.
            return
        end if

        current_index = parse_operand_base(parser, arena, view)
        if (current_index > 0) then
            current_index = parse_postfix_ops(parser, arena, view, current_index)
            ! A leading unary sign binds looser than ** (Fortran precedence,
            ! issue #215): -a**b parses as -(a**b), not (-a)**b. When a sign is
            ! pending and the operand is immediately followed by **, fold the
            ! whole power chain first, then apply the prefix.
            if (prefix_stack_has_sign(prefix_stack) .and. &
                peek_is_power_op(view, parser)) then
                current_index = fold_power_after_sign(parser, arena, view, &
                    current_index)
            end if
            current_index = apply_prefix_stack(arena, prefix_stack, current_index)
            call operand_stack_push(operands, current_index)
        else
            call token_stack_clear(prefix_stack)
        end if
        expect_operand = .false.
    end subroutine handle_operand_token

    logical function prefix_stack_has_sign(prefix_stack)
        type(token_stack_t), intent(in) :: prefix_stack

        prefix_stack_has_sign = .false.
        if (prefix_stack%size <= 0) return
        prefix_stack_has_sign = &
            prefix_stack%values(prefix_stack%size)%kind /= PREFIX_MARKER_KIND
    end function prefix_stack_has_sign

    logical function peek_is_power_op(view, parser)
        type(token_view_t), intent(in) :: view
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        token = view_peek_token(view, parser)
        peek_is_power_op = token%kind == TK_OPERATOR .and. trim(token%text) == "**"
    end function peek_is_power_op

    recursive function fold_power_after_sign(parser, arena, view, base_index) &
            result(result_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_view_t), intent(in) :: view
        integer, intent(in) :: base_index
        integer :: result_index
        type(token_t) :: power_token
        integer :: exponent_index

        power_token = view_consume_token(view, parser)
        exponent_index = parse_expression_with_precedence(parser, arena, &
            PREC_POWER)
        result_index = push_binary_op(arena, base_index, exponent_index, &
            power_token%text, power_token%line, &
            power_token%column)
    end function fold_power_after_sign

    subroutine handle_infix_operator(parser, view, operators, operands, arena, &
            token, minimum_precedence, expect_operand, &
            should_exit)
        type(parser_state_t), intent(inout) :: parser
        type(token_view_t), intent(in) :: view
        type(operator_stack_t), intent(inout) :: operators
        type(operand_stack_t), intent(inout) :: operands
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(inout) :: token
        integer, intent(in) :: minimum_precedence
        logical, intent(inout) :: expect_operand
        logical, intent(out) :: should_exit
        type(operator_entry_t) :: op_entry
        logical :: chain_prepared

        should_exit = .false.
        op_entry = get_infix_operator_entry(token)
        if (op_entry%symbol == "") then
            should_exit = .true.
            return
        end if
        if (op_entry%symbol == "/") then
            block
                type(token_t) :: lookahead
                lookahead = view_lookahead_token(view, parser, 1)
                if (trim(lookahead%text) == ")") then
                    should_exit = .true.
                    return
                end if
            end block
        end if

        if (op_entry%precedence < minimum_precedence) then
            should_exit = .true.
            return
        end if

        chain_prepared = .false.
        if (op_entry%precedence == PREC_COMPARISON) then
            call prepare_chained_comparison(operators, operands, arena, token, &
                chain_prepared)
        end if

        if (op_entry%precedence == PREC_COMPARISON .and. &
            .not. chain_prepared .and. comparison_active(operators)) then
            block
                use, intrinsic :: iso_fortran_env, only: error_unit
                write (error_unit, '(A)') &
                    "ERROR: Chained comparisons are not supported in Fortran"
                write (error_unit, '(A)') &
                    "  Suggestion: Use logical operators: (a < b) .and. (b < c)"
                write (error_unit, '(A,I0,A,I0)') &
                    "  Location: line ", token%line, ", column ", token%column
            end block
            call parser%error("Chained comparisons are not supported in Fortran", &
                suggestion="Use logical operators: (a < b) .and. (b < c)")
            should_exit = .true.
            return
        end if

        call reduce_operators_for_incoming(operators, operands, arena, op_entry)
        call operator_stack_push(operators, op_entry)
        token = view_consume_token(view, parser)
        expect_operand = .true.
    end subroutine handle_infix_operator

    recursive function parse_expression_with_precedence(parser, arena, &
            minimum_precedence, &
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
        logical :: expect_operand, should_exit
        type(token_t) :: token
        integer :: iteration_count
        integer, parameter :: MAX_ITERATIONS = 100000

        call operator_stack_clear(operators)
        call operand_stack_clear(operands)
        call token_stack_clear(prefix_stack)
        call build_token_view(view, parser)
        expr_index = 0
        expect_operand = .true.
        iteration_count = 0

        main_loop: do while (iteration_count < MAX_ITERATIONS)
        iteration_count = iteration_count + 1
        token = view_peek_token(view, parser)
        if (token%kind == TK_EOF) exit main_loop

        if (token%kind == TK_COMMENT) then
            if (is_trailing_comment(parser)) then
                exit main_loop
            else
                token = view_consume_token(view, parser)
                cycle
            end if
        else if (is_expression_trivia_token(token)) then
            token = view_consume_token(view, parser)
            cycle
        end if

        if (.not. expect_operand) then
            if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                if (operator_stack_has_open_group(operators)) then
                    call handle_closing_paren(parser, arena, view, operators, &
                        operands, prefix_stack, token, &
                        expect_operand)
                    cycle
                else
                    exit main_loop
                end if
            end if
        end if

        if (token_is_terminator(token, terminators)) exit main_loop

        if (expect_operand) then
            if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                if (is_legacy_array_literal_start(parser, view)) then
                    call handle_operand_token(parser, arena, view, operands, &
                        prefix_stack, token, &
                        expect_operand)
                    cycle
                end if
            end if

            ! Handle .not. operator with correct precedence (lower than comparison)
            ! ISO/IEC 1539-1:2018 Table 10.1: .not. binds looser than relational ops
            if (is_not_operator_token(token)) then
                ! Push virtual 0 operand for unary .not.
                call operand_stack_push(operands, 0)
                ! Create operator entry for .not. with PREC_NOT precedence
                block
                    type(operator_entry_t) :: not_entry
                    not_entry%symbol = token%text
                    not_entry%precedence = PREC_NOT
                    not_entry%right_associative = .true.
                    not_entry%token = view_consume_token(view, parser)
                    call reduce_operators_for_incoming(operators, operands, arena, &
                        not_entry)
                    call operator_stack_push(operators, not_entry)
                end block
                expect_operand = .true.
                cycle
            end if

            ! Handle a user-defined operator in prefix position (unary defined
            ! operator, e.g. .negation. x). Emit a binary op with a virtual 0
            ! left operand and the operand on the right, exactly like .not., so
            ! the single operand is a resolvable arena node (issue #2838).
            if (is_defined_operator_token(token)) then
                call operand_stack_push(operands, 0)
                block
                    type(operator_entry_t) :: defined_entry
                    defined_entry%symbol = token%text
                    defined_entry%precedence = PREC_UNARY
                    defined_entry%right_associative = .true.
                    defined_entry%token = view_consume_token(view, parser)
                    call reduce_operators_for_incoming(operators, operands, arena, &
                        defined_entry)
                    call operator_stack_push(operators, defined_entry)
                end block
                expect_operand = .true.
                cycle
            end if

            ! Handle high-precedence prefix operators (+, -) that bind immediately
            if (is_immediate_prefix_token(token)) then
                call token_stack_push(prefix_stack, &
                    view_consume_token(view, parser))
                cycle
            end if

            if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                block
                    integer :: complex_index
                    complex_index = try_parse_complex_literal(parser, arena, view)
                    if (complex_index > 0) then
                        complex_index = apply_prefix_stack(arena, prefix_stack, &
                            complex_index)
                        complex_index = parse_postfix_ops(parser, arena, view, &
                            complex_index)
                        call operand_stack_push(operands, complex_index)
                        expect_operand = .false.
                        cycle
                    end if
                end block
            end if

            if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                block
                    type(token_t) :: marker
                    marker%text = ""
                    marker%kind = PREFIX_MARKER_KIND
                    marker%line = token%line
                    marker%column = token%column
                    call token_stack_push(prefix_stack, marker)
                end block
                call push_group_marker(operators, view_consume_token(view, parser))
                expect_operand = .true.
                cycle
            end if

            call handle_operand_token(parser, arena, view, operands, &
                prefix_stack, token, expect_operand)
            cycle
        else
            call handle_infix_operator(parser, view, operators, operands, arena, &
                token, minimum_precedence, expect_operand, &
                should_exit)
            if (should_exit) exit main_loop
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
function parse_expression(tokens, arena, parent_parser) result(expr_index)
    type(token_t), intent(in) :: tokens(:)
    type(ast_arena_t), intent(inout) :: arena
    type(parser_state_t), intent(in), optional :: parent_parser
    integer :: expr_index
    type(parser_state_t) :: parser

    parser = create_parser_state(tokens)
    if (present(parent_parser)) then
        if (associated(parent_parser%diagnostic_sink)) then
            parser%diagnostic_sink => parent_parser%diagnostic_sink
        end if
    end if
    expr_index = parse_range(parser, arena)
end function parse_expression

! Parse logical EQV/NEQV operators (lowest precedence)
recursive function parse_logical_eqv(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_LOGICAL_EQV)
end function parse_logical_eqv

! Parse logical OR operators
recursive function parse_logical_or(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_LOGICAL_OR)
end function parse_logical_or

! Parse logical AND operators
recursive function parse_logical_and(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_LOGICAL_AND)
end function parse_logical_and

! Parse comparison operators
recursive function parse_comparison(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_COMPARISON)
end function parse_comparison

! Parse string concatenation operator (//) - Issue #214
recursive function parse_concatenation(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_CONCAT)
end function parse_concatenation
! Parse addition and subtraction
recursive function parse_term(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_TERM)
end function parse_term

! Parse multiplication and division
recursive function parse_factor(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_FACTOR)
end function parse_factor

! Parse exponentiation (**) - right-associative
recursive function parse_power(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_POWER)
end function parse_power

! Parse unary operators (+, -, .NOT.) - Issue #215
recursive function parse_unary(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_UNARY)
end function parse_unary

! Parse primary expressions (literals, identifiers, parentheses)
recursive function parse_primary(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index

    expr_index = parse_expression_with_precedence(parser, arena, PREC_POSTFIX)
end function parse_primary

recursive function parse_expression_until(parser, arena, terminators) &
        result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    character(len=*), intent(in), optional :: terminators(:)
    integer :: expr_index
    character(len=MAX_OPERATOR_LEN), allocatable :: local_terms(:)

    if (present(terminators)) then
        if (size(terminators) > 0) then
            allocate (character(len=MAX_OPERATOR_LEN) :: &
                local_terms(size(terminators)))
            local_terms = terminators
            expr_index = parse_expression_with_precedence(parser, arena, &
                PREC_RANGE + 1, &
                local_terms)
        else
            expr_index = parse_expression_with_precedence(parser, arena, &
                PREC_RANGE + 1)
        end if
    else
        expr_index = parse_expression_with_precedence(parser, arena, &
            PREC_RANGE + 1)
    end if
end function parse_expression_until

recursive function parse_postfix_chain(parser, arena, base_expr) &
        result(expr_index)
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

logical function is_trailing_comment(parser) result(is_trailing)
    type(parser_state_t), intent(in) :: parser
    integer :: pos, i

    is_trailing = .false.
    if (.not. associated(parser%tokens)) return
    pos = parser%current_token + 1
    if (pos > size(parser%tokens)) return

    do i = pos, size(parser%tokens)
        select case (parser%tokens(i)%kind)
        case (TK_WHITESPACE)
            cycle
        case (TK_NEWLINE, TK_EOF)
            is_trailing = .true.
            return
        case default
            return
        end select
    end do
    is_trailing = .true.
end function is_trailing_comment

pure logical function is_expression_trivia_token(token) result(is_trivia)
    type(token_t), intent(in) :: token

    select case (token%kind)
    case (TK_WHITESPACE, TK_COMMENT)
        is_trivia = .true.
    case default
        is_trivia = .false.
    end select
end function is_expression_trivia_token

!=================================================================================
! RANGE EXPRESSION PARSING SECTION
!=================================================================================

! Parse range/slice operator (:) - lowest precedence after logical operators
recursive function parse_range(parser, arena) result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    integer :: expr_index
    integer :: right_index
    integer :: stride_index
    logical :: double_colon
    type(token_t) :: op_token

    op_token = parser%peek()
    ! Handle assumed-rank (..) - Fortran 2018 feature for SELECT RANK
    if (op_token%kind == TK_OPERATOR .and. op_token%text == "..") then
        op_token = parser%consume()
        expr_index = push_assumed_rank_bounds(arena, &
            line=op_token%line, &
            column=op_token%column)
        return
    end if

    if (op_token%kind == TK_OPERATOR .and. op_token%text == "*") then
        op_token = parser%consume()
        expr_index = push_assumed_size_bounds(arena, &
            line=op_token%line, &
            column=op_token%column)
        return
    end if

    if (op_token%kind == TK_OPERATOR .and. &
        (op_token%text == ":" .or. op_token%text == "::")) then
        op_token = parser%consume()
        expr_index = 0
        double_colon = op_token%text == "::"

        if (double_colon) then
            right_index = 0
        else if (.not. parser%is_at_end()) then
            block
                type(token_t) :: next_tok
                next_tok = parser%peek()
                if (.not. (next_tok%kind == TK_OPERATOR .and. &
                    (next_tok%text == ")" .or. next_tok%text == "]" .or. &
                    next_tok%text == &
                    ","))) then
                    right_index = parse_expression_with_precedence(parser, arena, &
                        PREC_ASSIGNMENT)
                else
                    right_index = 0
                end if
            end block
        else
            right_index = 0
        end if

        if (double_colon) then
            stride_index = parse_logical_eqv(parser, arena)
        else
            stride_index = parse_stride_component(parser, arena)
        end if

        expr_index = push_range_expression(arena, expr_index, right_index, &
            stride_index=stride_index, &
            line=op_token%line, &
            column=op_token%column)
        return
    end if

    ! Use PREC_ASSIGNMENT to allow keyword arguments in calls (e.g., name=value)
    expr_index = parse_expression_with_precedence(parser, arena, PREC_ASSIGNMENT)

    if (.not. parser%is_at_end()) then
        op_token = parser%peek()
        if (op_token%kind == TK_OPERATOR .and. &
            (op_token%text == ":" .or. op_token%text == "::")) then
            op_token = parser%consume()
            double_colon = op_token%text == "::"

            if (double_colon) then
                right_index = 0
            else if (.not. parser%is_at_end()) then
                block
                    type(token_t) :: next_tok
                    next_tok = parser%peek()
                    if (.not. (next_tok%kind == TK_OPERATOR .and. &
                        (next_tok%text == ")" .or. next_tok%text &
                        == "]" .or. &
                        next_tok%text == &
                        ","))) then
                        right_index = parse_expression_with_precedence( &
                            parser, arena, PREC_ASSIGNMENT)
                    else
                        right_index = 0
                    end if
                end block
            else
                right_index = 0
            end if

            if (double_colon) then
                stride_index = parse_logical_eqv(parser, arena)
            else
                stride_index = parse_stride_component(parser, arena)
            end if

            expr_index = push_range_expression(arena, expr_index, &
                right_index, &
                stride_index=stride_index, &
                line=op_token%line, &
                column=op_token%column)
        end if
    end if
end function parse_range
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

!=================================================================================
! POSTFIX OPERATIONS SECTION
!=================================================================================

! Parse array indexing or function call postfix operator using parentheses: (...)
! Parse postfix operators on an expression
recursive function parse_postfix_ops(parser, arena, view, base_expr) &
        result(expr_index)
    type(parser_state_t), intent(inout) :: parser
    type(ast_arena_t), intent(inout) :: arena
    type(token_view_t), intent(in) :: view
    integer, intent(in) :: base_expr
    integer :: expr_index
    type(token_t) :: op_token
    integer :: loop_count
    type(array_parse_helpers_t) :: array_helpers
    character(len=:), allocatable :: instantiation_text

    array_helpers%parse_comparison => parse_comparison
    array_helpers%parse_unary => parse_unary
    array_helpers%parse_logical_eqv => parse_logical_eqv
    array_helpers%parse_range => parse_range

    expr_index = base_expr
    loop_count = 0

    ! Handle postfix operators in a loop
    do while (.not. parser%is_at_end() .and. loop_count < 1000)
        loop_count = loop_count + 1
        op_token = view_peek_token(view, parser)

        if (op_token%kind == TK_OPERATOR .and. &
            (op_token%text == "%" .or. op_token%text == ".")) then
            op_token = view_consume_token(view, parser)
            expr_index = parse_component_access_postfix(parser, arena, &
                expr_index, op_token)
            if (expr_index <= 0) exit
        else if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "{" .or. op_token%text == "^")) then
            call consume_inline_instantiation(parser, instantiation_text, view)
            if (allocated(instantiation_text)) then
                call append_inline_instantiation_to_name(arena, expr_index, &
                    instantiation_text)
                deallocate (instantiation_text)
            end if
        else if (op_token%kind == TK_OPERATOR .and. op_token%text == "(") then
            expr_index = parse_array_indexing_postfix(parser, arena, expr_index, &
                array_helpers)
        else if (op_token%kind == TK_OPERATOR .and. op_token%text == "[") then
            expr_index = parse_square_indexing_postfix(parser, arena, expr_index, &
                array_helpers)
        else
            exit
        end if
    end do
end function parse_postfix_ops

subroutine append_inline_instantiation_to_name(arena, expr_index, inst_text)
    use ast_nodes_core, only: identifier_node, component_access_node
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: expr_index
    character(len=*), intent(in) :: inst_text

    if (.not. arena%has_node_at(expr_index)) return

    if (.not. allocated(arena%entries(expr_index)%node)) return

    select type (node => arena%entries(expr_index)%node)
        type is (identifier_node)
        if (allocated(node%name)) then
            node%name = node%name//inst_text
        end if
        type is (component_access_node)
        if (allocated(node%component_name)) then
            node%component_name = node%component_name//inst_text
        end if
    class default
    end select
end subroutine append_inline_instantiation_to_name

!=================================================================================
! PRIMARY EXPRESSION PARSING SECTION
!=================================================================================

end module parser_expressions_module
