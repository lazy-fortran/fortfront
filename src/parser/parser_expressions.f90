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

    ! Public expression parsing interface
    public :: parse_expression
    public :: parse_range, parse_logical_eqv, parse_logical_or, parse_logical_and, parse_comparison
    public :: parse_concatenation, parse_term, parse_factor, parse_power, parse_unary, parse_primary

contains

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
        integer :: right_index
        type(token_t) :: op_token
        integer :: loop_count

        expr_index = parse_logical_or(parser, arena)
        loop_count = 0

        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == ".eqv." .or. op_token%text == ".neqv.")) then
                op_token = parser%consume()
                right_index = parse_logical_or(parser, arena)
                if (right_index > 0) then
                    expr_index = push_binary_op(arena, expr_index, right_index, &
                        op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_eqv

    ! Parse logical OR operators
    function parse_logical_or(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token
        integer :: loop_count

        expr_index = parse_logical_and(parser, arena)
        loop_count = 0

        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".or.") then
                op_token = parser%consume()
                right_index = parse_logical_and(parser, arena)
                if (right_index > 0) then
                    expr_index = push_binary_op(arena, expr_index, right_index, &
                        op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_or

    ! Parse logical AND operators
    function parse_logical_and(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token
        integer :: loop_count

        expr_index = parse_comparison(parser, arena)
        loop_count = 0

        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".and.") then
                op_token = parser%consume()
                right_index = parse_comparison(parser, arena)
                if (right_index > 0) then
                    expr_index = push_binary_op(arena, expr_index, right_index, &
                        op_token%text)
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function parse_logical_and

    ! Parse comparison operators
    function parse_comparison(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_concatenation(parser, arena)

        ! Make comparison operators non-associative (Issue #216)
        ! Parse at most ONE comparison operator
        if (.not. parser%is_at_end()) then
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "==" .or. op_token%text == "/=" .or. &
                 op_token%text == "<=" .or. op_token%text == ">=" .or. &
                 op_token%text == "<" .or. op_token%text == ">")) then
                op_token = parser%consume()
                right_index = parse_concatenation(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, &
                                             op_token%text, op_token%line, &
                                             op_token%column)
            end if
        end if
    end function parse_comparison

    ! Parse string concatenation operator (//) - Issue #214
    function parse_concatenation(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token
        integer :: loop_count

        expr_index = parse_term(parser, arena)
        loop_count = 0

        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "//") then
                op_token = parser%consume()
                right_index = parse_term(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, &
                                             op_token%text, op_token%line, &
                                             op_token%column)
            else
                exit
            end if
        end do
    end function parse_concatenation
    ! Parse addition and subtraction
    function parse_term(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token
        integer :: loop_count

        expr_index = parse_factor(parser, arena)
        loop_count = 0

        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "+" .or. op_token%text == "-")) then
                op_token = parser%consume()
                right_index = parse_factor(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, &
                                             op_token%text, op_token%line, &
                                             op_token%column)
            else
                exit
            end if
        end do
    end function parse_term

    ! Parse multiplication and division
    function parse_factor(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token
        integer :: loop_count

        expr_index = parse_power(parser, arena)
        loop_count = 0

        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "*" .or. op_token%text == "/")) then
                op_token = parser%consume()
                right_index = parse_power(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, &
                                             op_token%text, op_token%line, &
                                             op_token%column)
            else
                exit
            end if
        end do
    end function parse_factor

    ! Parse exponentiation (**) - right-associative
    function parse_power(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer, allocatable :: operands(:)
        type(token_t), allocatable :: operators(:)
        type(token_t), allocatable :: base_unary(:)
        type(token_t), allocatable :: unary_ops(:)
        type(token_t) :: op_token
        integer :: idx

        call gather_unary_tokens(parser, base_unary)
        expr_index = parse_primary(parser, arena)
        if (expr_index <= 0) then
            return
        end if

        call push_int(operands, expr_index)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "**") then
                op_token = parser%consume()
                call push_token(operators, op_token)
                call gather_unary_tokens(parser, unary_ops)
                expr_index = parse_primary(parser, arena)
                expr_index = apply_unary_stack(arena, expr_index, unary_ops)
                if (expr_index <= 0) exit
                call push_int(operands, expr_index)
            else
                exit
            end if
        end do

        if (.not. allocated(operators)) then
            expr_index = apply_unary_stack(arena, operands(1), base_unary)
            return
        end if

        expr_index = operands(size(operands))
        do idx = size(operands) - 1, 1, -1
            expr_index = push_binary_op(arena, operands(idx), expr_index, &
                operators(idx)%text, operators(idx)%line, operators(idx)%column)
        end do

        expr_index = apply_unary_stack(arena, expr_index, base_unary)
    end function parse_power

    ! Parse unary operators (+, -, .NOT.) - Issue #215
    function parse_unary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t), allocatable :: unary_ops(:)

        call gather_unary_tokens(parser, unary_ops)
        expr_index = parse_primary(parser, arena)
        expr_index = apply_unary_stack(arena, expr_index, unary_ops)
    end function parse_unary
    subroutine gather_unary_tokens(parser, unary_ops)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), allocatable, intent(out) :: unary_ops(:)
        type(token_t) :: current
        character(len=:), allocatable :: lowered

        allocate(unary_ops(0))

        do while (.not. parser%is_at_end())
            current = parser%peek()
            if (current%kind == TK_OPERATOR) then
                lowered = to_lower(current%text)
                if (lowered == "-" .or. lowered == "+" .or. lowered == ".not.") then
                    current = parser%consume()
                    call push_token(unary_ops, current)
                    cycle
                end if
            end if
            exit
        end do
    end subroutine gather_unary_tokens

    function apply_unary_stack(arena, expr_index, unary_ops) result(result_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        type(token_t), allocatable, intent(in) :: unary_ops(:)
        integer :: result_index
        integer :: idx
        integer :: zero_index
        character(len=:), allocatable :: lowered

        result_index = expr_index
        if (result_index <= 0) then
            return
        end if
        if (.not. allocated(unary_ops)) then
            return
        end if
        if (size(unary_ops) == 0) then
            return
        end if

        do idx = size(unary_ops), 1, -1
            lowered = to_lower(unary_ops(idx)%text)
            select case (lowered)
            case ("-")
                zero_index = push_literal(arena, "0", LITERAL_INTEGER, &
                    unary_ops(idx)%line, unary_ops(idx)%column)
                result_index = push_binary_op(arena, zero_index, result_index, "-")
            case ("+")
                cycle
            case (".not.")
                result_index = push_binary_op(arena, 0, result_index, ".not.")
            end select
        end do
    end function apply_unary_stack

    subroutine push_int(stack, value)
        integer, allocatable, intent(inout) :: stack(:)
        integer, intent(in) :: value
        integer, allocatable :: new_stack(:)

        if (.not. allocated(stack)) then
            allocate(stack(1))
            stack(1) = value
        else
            allocate(new_stack(size(stack)+1))
            if (size(stack) > 0) then
                new_stack(1:size(stack)) = stack
            end if
            new_stack(size(new_stack)) = value
            call move_alloc(new_stack, stack)
        end if
    end subroutine push_int

    subroutine push_token(stack, value)
        type(token_t), allocatable, intent(inout) :: stack(:)
        type(token_t), intent(in) :: value
        type(token_t), allocatable :: new_stack(:)

        if (.not. allocated(stack)) then
            allocate(stack(1))
            stack(1) = value
        else
            allocate(new_stack(size(stack)+1))
            if (size(stack) > 0) then
                new_stack(1:size(stack)) = stack
            end if
            new_stack(size(new_stack)) = value
            call move_alloc(new_stack, stack)
        end if
    end subroutine push_token

    ! Parse primary expressions (literals, identifiers, parentheses)  
    recursive function parse_primary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t) :: current

        current = parser%peek()

        select case (current%kind)
        case (TK_EOF)
            ! End of file reached - return invalid expression index
            expr_index = 0
            return
            
        case (TK_NUMBER)
            ! Parse number literal
            current = parser%consume()
            expr_index = parse_number_literal(current, arena)

        case (TK_STRING)
            ! Parse string literal
            current = parser%consume()
            expr_index = parse_string_literal(current, arena)

        case (TK_IDENTIFIER)
            ! Parse identifier or function call
            current = parser%consume()
            expr_index = parse_identifier_or_call(parser, arena, current)

        case (TK_OPERATOR)
            ! Check for parentheses or array literal
            if (current%text == "(") then
                ! Check for legacy array literal: (/ ... /) FIRST before consuming
                block
                    type(token_t) :: next_token
                    logical :: is_legacy_array
                    is_legacy_array = .false.
                    
                    ! Manual lookahead - check if next token is "/"
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        next_token = parser%tokens(parser%current_token + 1)
                        if (next_token%kind == TK_OPERATOR .and. next_token%text == "/") then
                            is_legacy_array = .true.
                        end if
                    end if
                    
                    if (is_legacy_array) then
                        ! Legacy array literal: (/ ... /)
                        expr_index = parse_legacy_array_literal(parser, arena)
                    else
                        ! Regular parenthesized expression
                        current = parser%consume()
                        expr_index = parse_range(parser, arena)  ! parse the expression inside
                        current = parser%peek()
                        if (current%text == ")") then
                            current = parser%consume()
                        end if
                    end if
                end block
            else if (current%text == "[") then
                ! Array literal: [1, 2, 3]
                expr_index = parse_modern_array_literal(parser, arena)
            else if (current%text == ".true." .or. current%text == ".false.") then
                ! Handle boolean literals as single tokens
                current = parser%consume()
                expr_index = parse_boolean_literal(current, arena)
            else
                ! Unrecognized operator - create error node
                expr_index = push_literal(arena, &
                    "!ERROR: Unrecognized operator '"//current%text//"'", &
                    LITERAL_STRING, current%line, current%column)
                current = parser%consume()
            end if

        case (TK_KEYWORD)
            ! Handle logical constants
            current = parser%consume()
            if (current%text == ".true." .or. current%text == ".false.") then
                expr_index = parse_boolean_literal(current, arena)
            else if (current%text == "real" .or. current%text == "integer" .or. &
                     current%text == "character" .or. current%text == "logical" .or. &
                     current%text == "complex" .or. current%text == "double") then
                ! Type keywords should not appear in expressions - this indicates 
                ! a parser routing error. Create a placeholder identifier node
                ! instead of an error to allow parsing to continue
                expr_index = push_identifier(arena, current%text, current%line, current%column)
            else
                ! Other keywords - create error node
                expr_index = push_literal(arena, &
                    "!ERROR: Unexpected keyword '"//current%text//"' in expression", &
                    LITERAL_STRING, current%line, current%column)
            end if

        case default
            ! Unrecognized token - create error node and skip
            expr_index = push_literal(arena, &
                "!ERROR: Unrecognized token '"//trim(current%text)//"' in expression", &
                LITERAL_STRING, current%line, current%column)
            current = parser%consume()
        end select
        
        ! Now handle postfix operators (%, (), etc.) on the primary expression
        if (expr_index > 0) then
            expr_index = parse_postfix_ops(parser, arena, expr_index)
        end if
    end function parse_primary

    !=================================================================================
    ! RANGE EXPRESSION PARSING SECTION
    !=================================================================================

    ! Parse range/slice operator (:) - lowest precedence after logical operators
    function parse_range(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        ! Check if we start with a colon (empty lower bound)
        op_token = parser%peek()
        if (op_token%kind == TK_OPERATOR .and. op_token%text == ":") then
            ! Empty lower bound case (e.g., :5)
            op_token = parser%consume()
            expr_index = 0  ! No lower bound

            ! Parse the upper bound (optional)
            if (.not. parser%is_at_end()) then
                block
                    type(token_t) :: next_tok
                    next_tok = parser%peek()
                    ! Check if next token is not a closing paren/bracket or comma
                    if (.not. (next_tok%kind == TK_OPERATOR .and. &
                               (next_tok%text == ")" .or. next_tok%text == "]" .or. next_tok%text == ","))) then
                        right_index = parse_logical_eqv(parser, arena)
                    else
                        ! Empty upper bound too (just :)
                        right_index = 0
                    end if
                end block
            else
                right_index = 0
            end if

            ! Check for stride (second colon) for empty lower bound case
            block
                integer :: stride_index
                stride_index = parse_stride_component(parser, arena)
                
                expr_index = push_range_expression(arena, expr_index, right_index, &
                                                  stride_index=stride_index, &
                                                  line=op_token%line, &
                                                  column=op_token%column)
            end block
            return
        end if

        ! Normal case: parse lower bound first
        expr_index = parse_logical_eqv(parser, arena)

        ! Check for colon operator
        if (.not. parser%is_at_end()) then
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ":") then
                op_token = parser%consume()

                ! Parse the upper bound (optional)
                if (.not. parser%is_at_end()) then
                    block
                        type(token_t) :: next_tok
                        next_tok = parser%peek()
                        ! Check if next token is not a closing paren/bracket or comma
                        if (.not. (next_tok%kind == TK_OPERATOR .and. &
                                 (next_tok%text == ")" .or. next_tok%text == "]" .or. next_tok%text == ","))) then
                            right_index = parse_logical_eqv(parser, arena)
                        else
                            ! Empty upper bound (e.g., arr(2:))
                            right_index = 0
                        end if
                    end block
                else
                    right_index = 0
                end if

                ! Check for stride (second colon)
                block
                    integer :: stride_index
                    stride_index = parse_stride_component(parser, arena)
                    
                    expr_index = push_range_expression(arena, expr_index, right_index, &
                                                      stride_index=stride_index, &
                                                      line=op_token%line, &
                                                  column=op_token%column)
                end block
            end if
        end if
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
    function parse_postfix_ops(parser, arena, base_expr) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        integer :: expr_index
        type(token_t) :: op_token
        integer :: loop_count
        
        expr_index = base_expr
        loop_count = 0
        
        ! Handle postfix operators in a loop
        do while (.not. parser%is_at_end() .and. loop_count < 1000)
            loop_count = loop_count + 1
            op_token = parser%peek()
            
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "%") then
                op_token = parser%consume()
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

    ! Parse identifier or function call
    function parse_identifier_or_call(parser, arena, identifier_token) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: identifier_token
        integer :: expr_index
        
        ! Check if followed by '(' for function call
        block
            type(token_t) :: next_token
            character(len=:), allocatable :: func_name
            integer, allocatable :: arg_indices(:)
            type(token_t) :: paren
            integer :: arg_count

            next_token = parser%peek()
            if (next_token%kind == TK_OPERATOR .and. next_token%text == "(") then
                ! Parse function call

                func_name = identifier_token%text
                arg_count = 0

                ! Consume opening paren
                paren = parser%consume()

                ! Parse arguments (now handles multiple arguments)
                next_token = parser%peek()
                if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ")") then
                    block
                        class(ast_node), allocatable :: arg

                        ! Handle multiple arguments using indices
                        arg_count = 0

                        ! Parse first argument
                        block
                            integer :: arg_index
                            arg_index = parse_range(parser, arena)
                            if (arg_index > 0) then
                                arg_count = 1
                                allocate (arg_indices(1))
                                arg_indices(1) = arg_index

                                ! Parse additional arguments separated by commas
                                do
                                    next_token = parser%peek()
                if (next_token%kind /= TK_OPERATOR .or. next_token%text /= ",") exit

                                    ! Consume comma
                                    next_token = parser%consume()

                                    ! Parse next argument
                                    arg_index = parse_range(parser, arena)
                                    if (arg_index > 0) then
                                        ! Extend index array
                                        arg_indices = [arg_indices, arg_index]
                                        arg_count = arg_count + 1
                                    else
                                        exit
                                    end if
                                end do
                            end if
                        end block
                    end block
                end if

                ! Consume closing paren if present
                next_token = parser%peek()
               if (next_token%kind == TK_OPERATOR .and. next_token%text == ")") then
                    paren = parser%consume()
                end if

                ! Create function call node with array slice detection
                if (allocated(arg_indices)) then
                    expr_index = &
                        push_call_or_subscript_with_slice_detection(arena, &
                        func_name, arg_indices, identifier_token%line, identifier_token%column)
                else
                    ! For empty args, create empty function call
                    block
                        integer, allocatable :: empty_args(:)
                        allocate (empty_args(0))  ! Empty index array
                        expr_index = push_call_or_subscript(arena, func_name, &
                            empty_args, identifier_token%line, identifier_token%column)
                    end block
                end if
            else
                ! Check for boolean literals first
                if (identifier_token%text == 'true' .or. identifier_token%text == 'false') then
                    expr_index = push_literal(arena, identifier_token%text, LITERAL_LOGICAL, &
                        identifier_token%line, identifier_token%column)
                else
                    expr_index = push_identifier(arena, identifier_token%text, &
                        identifier_token%line, identifier_token%column)
                end if
            end if
        end block
    end function parse_identifier_or_call

end module parser_expressions_module
