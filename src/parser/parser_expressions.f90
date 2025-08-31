module parser_expressions_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, &
                          TK_OPERATOR, TK_KEYWORD
    use ast_core
    use ast_nodes_core, only: component_access_node, identifier_node, &
                               range_subscript_node
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
    recursive function parse_power(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_unary(parser, arena)

        ! Right-associative: a ** b ** c = a ** (b ** c)
        if (.not. parser%is_at_end()) then
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "**") then
                op_token = parser%consume()
                ! Recursive call for right-associativity  
                right_index = parse_power(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, &
                                             op_token%text, op_token%line, &
                                             op_token%column)
            end if
        end if
    end function parse_power

    ! Parse unary operators (+, -, .NOT.) - Issue #215
    recursive function parse_unary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t) :: op_token

        op_token = parser%peek()

        if (op_token%kind == TK_OPERATOR .and. &
            (op_token%text == "-" .or. op_token%text == "+" .or. &
             op_token%text == ".not.")) then
            ! Unary operator
            op_token = parser%consume()
            expr_index = parse_power(parser, arena)  ! Parse the operand
            
            if (expr_index > 0) then
                if (op_token%text == "-") then
                    ! Create unary minus as 0 - operand
                    block
                        integer :: zero_index
                        zero_index = push_literal(arena, "0", LITERAL_INTEGER, &
                                                  op_token%line, op_token%column)
                        expr_index = push_binary_op(arena, zero_index, expr_index, "-")
                    end block
                else if (op_token%text == "+") then
                    ! Unary plus - just return the operand
                    ! expr_index already contains the operand
                else if (op_token%text == ".not.") then
                    ! Create logical NOT as unary operation (0 indicates unary)
                    expr_index = push_binary_op(arena, 0, expr_index, ".not.")
                end if
            else
                expr_index = 0
            end if
        else
            ! No unary operator, parse primary expression  
            expr_index = parse_primary(parser, arena)
        end if
    end function parse_unary
    ! Parse primary expressions (literals, identifiers, parentheses)  
    recursive function parse_primary(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t) :: current

        current = parser%peek()

        select case (current%kind)
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
                "!ERROR: Unrecognized token in expression", LITERAL_STRING, &
                current%line, current%column)
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
                    ! Check if next token is not a closing paren or comma
                    if (.not. (next_tok%kind == TK_OPERATOR .and. &
                               (next_tok%text == ")" .or. next_tok%text == ","))) then
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
                        ! Check if next token is not a closing paren or comma
                        if (.not. (next_tok%kind == TK_OPERATOR .and. &
                                 (next_tok%text == ")" .or. next_tok%text == ","))) then
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
            type(token_t) :: bracket_token, current
            integer, allocatable :: element_indices(:), temp_indices(:)
            integer :: element_count

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

            ! Parse elements using simplified logic
            expr_index = parse_simple_array_elements(parser, arena, "]", "modern", bracket_token)
        end block
    end function parse_modern_array_literal

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

    ! Parse array indexing or function call postfix operator (())
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
