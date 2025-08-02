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
                           push_component_access, push_range_subscript
    use parser_state_module, only: parser_state_t, create_parser_state
    use codegen_core, only: generate_code_from_arena
    implicit none
    private

    ! Public expression parsing interface
    public :: parse_expression
    public :: parse_range, parse_logical_or, parse_logical_and, parse_comparison
    public :: parse_member_access, parse_term, parse_factor, parse_primary

contains

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
                op_token = parser%consume()  ! consume second ':'
                
                ! Parse stride
                if (.not. parser%is_at_end()) then
                    next_tok = parser%peek()
                    if (.not. (next_tok%kind == TK_OPERATOR .and. &
                             (next_tok%text == ")" .or. next_tok%text == "," .or. &
                              next_tok%text == "]" .or. next_tok%text == ";"))) then
                        stride_index = parse_logical_or(parser, arena)
                    end if
                end if
            end if
        end if
    end function parse_stride_component

    ! Main expression parsing entry point with stack
    function parse_expression(tokens, arena) result(expr_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(parser_state_t) :: parser

        parser = create_parser_state(tokens)
        expr_index = parse_range(parser, arena)
    end function parse_expression

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
            op_token = parser%consume()  ! consume ':'
            expr_index = 0  ! No lower bound

            ! Parse the upper bound (optional)
            if (.not. parser%is_at_end()) then
                block
                    type(token_t) :: next_tok
                    next_tok = parser%peek()
                    ! Check if next token is not a closing paren or comma
                    if (.not. (next_tok%kind == TK_OPERATOR .and. &
                               (next_tok%text == ")" .or. next_tok%text == ","))) then
                        right_index = parse_logical_or(parser, arena)
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
                                                  line=op_token%line, column=op_token%column)
            end block
            return
        end if

        ! Normal case: parse lower bound first
        expr_index = parse_logical_or(parser, arena)

        ! Check for colon operator
        if (.not. parser%is_at_end()) then
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ":") then
                op_token = parser%consume()  ! consume ':'

                ! Parse the upper bound (optional)
                if (.not. parser%is_at_end()) then
                    block
                        type(token_t) :: next_tok
                        next_tok = parser%peek()
                        ! Check if next token is not a closing paren or comma
                        if (.not. (next_tok%kind == TK_OPERATOR .and. &
                                 (next_tok%text == ")" .or. next_tok%text == ","))) then
                            right_index = parse_logical_or(parser, arena)
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
                                                      line=op_token%line, column=op_token%column)
                end block
            end if
        end if
    end function parse_range

    ! Parse logical OR operators (lowest precedence)
    function parse_logical_or(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_logical_and(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".or.") then
                op_token = parser%consume()  ! consume operator
                right_index = parse_logical_and(parser, arena)
                if (right_index > 0) then
              expr_index = push_binary_op(arena, expr_index, right_index, op_token%text)
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

        expr_index = parse_comparison(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. op_token%text == ".and.") then
                op_token = parser%consume()  ! consume operator
                right_index = parse_comparison(parser, arena)
                if (right_index > 0) then
              expr_index = push_binary_op(arena, expr_index, right_index, op_token%text)
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

        expr_index = parse_member_access(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
                (op_token%text == "==" .or. op_token%text == "/=" .or. &
                 op_token%text == "<=" .or. op_token%text == ">=" .or. &
                 op_token%text == "<" .or. op_token%text == ">")) then
                op_token = parser%consume()
                right_index = parse_member_access(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_comparison

    ! Parse member access operator (%)
    function parse_member_access(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        ! Note: % operator is now handled as a postfix operator in parse_primary
        ! This function just passes through to parse_term for backward compatibility
        expr_index = parse_term(parser, arena)
    end function parse_member_access

    ! Parse addition and subtraction
    function parse_term(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_factor(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
       (op_token%text == "+" .or. op_token%text == "-" .or. op_token%text == "//")) then
                op_token = parser%consume()
                right_index = parse_factor(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_term

    ! Parse multiplication, division, and power
    function parse_factor(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        integer :: right_index
        type(token_t) :: op_token

        expr_index = parse_primary(parser, arena)

        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            if (op_token%kind == TK_OPERATOR .and. &
       (op_token%text == "*" .or. op_token%text == "/" .or. op_token%text == "**")) then
                op_token = parser%consume()
                right_index = parse_primary(parser, arena)
                expr_index = push_binary_op(arena, expr_index, right_index, op_token%text, op_token%line, op_token%column)
            else
                exit
            end if
        end do
    end function parse_factor

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
            if (index(current%text, '.') > 0) then
                ! Contains decimal point - classify as real
         expr_index = push_literal(arena, current%text, LITERAL_REAL, current%line, current%column)
            else
                ! No decimal point - classify as integer
      expr_index = push_literal(arena, current%text, LITERAL_INTEGER, current%line, current%column)
            end if

        case (TK_STRING)
            ! Parse string literal
            current = parser%consume()
       expr_index = push_literal(arena, current%text, LITERAL_STRING, current%line, current%column)

        case (TK_IDENTIFIER)
            ! Parse identifier or function call
            current = parser%consume()

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

                    func_name = current%text
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
                        expr_index = push_call_or_subscript_with_slice_detection(arena, &
                            func_name, arg_indices, current%line, current%column)
                    else
                        ! For empty args, create empty function call
                        block
                            integer, allocatable :: empty_args(:)
                            allocate (empty_args(0))  ! Empty index array
                            expr_index = push_call_or_subscript(arena, func_name, &
                                empty_args, current%line, current%column)
                        end block
                    end if
                else
         expr_index = push_identifier(arena, current%text, current%line, current%column)
                end if
            end block

        case (TK_OPERATOR)
            ! Debug: print operator
            ! print *, "DEBUG parse_primary: operator = '", trim(current%text), "'"
            ! Check for parentheses
            if (current%text == "(") then
                current = parser%consume()  ! consume '('
                expr_index = parse_range(parser, arena)  ! parse the expression inside
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()  ! consume ')'
                end if
            else if (current%text == "-" .or. current%text == "+") then
                ! Unary operator
                block
                    type(token_t) :: op_token
                    integer :: operand_index
                    op_token = parser%consume()
                    operand_index = parse_primary(parser, arena)
                    if (operand_index > 0) then
                        ! Create unary expression as binary op with zero
                        if (op_token%text == "-") then
                            ! For unary minus, create 0 - operand
                            block
                                integer :: zero_index
  zero_index = push_literal(arena, "0", LITERAL_INTEGER, op_token%line, op_token%column)
                      expr_index = push_binary_op(arena, zero_index, operand_index, "-")
                            end block
                        else
                            ! For unary plus, just return the operand
                            expr_index = operand_index
                        end if
                    else
                        expr_index = 0
                    end if
                end block
            else if (current%text == "[") then
                ! Array literal: [1, 2, 3]
                block
                    type(token_t) :: bracket_token
                    integer, allocatable :: element_indices(:)
                    integer :: element_count
                    integer, allocatable :: temp_indices(:)

                    bracket_token = parser%consume()  ! consume '['
                    element_count = 0
                    allocate (temp_indices(100))  ! Start with space for 100 elements

                    ! Check for empty array []
                    current = parser%peek()
                    if (current%text == "]") then
                        current = parser%consume()  ! consume ']'
                        allocate (element_indices(0))
                        expr_index = push_array_literal(arena, element_indices, bracket_token%line, bracket_token%column)
                    else
                        ! Parse array elements
                        do
                            ! Parse element expression
                            element_count = element_count + 1
                            if (element_count > size(temp_indices)) then
                                ! Resize array
                                block
                                    integer, allocatable :: new_indices(:)
                                    allocate (new_indices(size(temp_indices)*2))
                                    new_indices(1:size(temp_indices)) = temp_indices
                                    deallocate (temp_indices)
                                    allocate (temp_indices(size(new_indices)))
                                    temp_indices = new_indices
                                end block
                            end if

                           temp_indices(element_count) = parse_comparison(parser, arena)

                            ! Check for comma or closing bracket
                            current = parser%peek()
                            if (current%text == ",") then
                                current = parser%consume()  ! consume ','

                                ! Check for implied do loop: (expr, var=start,end)
                                ! Peek ahead to see if next token is identifier followed by =
                                block
                                    type(token_t) :: next1, next2
                                    integer :: saved_pos

                                    next1 = parser%peek()
                                    if (next1%kind == TK_IDENTIFIER) then
                                        ! Save position
                                        saved_pos = parser%current_token

                                        ! Consume identifier
                                        next1 = parser%consume()
                                        next2 = parser%peek()

                             if (next2%kind == TK_OPERATOR .and. next2%text == "=") then
                                            ! This is an implied do loop!
                                            ! Parse: (expr, var=start,end[,step])

                                            ! We already have the first expression in temp_indices(element_count)
                                            ! next1 contains the loop variable name
                                            block
                                               character(len=:), allocatable :: loop_var
                                                integer :: start_idx, end_idx, step_idx
                                                integer :: loop_expr_idx

                                                loop_var = next1%text

                                                ! Consume the '='
                                                current = parser%consume()

                                                ! Parse start expression
                                             start_idx = parse_comparison(parser, arena)

                                                ! Expect comma
                                                current = parser%peek()
                                                if (current%text /= ",") then
                write (error_unit, *) 'Error: Expected "," after loop start at line ', &
                                                        current%line
                                                    expr_index = 0
                                                    return
                                                end if
                                                current = parser%consume()

                                                ! Parse end expression
                                               end_idx = parse_comparison(parser, arena)

                                                ! Check for optional step
                                                step_idx = 0
                                                current = parser%peek()
                                                if (current%text == ",") then
                                                    current = parser%consume()
                                              step_idx = parse_comparison(parser, arena)
                                                end if

                                                ! Expect closing parenthesis
                                                current = parser%peek()
                                                if (current%text /= ")") then
           write (error_unit, *) 'Error: Expected ")" after implied do loop at line ', &
                                                        current%line
                                                    expr_index = 0
                                                    return
                                                end if
                                                current = parser%consume()

                                                ! Expect closing bracket
                                                current = parser%peek()
                                                if (current%text /= "]") then
           write (error_unit, *) 'Error: Expected "]" after implied do loop at line ', &
                                                        current%line
                                                    expr_index = 0
                                                    return
                                                end if
                                                current = parser%consume()

                                                ! Create a do_loop_node for the implied do
                                                block
                                                    use ast_factory, only: push_do_loop
                                               integer, allocatable :: body_idx_array(:)

                                                    ! The body is the expression we already parsed
                                                    allocate (body_idx_array(1))
                                         body_idx_array(1) = temp_indices(element_count)

                                                    ! Create the do loop node
                     loop_expr_idx = push_do_loop(arena, loop_var, start_idx, end_idx, &
                                                             step_idx, body_idx_array, &
                                   line=bracket_token%line, column=bracket_token%column)

                                                    ! Create an array with the implied do loop as its element
                                                    allocate (element_indices(1))
                                                    element_indices(1) = loop_expr_idx
                               expr_index = push_array_literal(arena, element_indices, &
                                               bracket_token%line, bracket_token%column)
                                                end block
                                            end block
                                            return
                                        else
                                            ! Not an implied do loop, restore position
                                            parser%current_token = saved_pos
                                        end if
                                    end if
                                end block
                            else if (current%text == "]") then
                                current = parser%consume()  ! consume ']'
                                exit
                            else
                                ! Error: expected comma or closing bracket
         write (error_unit, *) 'Error: Expected "," or "]" in array literal at line ', &
                                    current%line, ', column ', current%column
                                expr_index = 0
                                return
                            end if
                        end do

                        ! Copy to final array
                        allocate (element_indices(element_count))
                        element_indices = temp_indices(1:element_count)
                        expr_index = push_array_literal(arena, element_indices, bracket_token%line, bracket_token%column)
                    end if
                end block
            else if (current%text == ".not.") then
                ! Logical NOT operator
                block
                    type(token_t) :: op_token
                    integer :: operand_index
                    op_token = parser%consume()
                    operand_index = parse_primary(parser, arena)
                    if (operand_index > 0) then
                        ! Create unary NOT expression as binary op with false
                        block
                            integer :: false_index
             false_index = push_literal(arena, ".false.", LITERAL_LOGICAL, op_token%line, op_token%column)
                 expr_index = push_binary_op(arena, false_index, operand_index, ".not.")
                        end block
                    else
                        expr_index = 0
                    end if
                end block
            else if (current%text == ".") then
                ! Check for logical literals (.true. or .false.)
                block
                    type(token_t) :: next_token, third_token
                    if (parser%current_token + 2 <= size(parser%tokens)) then
                        next_token = parser%tokens(parser%current_token + 1)
                        third_token = parser%tokens(parser%current_token + 2)

                        if (next_token%kind == TK_IDENTIFIER .and. &
                     third_token%kind == TK_OPERATOR .and. third_token%text == ".") then
                     if (next_token%text == "true" .or. next_token%text == "false") then
                                ! It's a logical literal
                                current = parser%consume()  ! consume first '.'
                                current = parser%consume()  ! consume 'true'/'false'
                                current = parser%consume()  ! consume second '.'
    expr_index = push_literal(arena, "."//trim(next_token%text)//".", LITERAL_LOGICAL, &
                                                          current%line, current%column)
                            else
                                ! Not a logical literal
      expr_index = push_literal(arena, "", LITERAL_STRING, current%line, current%column)
                                current = parser%consume()
                            end if
                        else
                            ! Not a logical literal pattern
      expr_index = push_literal(arena, "", LITERAL_STRING, current%line, current%column)
                            current = parser%consume()
                        end if
                    else
                        ! Not enough tokens
      expr_index = push_literal(arena, "", LITERAL_STRING, current%line, current%column)
                        current = parser%consume()
                    end if
                end block
            else
                ! Unrecognized operator - create error node
expr_index = push_literal(arena, "!ERROR: Unrecognized operator '"//current%text//"'", &
                                          LITERAL_STRING, current%line, current%column)
                current = parser%consume()
            end if

        case (TK_KEYWORD)
            ! Handle logical constants
            current = parser%consume()
            if (current%text == ".true." .or. current%text == ".false.") then
      expr_index = push_literal(arena, current%text, LITERAL_LOGICAL, current%line, current%column)
            else
                ! Other keywords - create error node
      expr_index = push_literal(arena, "!ERROR: Unexpected keyword '"//current%text//"' in expression", &
                                          LITERAL_STRING, current%line, current%column)
            end if

        case default
            ! Unrecognized token - create error node and skip
      expr_index = push_literal(arena, "!ERROR: Unrecognized token in expression", LITERAL_STRING, current%line, current%column)
            current = parser%consume()
        end select
        
        ! Now handle postfix operators (%, (), etc.) on the primary expression
        if (expr_index > 0) then
            expr_index = parse_postfix_ops(parser, arena, expr_index)
        end if
    end function parse_primary
    
    ! Parse postfix operators on an expression
    function parse_postfix_ops(parser, arena, base_expr) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        integer :: expr_index
        type(token_t) :: op_token
        
        expr_index = base_expr
        
        ! Handle postfix operators in a loop
        do while (.not. parser%is_at_end())
            op_token = parser%peek()
            
            if (op_token%kind == TK_OPERATOR .and. op_token%text == "%") then
                ! Component access
                op_token = parser%consume()  ! consume '%'
                
                block
                    type(token_t) :: component_token
                    component_token = parser%peek()
                    if (component_token%kind == TK_IDENTIFIER) then
                        component_token = parser%consume()
                        expr_index = push_component_access(arena, expr_index, component_token%text, &
                                                         op_token%line, op_token%column)
                    else
                        ! Error: expected identifier after %
                        expr_index = push_literal(arena, "!ERROR: Expected identifier after %", &
                                                LITERAL_STRING, op_token%line, op_token%column)
                        exit
                    end if
                end block
                
            else if (op_token%kind == TK_OPERATOR .and. op_token%text == "(") then
                ! Array indexing or function call on the expression result
                ! NOTE: This is a partial implementation. Full support for
                ! expressions like matrix%data(i,j) requires AST restructuring
                block
                    integer, allocatable :: arg_indices(:)
                    type(token_t) :: paren
                    integer :: arg_count
                    character(len=:), allocatable :: name_for_call
                    
                    arg_count = 0
                    
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
                                    if (op_token%kind /= TK_OPERATOR .or. op_token%text /= ",") exit
                                    
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
                        ! Get the name to use for the call_or_subscript node
                        select type (node => arena%entries(expr_index)%node)
                        type is (component_access_node)
                            ! For component access, use the component name
                            name_for_call = node%component_name
                        type is (identifier_node)
                            ! For identifiers, use the identifier name
                            name_for_call = node%name
                        type is (range_subscript_node)
                            ! For range subscripts, handle nested subscript operation
                            block
                                use ast_nodes_bounds, only: range_expression_node
                                logical :: is_range_subscript
                                
                                is_range_subscript = .false.
                                if (size(arg_indices) == 1 .and. arg_indices(1) > 0 .and. &
                                    arg_indices(1) <= arena%size) then
                                    select type (arg_node => arena%entries(arg_indices(1))%node)
                                    type is (range_expression_node)
                                        ! Create nested range subscript
                                        expr_index = push_range_subscript(arena, &
                                            expr_index, arg_node%start_index, &
                                            arg_node%end_index, paren%line, paren%column)
                                        is_range_subscript = .true.
                                    end select
                                end if
                                
                                if (is_range_subscript) then
                                    deallocate(arg_indices)
                                    cycle
                                else
                                    ! Not a range - can't handle other operations on &
                                    ! range_subscript
                                    deallocate(arg_indices)
                                    exit
                                end if
                            end block
                        type is (array_slice_node)
                            ! For array slices (which could be character substrings),
                            ! handle nested subscript operation
                            block
                                use ast_nodes_bounds, only: range_expression_node, &
                                    array_slice_node
                                logical :: is_range_subscript
                                
                                is_range_subscript = .false.
                                if (size(arg_indices) == 1 .and. &
                                    arg_indices(1) > 0 .and. &
                                    arg_indices(1) <= arena%size) then
                                    select type (arg_node => &
                                        arena%entries(arg_indices(1))%node)
                                    type is (range_expression_node)
                                        ! Create nested range subscript using the 
                                        ! array_slice as the base expression
                                        expr_index = push_range_subscript(arena, &
                                            expr_index, arg_node%start_index, &
                                            arg_node%end_index, paren%line, &
                                            paren%column)
                                        is_range_subscript = .true.
                                    end select
                                end if
                                
                                if (is_range_subscript) then
                                    deallocate(arg_indices)
                                    cycle
                                else
                                    ! Not a range - can't handle other operations
                                    deallocate(arg_indices)
                                    exit
                                end if
                            end block
                        class default
                            ! For other expressions, we can't handle array indexing
                            deallocate(arg_indices)
                            exit
                        end select
                        
                        if (allocated(name_for_call)) then
                            ! Special handling for component access followed by array indexing
                            select type (node => arena%entries(expr_index)%node)
                            type is (component_access_node)
                                ! Build the full qualified name
                                block
                                    character(len=:), allocatable :: base_name, full_name
                                    
                                    ! Get the base name
                                    if (node%base_expr_index > 0) then
                                        select type (base_node => arena%entries(node%base_expr_index)%node)
                                        type is (identifier_node)
                                            base_name = base_node%name
                                        type is (component_access_node)
                                            ! Handle chained component access recursively
                                            base_name = generate_code_from_arena(arena, node%base_expr_index)
                                        class default
                                            base_name = "__expr__"
                                        end select
                                    else
                                        base_name = "__expr__"
                                    end if
                                    
                                    ! Build full qualified name
                                    full_name = base_name // "%" // name_for_call
                                    
                                    ! Create call_or_subscript with full name
                                    expr_index = &
                                        push_call_or_subscript_with_slice_detection(arena, &
                                        full_name, arg_indices, paren%line, paren%column)
                                end block
                            class default
                                ! Standard case
                                expr_index = &
                                    push_call_or_subscript_with_slice_detection(arena, &
                                    name_for_call, arg_indices, &
                                    paren%line, paren%column)
                            end select
                        end if
                    end if
                end block
            else
                ! No more postfix operators
                exit
            end if
        end do
    end function parse_postfix_ops

end module parser_expressions_module
