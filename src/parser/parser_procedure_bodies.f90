module parser_procedure_bodies_module
    ! Procedure body parsing for subroutines and functions in module contexts
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory, only: push_subroutine_def, push_function_def, &
                           push_parameter_declaration, push_print_statement, &
                           push_subroutine_call, push_assignment, push_identifier, &
                           push_literal, push_binary_op
    use parser_declarations, only: parse_declaration
    use ast_types, only: LITERAL_STRING, LITERAL_INTEGER
    implicit none
    private

    public :: parse_subroutine_in_module, parse_function_in_module

contains

    ! Safe subroutine parsing for module contexts (avoids circular dependencies)
    recursive function parse_subroutine_in_module(parser, arena) result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: sub_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        
        
        ! Consume subroutine keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
        else
            subroutine_name = "unnamed_subroutine"
        end if
        
        ! Parse parameters (simplified - no type info)
        allocate(param_indices(0))
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Parse parameter names only
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                    exit
                end if
                
                if (token%kind == TK_IDENTIFIER) then
                    ! Create simple parameter node
                    block
                        integer :: param_index
                        block
                            integer, allocatable :: empty_dims(:)
                            allocate(empty_dims(0))
                            param_index = push_parameter_declaration(arena, token%text, "", 0, 0, .false., &
                                                                   empty_dims, token%line, token%column)
                        end block
                        param_indices = [param_indices, param_index]
                    end block
                    token = parser%consume()
                end if
                
                ! Skip commas
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                end if
            end do
        end if
        
        ! Parse subroutine body until "end subroutine" (simplified)
        allocate(body_indices(0))
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            ! Check for end of subroutine
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "subroutine") then
                        ! Check if this "end subroutine" belongs to this subroutine
                        ! Look ahead to see if there's a subroutine name
                        if (parser%current_token + 2 <= size(parser%tokens) .and. &
                            parser%tokens(parser%current_token + 2)%kind == TK_IDENTIFIER) then
                            ! There is a name - check if it matches our subroutine
                            if (parser%tokens(parser%current_token + 2)%text == subroutine_name) then
                                ! This is our matching "end subroutine" - consume it and exit
                                token = parser%consume()  ! consume "end"
                                token = parser%consume()  ! consume "subroutine" 
                                token = parser%consume()  ! consume subroutine name
                                exit
                            else
                                ! This "end subroutine" belongs to a nested procedure - continue parsing
                                ! Don't consume these tokens, let the nested procedure parser handle them
                            end if
                        else
                            ! No name after "end subroutine" - assume it's ours
                            token = parser%consume()  ! consume "end"
                            token = parser%consume()  ! consume "subroutine"
                            exit
                        end if
                    end if
                end if
            end if
            
            ! Handle nested procedures directly to avoid recursive call issues
            if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                block
                    integer :: nested_index
                    nested_index = parse_subroutine_in_module(parser, arena)
                    if (nested_index > 0) then
                        body_indices = [body_indices, nested_index]
                    end if
                end block
            else if (token%kind == TK_KEYWORD .and. token%text == "function") then
                block
                    integer :: nested_index
                    nested_index = parse_function_in_module(parser, arena)
                    if (nested_index > 0) then
                        body_indices = [body_indices, nested_index]
                    end if
                end block
            else if (token%kind /= TK_NEWLINE) then
                ! Parse basic statements for subroutine body (avoiding circular dependencies)
                block
                    integer :: stmt_index
                    stmt_index = parse_basic_statement_in_subroutine(parser, arena)
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                end block
            else
                token = parser%consume()  ! consume newline
            end if
        end do
        
        ! Create subroutine node
        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, body_indices, &
                                       line, column)
    end function parse_subroutine_in_module

    ! Safe function parsing for module contexts (avoids circular dependencies)
    recursive function parse_function_in_module(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index
        type(token_t) :: token
        character(len=:), allocatable :: function_name, return_type_str
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)
        
        ! Initialize
        return_type_str = ""
        
        ! Check if we have a return type before "function"
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. &
            (token%text == "real" .or. token%text == "integer" .or. &
             token%text == "logical" .or. token%text == "character")) then
            return_type_str = token%text
            token = parser%consume()
        end if
        
        ! Consume function keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            func_index = 0
            return
        end if
        
        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            function_name = token%text
            token = parser%consume()
        else
            function_name = "unnamed_function"
        end if
        
        ! Parse parameters (simplified - no type info)
        allocate(param_indices(0))
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Parse parameter names only
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                    exit
                end if
                
                if (token%kind == TK_IDENTIFIER) then
                    ! Create simple parameter node
                    block
                        integer :: param_index
                        block
                            integer, allocatable :: empty_dims(:)
                            allocate(empty_dims(0))
                            param_index = push_parameter_declaration(arena, token%text, "", 0, 0, .false., &
                                                                   empty_dims, token%line, token%column)
                        end block
                        param_indices = [param_indices, param_index]
                    end block
                    token = parser%consume()
                end if
                
                ! Skip commas
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                end if
            end do
        end if
        
        ! Skip result clause if present
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        end if
        
        ! Parse function body until "end function" (simplified)
        allocate(body_indices(0))
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            ! Check for end of function
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "function") then
                        ! Check if this "end function" belongs to this function
                        ! Look ahead to see if there's a function name
                        if (parser%current_token + 2 <= size(parser%tokens) .and. &
                            parser%tokens(parser%current_token + 2)%kind == TK_IDENTIFIER) then
                            ! There is a name - check if it matches our function
                            if (parser%tokens(parser%current_token + 2)%text == function_name) then
                                ! This is our matching "end function" - consume it and exit
                                token = parser%consume()  ! consume "end"
                                token = parser%consume()  ! consume "function" 
                                token = parser%consume()  ! consume function name
                                exit
                            else
                                ! This "end function" belongs to a nested procedure - continue parsing
                                ! Don't consume these tokens, let the nested procedure parser handle them
                            end if
                        else
                            ! No name after "end function" - assume it's ours
                            token = parser%consume()  ! consume "end"
                            token = parser%consume()  ! consume "function"
                            exit
                        end if
                    end if
                end if
            end if
            
            ! Stop parsing body if we encounter another function or subroutine definition
            ! This prevents consuming tokens that belong to subsequent procedures
            if (token%kind == TK_KEYWORD .and. &
                (token%text == "function" .or. token%text == "subroutine")) then
                exit  ! Don't consume, let the module parser handle it
            end if
            
            ! Handle nested procedures directly to avoid recursive call issues
            if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                block
                    integer :: nested_index
                    nested_index = parse_subroutine_in_module(parser, arena)
                    if (nested_index > 0) then
                        body_indices = [body_indices, nested_index]
                    end if
                end block
            else if (token%kind == TK_KEYWORD .and. token%text == "function") then
                block
                    integer :: nested_index
                    nested_index = parse_function_in_module(parser, arena)
                    if (nested_index > 0) then
                        body_indices = [body_indices, nested_index]
                    end if
                end block
            else if (token%kind /= TK_NEWLINE) then
                ! Parse basic statements for subroutine body (avoiding circular dependencies)
                block
                    integer :: stmt_index
                    stmt_index = parse_basic_statement_in_subroutine(parser, arena)
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                end block
            else
                token = parser%consume()  ! consume newline
            end if
        end do
        
        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, &
                                      return_type_str, body_indices, &
                                      line, column, result_variable="")
    end function parse_function_in_module

    ! Basic statement parsing for subroutine/function bodies (avoiding circular deps)
    function parse_basic_statement_in_subroutine(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        
        ! Check first token to determine statement type
        token = parser%peek()
        stmt_index = 0
        
        select case (token%kind)
        case (TK_KEYWORD)
            select case (token%text)
            case ("print")
                stmt_index = parse_simple_print_statement(parser, arena)
            case ("integer", "real", "logical", "character", "complex", "double")
                stmt_index = parse_declaration(parser, arena)
            case ("call")
                stmt_index = parse_simple_call_statement(parser, arena)
            case ("contains")
                ! Handle contains section with nested procedures
                token = parser%consume()  ! consume 'contains'
                stmt_index = 0  ! contains itself is not a statement, just a marker
                ! The nested procedures will be parsed in subsequent iterations
            case ("subroutine")
                ! Handle nested subroutine definitions
                stmt_index = parse_subroutine_in_module(parser, arena)
            case ("function")
                ! Handle nested function definitions  
                stmt_index = parse_function_in_module(parser, arena)
            case default
                ! Unknown keyword - consume it
                token = parser%consume()
                stmt_index = 0
            end select
        case (TK_IDENTIFIER)
            ! Likely assignment statement
            stmt_index = parse_simple_assignment_statement(parser, arena)
        case default
            ! Unknown token - consume it
            token = parser%consume()
            stmt_index = 0
        end select
    end function parse_basic_statement_in_subroutine

    ! Simple print statement parser for subroutine bodies
    function parse_simple_print_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        
        ! Consume 'print' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Expect '*' or format specifier
        allocate(arg_indices(0))
        
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            token = parser%consume()  ! consume '*'
            
            ! Optional comma (may not be present in compact format like print*,"foo")
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
            end if
            
            ! Parse print arguments until end of line
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_NEWLINE) then
                    exit
                end if
                
                if (token%kind == TK_STRING) then
                    ! String literal
                    block
                        integer :: arg_index
                        arg_index = push_literal(arena, token%text, LITERAL_STRING, &
                                               token%line, token%column)
                        arg_indices = [arg_indices, arg_index]
                    end block
                    token = parser%consume()
                    
                    ! After parsing a string literal, check if we're at end of statement
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        exit  ! End of print statement
                    else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume comma and continue
                    else
                        ! No comma after string - end of print arguments
                        exit
                    end if
                else if (token%kind == TK_IDENTIFIER) then
                    ! Variable reference - but be careful not to consume keywords that end the statement
                    if (token%text == "end" .or. token%text == "subroutine" .or. &
                        token%text == "function") then
                        ! This identifier likely belongs to a different statement
                        exit
                    end if
                    
                    block
                        integer :: arg_index
                        arg_index = push_identifier(arena, token%text, &
                                                  token%line, token%column)
                        arg_indices = [arg_indices, arg_index]
                    end block
                    token = parser%consume()
                    
                    ! After parsing an identifier, check if we're at end of statement
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        exit  ! End of print statement
                    else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume comma and continue
                    else
                        ! No comma after identifier - end of print arguments
                        exit
                    end if
                else if (token%kind == TK_NUMBER) then
                    ! Numeric literal
                    block
                        integer :: arg_index
                        arg_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                               token%line, token%column)
                        arg_indices = [arg_indices, arg_index]
                    end block
                    token = parser%consume()
                    
                    ! After parsing a number, check if we're at end of statement
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        exit  ! End of print statement
                    else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume comma and continue
                    else
                        ! No comma after number - end of print arguments
                        exit
                    end if
                else if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume comma
                else
                    ! Unknown token - likely end of print statement
                    exit
                end if
            end do
        end if
        
        ! Create print statement node
        stmt_index = push_print_statement(arena, "*", arg_indices, line, column)
    end function parse_simple_print_statement

    ! Simple call statement parser for subroutine bodies
    function parse_simple_call_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer, allocatable :: arg_indices(:)
        integer :: line, column
        
        ! Consume 'call' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
            
            ! For simplicity, skip arguments if present
            allocate(arg_indices(0))
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('
                ! Skip until closing parenthesis
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()  ! consume ')'
                        exit
                    end if
                    token = parser%consume()  ! skip argument tokens
                end do
            end if
            
            ! Create call statement node
            stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, &
                                            line, column)
        else
            ! Error: expected subroutine name
            stmt_index = 0
        end if
    end function parse_simple_call_statement

    ! Simple assignment statement parser for subroutine bodies
    function parse_simple_assignment_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer :: lhs_index, rhs_index
        
        stmt_index = 0
        
        ! Parse left-hand side (identifier)
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            lhs_index = push_identifier(arena, token%text, token%line, token%column)
            token = parser%consume()
            
            ! Expect assignment operator
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                
                ! Parse right-hand side (expression)
                rhs_index = parse_simple_rhs_expression(parser, arena)
                
                if (rhs_index <= 0) then
                    ! Failed to parse expression
                    return
                end if
                
                ! Create assignment node
                stmt_index = push_assignment(arena, lhs_index, rhs_index, &
                                           token%line, token%column)
            else
                ! No assignment operator
                stmt_index = 0
            end if
        else
            ! No identifier
            stmt_index = 0
        end if
    end function parse_simple_assignment_statement

    ! Parse simple right-hand side expression (handles binary operations)
    function parse_simple_rhs_expression(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t) :: token
        integer :: left_index, right_index
        character(len=:), allocatable :: op_text
        
        expr_index = 0
        
        ! Parse first operand
        token = parser%peek()
        if (token%kind == TK_STRING) then
            left_index = push_literal(arena, token%text, LITERAL_STRING, &
                                     token%line, token%column)
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            left_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                     token%line, token%column)
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            left_index = push_identifier(arena, token%text, &
                                        token%line, token%column)
            token = parser%consume()
        else
            return  ! Invalid expression
        end if
        
        ! Check for binary operator
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. &
            (token%text == "+" .or. token%text == "-" .or. &
             token%text == "*" .or. token%text == "/" .or. &
             token%text == "**")) then
            op_text = token%text
            token = parser%consume()  ! consume operator
            
            ! Parse second operand
            token = parser%peek()
            if (token%kind == TK_STRING) then
                right_index = push_literal(arena, token%text, LITERAL_STRING, &
                                         token%line, token%column)
                token = parser%consume()
            else if (token%kind == TK_NUMBER) then
                right_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                         token%line, token%column)
                token = parser%consume()
            else if (token%kind == TK_IDENTIFIER) then
                right_index = push_identifier(arena, token%text, &
                                            token%line, token%column)
                token = parser%consume()
            else
                expr_index = left_index  ! Return just the left operand
                return
            end if
            
            ! Create binary operation node
            expr_index = push_binary_op(arena, left_index, right_index, op_text, &
                                       token%line, token%column)
        else
            ! No operator, just return the single operand
            expr_index = left_index
        end if
    end function parse_simple_rhs_expression
    
    ! Skip tokens until end of line
    subroutine skip_to_end_of_line(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()  ! consume newline
                exit
            end if
            token = parser%consume()
        end do
    end subroutine skip_to_end_of_line

end module parser_procedure_bodies_module