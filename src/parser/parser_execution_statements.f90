module parser_execution_statements_module
    ! Parser module for execution statement types (call, program)
    use lexer_core
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_STRING, TK_NEWLINE, TK_KEYWORD
    use parser_state_module
    use parser_expressions_module, only: parse_range
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement
    use parser_memory_statements_module, only: parse_allocate_statement, parse_deallocate_statement
    use parser_control_statements_module, only: parse_stop_statement, parse_goto_statement, &
                                               parse_error_stop_statement, parse_return_statement, &
                                               parse_cycle_statement, parse_exit_statement
    use parser_control_flow_module, only: parse_do_loop
    use parser_procedure_definitions_module, only: parse_function_definition, parse_subroutine_definition
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING, LITERAL_INTEGER, LITERAL_REAL
    implicit none
    private

    public :: parse_call_statement, parse_program_statement, parse_assignment_statement

    ! Module variable to store additional indices from multi-declaration parsing
    integer, allocatable :: additional_execution_indices(:)

contains

    ! Helper subroutine to parse call arguments
    subroutine parse_call_arguments(parser, arena, arg_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: arg_index
        
        allocate(arg_indices(0))
        
        ! Consume opening parenthesis
        token = parser%consume()
        
        ! Parse arguments
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
                exit
            end if
            
            ! Parse argument expression
            arg_index = parse_range(parser, arena)
            if (arg_index > 0) then
                arg_indices = [arg_indices, arg_index]
            end if
            
            ! Check for comma (more arguments)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
                cycle
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
                exit
            else
                ! Unexpected token - try to continue
                exit
            end if
        end do
    end subroutine parse_call_arguments

    function parse_call_statement(parser, arena) result(stmt_index)
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
            token = parser%consume()
            subroutine_name = token%text

            ! Check for arguments
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! Parse arguments
                call parse_call_arguments(parser, arena, arg_indices)
            else
                ! No arguments
                allocate (arg_indices(0))
            end if

            ! Create call node
            stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, &
                                              line, column)
        else
            ! Error: expected subroutine name
            stmt_index = push_literal(arena, "! Error: expected subroutine name after 'call'", &
                                      LITERAL_STRING, line, column)
        end if
    end function parse_call_statement

    function parse_program_statement(parser, arena) result(prog_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        type(token_t) :: token, name_token
        character(len=:), allocatable :: program_name
        integer :: line, column
        integer, allocatable :: body_indices(:)
        integer :: stmt_index

        prog_index = 0
        allocate (body_indices(0))

        ! Check if we're already at 'program' keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "program") then
            ! Consume 'program' keyword
            token = parser%consume()
        else
            ! Not at program keyword, return 0
            prog_index = 0
            return
        end if
        
        line = token%line
        column = token%column

        ! Get program name (optional in lazy fortran, required in standard)
        name_token = parser%peek()
        if (name_token%kind == TK_IDENTIFIER) then
            name_token = parser%consume()
            program_name = name_token%text
        else
            program_name = "main"
        end if

        ! Parse program body until 'end program'
        ! Use a simpler approach that delegates to individual statement parsing
        call parse_program_body(parser, arena, body_indices)

        ! Create program node
        prog_index = push_program(arena, program_name, body_indices, line, column)

    end function parse_program_statement

    ! Parse the body of a program until 'end program'
    ! Simplified approach that handles the basic case
    subroutine parse_program_body(parser, arena, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_index

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for 'end program'
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "program") then
                        ! Found 'end program', consume both tokens
                        token = parser%consume()  ! end
                        token = parser%consume()  ! program

                        ! Optional program name after 'end program'
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER) then
                            token = parser%consume()
                        end if
                        exit
                    end if
                end if
            end if

            ! Parse statements directly without complex boundary detection
            stmt_index = 0
            
            select case (token%kind)
            case (TK_KEYWORD)
                select case (token%text)
                case ("implicit")
                    call parse_simple_implicit(parser, arena, stmt_index)
                case ("real", "integer", "logical", "character", "complex")
                    ! Handle single vs multi-variable declarations
                    call handle_variable_declaration(parser, arena, stmt_index)
                case ("print")
                    stmt_index = parse_print_statement(parser, arena)
                case ("write")
                    ! Parse write statement using the IO module (already in use at module level)
                    stmt_index = parse_write_statement(parser, arena)
                case ("allocate")
                    stmt_index = parse_allocate_statement(parser, arena)
                case ("deallocate")
                    stmt_index = parse_deallocate_statement(parser, arena)
                case ("if")
                    ! Simple inline if parsing to avoid circular dependency
                    stmt_index = parse_simple_if(parser, arena)
                case ("stop")
                    stmt_index = parse_stop_statement(parser, arena)
                case ("go", "goto")
                    stmt_index = parse_goto_statement(parser, arena)
                case ("error")
                    stmt_index = parse_error_stop_statement(parser, arena)
                case ("return")
                    stmt_index = parse_return_statement(parser, arena)
                case ("cycle")
                    stmt_index = parse_cycle_statement(parser, arena)
                case ("exit")
                    stmt_index = parse_exit_statement(parser, arena)
                case ("call")
                    stmt_index = parse_call_statement(parser, arena)
                case ("do")
                    ! Parse do loop using control flow module
                    stmt_index = parse_do_loop(parser, arena)
                case ("contains")
                    ! Consume 'contains' keyword and continue parsing definitions
                    token = parser%consume()
                    stmt_index = 0
                case ("function")
                    ! Parse function definition in contains section
                    stmt_index = parse_function_definition(parser, arena)
                case ("subroutine")
                    ! Parse subroutine definition in contains section
                    stmt_index = parse_subroutine_definition(parser, arena)
                case default
                    ! Skip unknown keywords for now
                    token = parser%consume()
                    stmt_index = 0
                end select
            case (TK_IDENTIFIER)
                call parse_assignment_statement(parser, arena, stmt_index)
            case (TK_NEWLINE, TK_COMMENT)
                ! Skip newlines and comments
                token = parser%consume()
                stmt_index = 0
            case default
                ! Skip unrecognized tokens
                token = parser%consume()
                stmt_index = 0
            end select
            
            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
                
                ! Handle additional indices from multi-declaration parsing
                if (allocated(additional_execution_indices)) then
                    if (size(additional_execution_indices) > 0) then
                        body_indices = [body_indices, additional_execution_indices]
                    end if
                    deallocate(additional_execution_indices)
                end if
            end if
        end do
    end subroutine parse_program_body

    ! Parse a simple if statement (minimal implementation to avoid circular dependencies)
    function parse_simple_if(parser, arena) result(if_index)
        use parser_expressions_module, only: parse_expression
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index
        type(token_t) :: token
        integer :: condition_index, line, column
        integer, allocatable :: then_body_indices(:)
        
        if_index = 0
        allocate(then_body_indices(0))
        
        ! Consume 'if' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Parse condition in parentheses
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            ! Create a token array from current position for expression parser
            block
                type(token_t), allocatable :: expr_tokens(:)
                integer :: start_pos, end_pos, paren_depth
                
                start_pos = parser%current_token
                end_pos = start_pos
                paren_depth = 1
                
                ! Find the matching closing parenthesis
                do while (end_pos <= size(parser%tokens) .and. paren_depth > 0)
                    if (parser%tokens(end_pos)%text == "(") then
                        paren_depth = paren_depth + 1
                    else if (parser%tokens(end_pos)%text == ")") then
                        paren_depth = paren_depth - 1
                    end if
                    if (paren_depth > 0) end_pos = end_pos + 1
                end do
                
                ! Extract tokens for expression
                if (end_pos > start_pos) then
                    allocate(expr_tokens(end_pos - start_pos))
                    expr_tokens = parser%tokens(start_pos:end_pos-1)
                    condition_index = parse_expression(expr_tokens, arena)
                    parser%current_token = end_pos
                else
                    condition_index = 0
                end if
            end block
            
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        else
            condition_index = 0
        end if
        
        ! Expect 'then'
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "then") then
            token = parser%consume()
        end if
        
        ! Parse body until 'end if' or 'else' or 'elseif'
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            ! Check for end of if
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "if") then
                        token = parser%consume()  ! consume 'end'
                        token = parser%consume()  ! consume 'if'
                        exit
                    end if
                end if
            else if (token%kind == TK_KEYWORD .and. &
                    (token%text == "else" .or. token%text == "elseif")) then
                ! For simplicity, skip else/elseif blocks for now
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                            if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                                parser%tokens(parser%current_token + 1)%text == "if") then
                                token = parser%consume()  ! consume 'end'
                                token = parser%consume()  ! consume 'if'
                                exit
                            end if
                        end if
                    end if
                    token = parser%consume()
                end do
                exit
            end if
            
            ! Parse statement in if body
            call parse_if_body_statement(parser, arena, then_body_indices)
        end do
        
        ! Create if node
        if_index = push_if(arena, condition_index, then_body_indices, &
                          line=line, column=column)
    end function parse_simple_if
    
    ! Parse a statement in an if body
    subroutine parse_if_body_statement(parser, arena, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_index
        
        token = parser%peek()
        stmt_index = 0
        
        select case (token%kind)
        case (TK_KEYWORD)
            select case (token%text)
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("real", "integer", "logical", "character")
                stmt_index = parse_declaration(parser, arena)
            case default
                token = parser%consume()
            end select
        case (TK_IDENTIFIER)
            ! Try parsing an assignment
            call parse_assignment_statement(parser, arena, stmt_index)
        case (TK_NEWLINE)
            token = parser%consume()
        case default
            token = parser%consume()
        end select
        
        if (stmt_index > 0) then
            body_indices = [body_indices, stmt_index]
        end if
    end subroutine parse_if_body_statement

    ! Parse a simple assignment statement or multi-variable assignment
    subroutine parse_assignment_statement(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: id_token, op_token
        integer :: target_index, value_index
        logical :: is_multi_assignment

        stmt_index = 0
        
        ! Check if this is a multi-variable assignment (has commas before =)
        is_multi_assignment = is_multi_var_assignment(parser)
        
        if (is_multi_assignment) then
            call parse_multi_variable_assignment(parser, arena, stmt_index)
        else
            ! Original single assignment logic
            id_token = parser%consume()
            op_token = parser%peek()

            if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                op_token = parser%consume()

                ! Create target identifier
                target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column)

                ! Parse value expression
                value_index = parse_range(parser, arena)
                
                if (value_index > 0) then
                    stmt_index = push_assignment(arena, target_index, value_index, &
                                               id_token%line, id_token%column)
                end if
            end if
        end if
    end subroutine parse_assignment_statement

    ! Parse a simple variable declaration
    subroutine parse_simple_declaration(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: type_token, colon_token, name_token
        character(len=:), allocatable :: type_name, var_name

        stmt_index = 0
        
        ! Get type
        type_token = parser%consume()
        type_name = type_token%text
        
        ! Check for ::
        colon_token = parser%peek()
        if (colon_token%kind == TK_OPERATOR .and. colon_token%text == "::") then
            colon_token = parser%consume()
        end if
        
        ! Get variable name
        name_token = parser%peek()
        if (name_token%kind == TK_IDENTIFIER) then
            name_token = parser%consume()
            var_name = name_token%text
            
            ! Create simple declaration node
            block
                integer, allocatable :: empty_dim_indices(:)
                allocate(empty_dim_indices(0))
                stmt_index = push_declaration(arena, type_name, var_name, &
                                            dimension_indices=empty_dim_indices, &
                                            line=type_token%line, column=type_token%column)
            end block
        end if
    end subroutine parse_simple_declaration

    ! Parse a simple implicit statement
    subroutine parse_simple_implicit(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: implicit_token, none_token
        character(len=:), allocatable :: implicit_type

        stmt_index = 0
        
        ! Get implicit keyword
        implicit_token = parser%consume()
        
        ! Check for 'none'
        none_token = parser%peek()
        if (none_token%kind == TK_KEYWORD .and. none_token%text == "none") then
            none_token = parser%consume()
            implicit_type = "none"
        else
            implicit_type = "default"
        end if
        
        ! Create implicit statement node
        if (implicit_type == "none") then
            stmt_index = push_implicit_statement(arena, .true., &
                                               line=implicit_token%line, column=implicit_token%column)
        else
            stmt_index = push_implicit_statement(arena, .false., &
                                               line=implicit_token%line, column=implicit_token%column)
        end if
    end subroutine parse_simple_implicit

    ! Handle single vs multi-variable declarations (duplicate of dispatcher logic)
    subroutine handle_variable_declaration(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        logical :: has_initializer, has_comma
        integer, allocatable :: decl_indices(:)
        
        
        ! Analyze the declaration structure
        call analyze_declaration_structure(parser, has_initializer, has_comma)
        
        if (has_initializer .and. .not. has_comma) then
            ! Single variable with initializer - use parse_declaration
            stmt_index = parse_declaration(parser, arena)
        else if (has_comma) then
            ! Multi-variable declaration - use parse_multi_declaration  
            decl_indices = parse_multi_declaration(parser, arena)
            if (allocated(decl_indices) .and. size(decl_indices) > 0) then
                stmt_index = decl_indices(1)  ! Return first declaration index
                
                ! Store additional indices if any
                if (size(decl_indices) > 1) then
                    allocate(additional_execution_indices(size(decl_indices) - 1))
                    additional_execution_indices = decl_indices(2:)
                end if
            else
                stmt_index = parse_declaration(parser, arena)  ! Fallback
            end if
        else
            ! Single variable without initializer - use parse_declaration
            stmt_index = parse_declaration(parser, arena)
        end if
    end subroutine handle_variable_declaration

    ! Check if the current position is a multi-variable assignment
    logical function is_multi_var_assignment(parser)
        type(parser_state_t), intent(in) :: parser
        integer :: pos
        type(token_t) :: token
        
        is_multi_var_assignment = .false.
        pos = parser%current_token
        
        ! Look ahead for pattern: identifier, comma, identifier, ..., =
        do while (pos <= size(parser%tokens))
            token = parser%tokens(pos)
            
            if (token%kind == TK_IDENTIFIER) then
                pos = pos + 1
                if (pos > size(parser%tokens)) exit
                
                token = parser%tokens(pos)
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    ! Found comma after identifier, continue looking
                    is_multi_var_assignment = .true.
                    pos = pos + 1
                    cycle
                else if (token%kind == TK_OPERATOR .and. token%text == "=" .and. is_multi_var_assignment) then
                    ! Found = and we already saw a comma, this is multi-var assignment
                    return
                else if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    ! Found = but no comma, this is single assignment
                    is_multi_var_assignment = .false.
                    return
                else
                    ! Something else, not an assignment
                    is_multi_var_assignment = .false.
                    return
                end if
            else
                ! Not identifier, exit
                exit
            end if
        end do
        
        is_multi_var_assignment = .false.
    end function is_multi_var_assignment

    ! Parse multi-variable assignment like "a, b, c = 1, 2, 3"
    subroutine parse_multi_variable_assignment(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable :: var_indices(:)  ! Store identifier indices directly
        integer, allocatable :: value_indices(:)  ! Store literal indices directly
        integer :: num_vars, num_values, i
        type(token_t) :: token
        integer :: target_index, value_index, literal_type
        integer, allocatable :: assignment_indices(:)
        
        stmt_index = 0
        allocate(var_indices(0))
        allocate(value_indices(0))
        allocate(assignment_indices(0))
        
        ! Parse left-hand side variables (a, b, c)
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                ! Create identifier node immediately
                target_index = push_identifier(arena, token%text, token%line, token%column)
                var_indices = [var_indices, target_index]
                
                ! Check for comma or equals
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume comma
                    cycle
                else if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume equals
                    exit
                else
                    ! Error condition
                    return
                end if
            else
                return
            end if
        end do
        
        num_vars = size(var_indices)
        if (num_vars == 0) return
        
        ! Parse right-hand side values (1, 2, 3)
        do while (.not. parser%is_at_end())
            ! Parse a value expression (could be literal, variable, etc.)
            token = parser%peek()
            if (token%kind == TK_NUMBER .or. token%kind == TK_STRING .or. token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD .or. &
                (token%kind == TK_OPERATOR .and. (token%text == '.true.' .or. token%text == '.false.'))) then
                token = parser%consume()
                ! Determine literal type based on token kind
                literal_type = get_literal_type_from_token_kind(token%kind, token%text)
                ! Create literal node immediately
                value_index = push_literal(arena, token%text, literal_type, token%line, token%column)
                value_indices = [value_indices, value_index]
                
                ! Check for comma or end
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume comma
                    cycle
                else
                    ! End of expression list
                    exit
                end if
            else if (token%kind == TK_NEWLINE .or. parser%is_at_end()) then
                exit
            else
                ! Try to consume the token and exit
                token = parser%consume()
                exit
            end if
        end do
        
        num_values = size(value_indices)
        
        ! Create individual assignment statements
        ! For each variable, assign corresponding value (or last value if fewer values)
        do i = 1, num_vars
            if (i <= num_values) then
                ! Use corresponding value
                target_index = var_indices(i)
                value_index = value_indices(i)
            else
                ! Use last value if not enough values provided
                target_index = var_indices(i)
                value_index = value_indices(num_values)
            end if
            
            if (target_index > 0 .and. value_index > 0) then
                assignment_indices = [assignment_indices, &
                    push_assignment(arena, target_index, value_index, &
                                  parser%tokens(parser%current_token-1)%line, &
                                  parser%tokens(parser%current_token-1)%column)]
            end if
        end do
        
        ! Return first assignment index and store rest in additional_execution_indices
        if (size(assignment_indices) > 0) then
            stmt_index = assignment_indices(1)
            
            if (size(assignment_indices) > 1) then
                if (allocated(additional_execution_indices)) deallocate(additional_execution_indices)
                allocate(additional_execution_indices(size(assignment_indices) - 1))
                additional_execution_indices = assignment_indices(2:)
            end if
        end if
        
    end subroutine parse_multi_variable_assignment

    ! Helper function to determine literal type from token kind
    function get_literal_type_from_token_kind(token_kind, token_text) result(literal_type)
        integer, intent(in) :: token_kind
        character(len=*), intent(in) :: token_text
        integer :: literal_type
        
        select case (token_kind)
        case (TK_NUMBER)
            ! Check if it contains a decimal point or exponent
            if (index(token_text, '.') > 0 .or. index(token_text, 'e') > 0 .or. &
                index(token_text, 'E') > 0 .or. index(token_text, 'd') > 0 .or. &
                index(token_text, 'D') > 0) then
                literal_type = LITERAL_REAL
            else
                literal_type = LITERAL_INTEGER
            end if
        case (TK_STRING)
            literal_type = LITERAL_STRING
        case (TK_OPERATOR)
            ! For logical literals (.true., .false.)
            if (token_text == '.true.' .or. token_text == '.false.') then
                literal_type = LITERAL_INTEGER  ! Treat as integer for now (should be LITERAL_LOGICAL)
            else
                literal_type = LITERAL_STRING  ! Default fallback
            end if
        case default
            ! For identifiers and other tokens, treat as string
            literal_type = LITERAL_STRING
        end select
    end function get_literal_type_from_token_kind

end module parser_execution_statements_module