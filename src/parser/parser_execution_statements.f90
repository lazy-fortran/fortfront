module parser_execution_statements_module
    ! Parser module for execution statement types (call, program)
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_range
    use parser_declarations, only: parse_declaration
    use parser_io_statements_module, only: parse_print_statement
    use parser_memory_statements_module, only: parse_allocate_statement, parse_deallocate_statement
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING
    implicit none
    private

    public :: parse_call_statement, parse_program_statement

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

        ! Consume 'program' keyword
        token = parser%consume()
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
                    ! Use full declaration parser to handle initializers properly
                    stmt_index = parse_declaration(parser, arena)
                case ("print")
                    stmt_index = parse_print_statement(parser, arena)
                case ("allocate")
                    stmt_index = parse_allocate_statement(parser, arena)
                case ("deallocate")
                    stmt_index = parse_deallocate_statement(parser, arena)
                case ("if")
                    ! Simple inline if parsing to avoid circular dependency
                    stmt_index = parse_simple_if(parser, arena)
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

    ! Parse a simple assignment statement
    subroutine parse_assignment_statement(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: id_token, op_token
        integer :: target_index, value_index

        stmt_index = 0
        
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

end module parser_execution_statements_module