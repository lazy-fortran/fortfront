module parser_statement_core_module
    ! Core statement parsing utilities without circular dependencies
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_declaration
    use parser_expressions_module, only: parse_comparison
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement, parse_read_statement
    use parser_control_statements_module, only: parse_stop_statement, parse_return_statement, &
                                               parse_goto_statement, parse_error_stop_statement, &
                                               parse_cycle_statement, parse_exit_statement
    use parser_memory_statements_module, only: parse_allocate_statement, parse_deallocate_statement
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING
    use ast_nodes_control, only: association_t
    implicit none
    private

    public :: parse_statement_in_if_block_simple

contains

    ! Statement parsing for if blocks - simplified version without circular dependencies
    function parse_statement_in_if_block_simple(parser, arena, token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer :: stmt_index

        ! Simplified statement parsing for if blocks
        select case (token%kind)
        case (TK_KEYWORD)
            select case (token%text)
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                stmt_index = parse_read_statement(parser, arena)
            case ("call")
                stmt_index = parse_call_statement_simple(parser, arena)
            case ("integer", "real", "logical", "character", "complex", "double", "type")
                stmt_index = parse_declaration(parser, arena)
            case ("allocate")
                stmt_index = parse_allocate_statement(parser, arena)
            case ("deallocate")
                stmt_index = parse_deallocate_statement(parser, arena)
            case ("if")
                ! Simple if parsing without recursion
                stmt_index = parse_if_simple(parser, arena)
            case ("stop")
                stmt_index = parse_stop_statement(parser, arena)
            case ("return")
                stmt_index = parse_return_statement(parser, arena)
            case ("goto", "go")
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case ("cycle")
                stmt_index = parse_cycle_statement(parser, arena)
            case ("exit")
                stmt_index = parse_exit_statement(parser, arena)
            case ("associate")
                stmt_index = parse_associate_simple(parser, arena)
            case default
                stmt_index = skip_unknown_statement(parser)
            end select
        case default
            stmt_index = parse_assignment_simple(parser, arena)
        end select
    end function parse_statement_in_if_block_simple

    ! Simple call statement parser to break circular dependency
    function parse_call_statement_simple(parser, arena) result(stmt_index)
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

            ! For simplicity, no arguments parsed in if blocks
            allocate (arg_indices(0))

            ! Create call node
            stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, &
                                              line, column)
        else
            ! Error: expected subroutine name
            stmt_index = push_literal(arena, "! Error: expected subroutine name after 'call'", &
                                      LITERAL_STRING, line, column)
        end if
    end function parse_call_statement_simple

    ! Simple assignment parser without dependencies on parser_execution_statements
    function parse_assignment_simple(parser, arena) result(assign_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assign_index

        type(token_t) :: id_token, eq_token, rhs_token
        integer :: target_index, rhs_index
        
        assign_index = 0
        
        ! Get the identifier
        id_token = parser%peek()
        if (id_token%kind /= TK_IDENTIFIER) then
            return
        end if
        id_token = parser%consume()
        
        ! Check for assignment operator
        eq_token = parser%peek()
        if (eq_token%kind /= TK_OPERATOR .or. eq_token%text /= "=") then
            return
        end if
        eq_token = parser%consume()
        
        ! Create target identifier
        target_index = push_identifier(arena, id_token%text, &
                                     id_token%line, id_token%column)
        
        ! Parse simple RHS expression (just identifier or literal)
        rhs_token = parser%peek()
        if (rhs_token%kind == TK_IDENTIFIER) then
            rhs_token = parser%consume()
            rhs_index = push_identifier(arena, rhs_token%text, &
                                       rhs_token%line, rhs_token%column)
        else if (rhs_token%kind == TK_NUMBER) then
            rhs_token = parser%consume()
            rhs_index = push_literal(arena, rhs_token%text, &
                                    rhs_token%line, rhs_token%column, LITERAL_STRING)
        else if (rhs_token%kind == TK_STRING) then
            rhs_token = parser%consume()
            rhs_index = push_literal(arena, rhs_token%text, &
                                    rhs_token%line, rhs_token%column, LITERAL_STRING)
        else
            ! Use comparison parser for complex expressions
            rhs_index = parse_comparison(parser, arena)
        end if
        
        ! Create assignment node
        if (rhs_index > 0 .and. target_index > 0) then
            assign_index = push_assignment(arena, target_index, rhs_index, &
                                         id_token%line, id_token%column)
        end if
    end function parse_assignment_simple

    ! Skip unknown statement (utility function)
    function skip_unknown_statement(parser) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        integer :: stmt_index

        type(token_t) :: token

        ! Skip tokens until end of statement
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. &
                (token%kind == TK_KEYWORD .and. &
                 (token%text == "end" .or. token%text == "endif"))) then
                exit
            end if
            token = parser%consume()
        end do

        stmt_index = 0  ! No valid statement created
    end function skip_unknown_statement

    ! Simple if statement parser for function/subroutine bodies
    function parse_if_simple(parser, arena) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index
        
        type(token_t) :: if_token, then_token, token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        
        ! Consume 'if' keyword
        if_token = parser%consume()
        
        ! Parse condition (parentheses)
        condition_index = parse_comparison(parser, arena)
        
        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            token = parser%consume()
            
            ! Parse then body
            allocate(then_body_indices(0))
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    if (token%text == "else" .or. token%text == "end") then
                        exit
                    end if
                end if
                
                ! Parse a statement (avoid recursion)
                block
                    integer :: stmt_index
                    stmt_index = parse_statement_in_if_block_simple(parser, arena, token)
                    if (stmt_index > 0) then
                        then_body_indices = [then_body_indices, stmt_index]
                    else
                        token = parser%consume()  ! Skip unknown statement
                    end if
                end block
            end do
            
            ! Check for else
            allocate(else_body_indices(0))
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "else") then
                token = parser%consume()
                
                ! Parse else body
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "end") then
                        exit
                    end if
                    
                    ! Parse a statement
                    block
                        integer :: stmt_index
                        stmt_index = parse_statement_in_if_block_simple(parser, arena, token)
                        if (stmt_index > 0) then
                            else_body_indices = [else_body_indices, stmt_index]
                        else
                            token = parser%consume()  ! Skip unknown statement
                        end if
                    end block
                end do
            end if
            
            ! Consume "end if"
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "if") then
                    token = parser%consume()
                end if
            end if
            
            ! Create if node
            if_index = push_if(arena, condition_index, then_body_indices, &
                              else_body_indices=else_body_indices, &
                              line=if_token%line, column=if_token%column)
        else
            ! Single-line if statement
            if_index = push_literal(arena, "! Single-line if not yet supported", &
                                   LITERAL_STRING, if_token%line, if_token%column)
        end if
        
    end function parse_if_simple

    ! Simple associate statement parser for function/subroutine bodies
    function parse_associate_simple(parser, arena) result(assoc_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assoc_index
        
        type(token_t) :: token, first_token
        type(association_t), allocatable :: associations(:)
        integer, allocatable :: body_indices(:)
        integer :: i, assoc_count, line, column
        
        ! Consume 'associate' keyword
        first_token = parser%consume()
        line = first_token%line
        column = first_token%column
        
        ! Parse associations (simplified)
        allocate(associations(0))
        allocate(body_indices(0))
        
        ! Look for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            
            ! Parse associations
            assoc_count = 0
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                    exit
                end if
                
                ! Parse association: name => expr
                if (token%kind == TK_IDENTIFIER) then
                    block
                        character(len=:), allocatable :: assoc_name
                        integer :: target_index
                        type(association_t) :: new_assoc
                        
                        assoc_name = token%text
                        token = parser%consume()
                        
                        ! Look for =>
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=>") then
                            token = parser%consume()
                            
                            ! Parse target expression
                            target_index = parse_comparison(parser, arena)
                            
                            ! Create association
                            new_assoc%name = assoc_name
                            new_assoc%expr_index = target_index
                            
                            ! Add to associations array
                            associations = [associations, new_assoc]
                            assoc_count = assoc_count + 1
                        end if
                    end block
                end if
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                else if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
                    ! Skip unexpected token
                    token = parser%consume()
                end if
            end do
        end if
        
        ! Parse body statements until 'end associate'
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "associate"
                block
                    integer :: saved_pos
                    saved_pos = parser%current_token
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "associate") then
                        token = parser%consume()
                        exit
                    else
                        parser%current_token = saved_pos
                    end if
                end block
            end if
            
            ! Parse a statement
            block
                integer :: stmt_index
                stmt_index = parse_statement_in_if_block_simple(parser, arena, token)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                else
                    token = parser%consume()  ! Skip unknown statement
                end if
            end block
        end do
        
        ! Create associate node
        assoc_index = push_associate(arena, associations, body_indices, line, column)
        
    end function parse_associate_simple

end module parser_statement_core_module