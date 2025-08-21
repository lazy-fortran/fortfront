module parser_execution_statements_module
    ! Parser module for execution statement types (call, program)
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_range
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

        ! Note: Program body parsing requires significant dependencies
        ! This is a simplified version for the refactoring
        ! Full implementation would need parse_use_statement, parse_implicit_statement, etc.
        
        ! Create program node with current body
        prog_index = push_program(arena, program_name, body_indices, line, column)
    end function parse_program_statement

end module parser_execution_statements_module