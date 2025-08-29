module parser_module_structures_module
    ! Module structure parsing for module definitions and bodies
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory, only: push_module_structured, push_implicit_statement, &
                           push_assignment, push_identifier, push_literal
    use parser_declarations, only: parse_declaration
    use ast_types, only: LITERAL_STRING
    ! Temporarily removed to avoid circular dependency
    ! Will be added back after refactoring is complete
    implicit none
    private

    public :: parse_module

contains

    function parse_module(parser, arena) result(module_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: module_index
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        integer :: line, column
        integer, allocatable :: declaration_indices(:), procedure_indices(:)
        logical :: has_contains, in_contains_section
        integer :: stmt_index

        ! Consume 'module' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
        else
            module_name = "unnamed_module"
        end if

        ! Initialize arrays
        allocate(declaration_indices(0))
        allocate(procedure_indices(0))
        has_contains = .false.
        in_contains_section = .false.

        ! Minimal parsing to detect structure and consume tokens
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of module
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "module"
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "module") then
                        ! Consume "end module"
                        token = parser%consume()  ! consume "end"
                        token = parser%consume()  ! consume "module"
                        exit
                    end if
                end if
            end if

            ! Check for contains keyword
            if (token%kind == TK_KEYWORD .and. token%text == "contains") then
                has_contains = .true.
                in_contains_section = .true.
                token = parser%consume()  ! consume "contains"
                cycle  ! Continue to next iteration
            end if
            
            ! Parse declarations in module body (before contains)
            if (.not. in_contains_section) then
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("integer", "real", "logical", "character", "complex")
                        stmt_index = parse_declaration(parser, arena)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle  ! Continue to next iteration
                    case ("implicit")
                        ! Parse implicit statement
                        call parse_simple_implicit_in_module(parser, arena, stmt_index)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle  ! Continue to next iteration
                    end select
                else if (token%kind == TK_IDENTIFIER) then
                    ! Handle assignments and other statements in module body
                    ! For now, parse as a simple assignment statement
                    block
                        type(token_t) :: id_token, eq_token
                        integer :: target_index, assign_index
                        
                        id_token = parser%consume()  ! Get the identifier
                        
                        ! Check if it's an assignment
                        eq_token = parser%peek()
                        if (eq_token%kind == TK_OPERATOR .and. eq_token%text == "=") then
                            eq_token = parser%consume()  ! Consume '='
                            
                            ! Create target identifier
                            target_index = push_identifier(arena, id_token%text, &
                                                         id_token%line, id_token%column)
                            
                            ! For simple module assignments, just consume the rest of the line
                            ! We'll create a simple identifier node for the RHS
                            block
                                type(token_t) :: rhs_token
                                integer :: rhs_index
                                
                                rhs_token = parser%peek()
                                if (rhs_token%kind == TK_NUMBER .or. rhs_token%kind == TK_IDENTIFIER) then
                                    rhs_token = parser%consume()
                                    
                                    ! Create identifier or literal node for RHS
                                    if (rhs_token%kind == TK_IDENTIFIER) then
                                        rhs_index = push_identifier(arena, rhs_token%text, &
                                                                   rhs_token%line, rhs_token%column)
                                    else
                                        ! For numbers, create as literal
                                        rhs_index = push_literal(arena, rhs_token%text, &
                                                                rhs_token%line, rhs_token%column, LITERAL_STRING)
                                    end if
                                    
                                    if (rhs_index > 0 .and. target_index > 0) then
                                        ! Create assignment node
                                        assign_index = push_assignment(arena, target_index, rhs_index, &
                                                                     id_token%line, id_token%column)
                                        if (assign_index > 0) then
                                            declaration_indices = [declaration_indices, assign_index]
                                        end if
                                    end if
                                end if
                            end block
                        else
                            ! Not an assignment, just consume the identifier
                            ! This handles other statement types that we don't parse fully yet
                        end if
                    end block
                    cycle  ! Continue to next iteration
                end if
            end if

            ! Parse subroutine definitions for contains section (temporarily simplified)
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. token%text == "subroutine") then
                ! Temporarily skip procedure parsing to avoid circular dependency
                ! This will be restored after refactoring is complete
                call skip_procedure_body(parser, "subroutine")
                cycle
            end if
            
            ! Parse function definitions for contains section (temporarily simplified)
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. token%text == "function") then
                ! Temporarily skip procedure parsing to avoid circular dependency
                ! This will be restored after refactoring is complete
                call skip_procedure_body(parser, "function")
                cycle
            end if
            
            ! Handle comments specially - skip them without disrupting module parsing
            if (token%kind == TK_COMMENT) then
                token = parser%consume()  ! Skip the comment token
                cycle  ! Continue to next iteration
            end if
            
            ! Handle newlines to avoid getting stuck
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()  ! Skip newline
                cycle  ! Continue to next iteration
            end if
            
            ! Only advance if we haven't handled this token specifically
            ! (declarations and contains are handled above with cycle)
            if (in_contains_section) then
                ! Only consume tokens that are not function/subroutine keywords to prevent
                ! interfering with multiple procedure definitions in contains section
                if (.not. (token%kind == TK_KEYWORD .and. &
                          (token%text == "function" .or. token%text == "subroutine"))) then
                    token = parser%consume()  ! Advance for non-procedure tokens
                end if
            else
                ! For any unhandled token in module body, consume and continue
                ! This prevents infinite loops with unexpected tokens
                token = parser%consume()  ! Advance for unhandled tokens
            end if
        end do

        ! Create module node with proper structure
        module_index = push_module_structured(arena, module_name, declaration_indices, &
                                             procedure_indices, has_contains, line, column)
    end function parse_module
    
    ! Parse a simple implicit statement in module context
    subroutine parse_simple_implicit_in_module(parser, arena, stmt_index)
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
    end subroutine parse_simple_implicit_in_module

    ! Temporary helper to skip procedure bodies during refactoring
    subroutine skip_procedure_body(parser, proc_type)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: proc_type
        type(token_t) :: token
        integer :: nesting_level
        
        nesting_level = 1  ! We're already inside a procedure
        
        ! Consume the procedure keyword
        token = parser%consume()
        
        ! Skip the procedure name if present
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
        end if
        
        ! Skip until matching "end <proc_type>"
        do while (.not. parser%is_at_end() .and. nesting_level > 0)
            token = parser%peek()
            
            if (token%kind == TK_KEYWORD) then
                if (token%text == proc_type) then
                    nesting_level = nesting_level + 1
                else if (token%text == "end") then
                    ! Check if next token is our procedure type
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == proc_type) then
                            nesting_level = nesting_level - 1
                        end if
                    end if
                end if
            end if
            
            token = parser%consume()
        end do
    end subroutine skip_procedure_body

end module parser_module_structures_module