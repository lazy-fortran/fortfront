module parser_dispatcher_module
    ! Statement dispatcher that delegates to appropriate parsing modules
    ! This implements the SRP by separating the switch logic from the implementations
    use iso_fortran_env, only: error_unit
    use lexer_core
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD
    use parser_state_module
    use parser_expressions_module
    use parser_declarations, only: parse_declaration, parse_multi_declaration, parse_derived_type_def
    use parser_utils, only: analyze_declaration_structure
    use parser_import_statements_module, only: parse_use_statement, parse_include_statement, &
                                              parse_module
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement, &
                                           parse_read_statement
    use parser_definition_statements_module, only: parse_function_definition, &
                                                  parse_subroutine_definition, &
                                                  parse_interface_block
    use parser_control_statements_module, only: parse_stop_statement, parse_return_statement, &
                                               parse_goto_statement, parse_error_stop_statement, &
                                               parse_cycle_statement, parse_exit_statement, &
                                               parse_end_statement
    use parser_memory_statements_module, only: parse_allocate_statement, parse_deallocate_statement
    use parser_execution_statements_module, only: parse_call_statement, parse_program_statement
    use parser_control_flow_module, only: parse_if, parse_do_loop, parse_select_case, &
                                         parse_where_construct, parse_associate
    use ast_core
    use ast_factory
    use parser_expressions_module, only: parse_expression, parse_range
    implicit none
    private

    public :: parse_statement_dispatcher, get_additional_indices, clear_additional_indices
    
    ! Module variable to store additional indices from multi-declaration parsing
    integer, allocatable :: additional_indices(:)

contains

    ! Parse a statement by dispatching to appropriate parsing module
    function parse_statement_dispatcher(tokens, arena) result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        integer :: target_index, value_index

        parser = create_parser_state(tokens)
        first_token = parser%peek()

        ! Dispatch based on first token
        select case (first_token%kind)
        case (TK_KEYWORD)
            select case (first_token%text)
            case ("use")
                stmt_index = parse_use_statement(parser, arena)
            case ("include")
                stmt_index = parse_include_statement(parser, arena)
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                stmt_index = parse_read_statement(parser, arena)
            case ("allocate")
                stmt_index = parse_allocate_statement(parser, arena)
            case ("deallocate")
                stmt_index = parse_deallocate_statement(parser, arena)
            case ("if")
                stmt_index = parse_if(parser, arena)
            case ("do")
                stmt_index = parse_do_loop(parser, arena)
            case ("where")
                stmt_index = parse_where_construct(parser, arena)
            case ("select")
                stmt_index = parse_select_case(parser, arena)
            case ("function")
                stmt_index = parse_function_definition(parser, arena)
            case ("subroutine")
                stmt_index = parse_subroutine_definition(parser, arena)
            case ("interface")
                stmt_index = parse_interface_block(parser, arena)
            case ("module")
                stmt_index = parse_module(parser, arena)
            case ("program")
                stmt_index = parse_program_statement(parser, arena)
            case ("type")
                stmt_index = parse_type_or_declaration(parser, arena)
            case ("real", "integer", "logical", "character", "complex")
                stmt_index = parse_type_or_declaration(parser, arena)
            case ("call")
                stmt_index = parse_call_statement(parser, arena)
            case ("stop")
                stmt_index = parse_stop_statement(parser, arena)
            case ("return")
                stmt_index = parse_return_statement(parser, arena)
            case ("go")
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case ("cycle")
                stmt_index = parse_cycle_statement(parser, arena)
            case ("exit")
                stmt_index = parse_exit_statement(parser, arena)
            case ("associate")
                stmt_index = parse_associate(parser, arena)
            case ("end")
                stmt_index = parse_end_statement(parser, arena)
            case default
                stmt_index = parse_as_expression(tokens, arena)
            end select
        case (TK_IDENTIFIER)
            ! Could be assignment or expression
            stmt_index = parse_assignment_or_expression(parser, arena)
        case (TK_COMMENT)
            ! Parse comment
            stmt_index = parse_comment(parser, arena)
        case (TK_NEWLINE)
            ! Parse blank line (newline token)
            stmt_index = parse_blank_line(parser, arena)
        case default
            ! Parse as expression
            stmt_index = parse_as_expression(tokens, arena)
        end select

    end function parse_statement_dispatcher

    ! Parse type declaration or derived type definition
    function parse_type_or_declaration(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: first_token, second_token
        logical :: is_derived_type_def

        first_token = parser%peek()
        is_derived_type_def = .false.
        

        if (first_token%text == "type") then
            ! Check if this is a derived type definition or variable declaration
            if (parser%current_token + 1 <= size(parser%tokens)) then
                second_token = parser%tokens(parser%current_token + 1)

                ! If second token is :: or identifier, it's a derived type definition
              if (second_token%kind == TK_OPERATOR .and. second_token%text == "::") then
                    is_derived_type_def = .true.
                else if (second_token%kind == TK_IDENTIFIER) then
                    is_derived_type_def = .true.
                end if
            end if

            if (is_derived_type_def) then
                stmt_index = parse_derived_type_def(parser, arena)
            else
                stmt_index = parse_declaration(parser, arena)
            end if
        else
            ! Other type keywords - assume it's a declaration unless proven otherwise
            ! This handles cases like "real(kind=real64) :: x" where kind parameters
            ! might interfere with double colon detection
            if (has_double_colon(parser)) then
                ! Confirmed declaration with :: - check if single or multi-variable
                block
                    logical :: has_initializer, has_comma
                    integer, allocatable :: decl_indices(:)
                    
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
                                allocate(additional_indices(size(decl_indices) - 1))
                                additional_indices = decl_indices(2:)
                            end if
                        else
                            stmt_index = parse_declaration(parser, arena)  ! Fallback
                        end if
                    else
                        ! Single variable without initializer - use parse_declaration
                        stmt_index = parse_declaration(parser, arena)
                    end if
                end block
            else
                ! Check if this looks like a function definition
                if (looks_like_function_definition(parser)) then
                    stmt_index = parse_function_or_expression(parser, arena)
                else
                    ! Default to declaration parsing for type keywords
                    ! This handles "real(kind=real64) :: x" where :: detection fails
                    stmt_index = parse_declaration(parser, arena)
                end if
            end if
        end if

    end function parse_type_or_declaration

    ! Parse assignment or expression
    function parse_assignment_or_expression(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: id_token, op_token
        integer :: target_index, value_index
        logical :: is_multi_assignment

        ! Check if this is a multi-variable assignment before consuming tokens
        is_multi_assignment = is_multi_var_assignment_dispatcher(parser)
        
        if (is_multi_assignment) then
            call parse_multi_variable_assignment_dispatcher(parser, arena, stmt_index)
        else
            ! Handle single assignment or expression
            id_token = parser%consume()
            op_token = parser%peek()

            if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                op_token = parser%consume()

                ! Create target identifier
                target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column)

                ! Parse value expression
                value_index = parse_range(parser, arena)

                ! Create assignment
                if (value_index > 0) then
                    stmt_index = push_assignment(arena, target_index, value_index, &
                                                  id_token%line, id_token%column)
                else
                    stmt_index = push_literal(arena, "! Error: missing value", &
                                               LITERAL_STRING, id_token%line, &
                                               id_token%column)
                end if
            else
                ! Not an assignment, treat as expression statement
                stmt_index = push_identifier(arena, id_token%text, id_token%line, id_token%column)
            end if
        end if

    end function parse_assignment_or_expression

    ! Parse function definition or expression
    function parse_function_or_expression(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: first_token, second_token

        first_token = parser%peek()

        ! Look ahead to see if next token is "function"
        if (parser%current_token + 1 <= size(parser%tokens)) then
            second_token = parser%tokens(parser%current_token + 1)
         if (second_token%kind == TK_KEYWORD .and. second_token%text == "function") then
                stmt_index = parse_function_definition(parser, arena)
                return
            end if
        end if

        ! Not a function definition, parse as expression
        stmt_index = parse_as_expression(parser%tokens, arena)

    end function parse_function_or_expression

    ! Parse as expression
    function parse_as_expression(tokens, arena) result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        stmt_index = parse_expression(tokens, arena)
    end function parse_as_expression

    ! Helper function to check for double colon
    logical function has_double_colon(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: i, paren_depth

        has_double_colon = .false.
        paren_depth = 0
        
        do i = parser%current_token + 1, min(parser%current_token + 50, size(parser%tokens))
            if (parser%tokens(i)%kind == TK_OPERATOR) then
                if (parser%tokens(i)%text == "(") then
                    paren_depth = paren_depth + 1
                else if (parser%tokens(i)%text == ")") then
                    paren_depth = paren_depth - 1
                else if (parser%tokens(i)%text == "::" .and. paren_depth == 0) then
                    has_double_colon = .true.
                    exit
                end if
            else if (parser%tokens(i)%kind == TK_EOF) then
                exit  ! Stop on EOF
            else if (parser%tokens(i)%kind == TK_KEYWORD .and. paren_depth == 0) then
                ! Only stop on keywords outside of parentheses
                ! Allow declaration attribute keywords to continue search
                if (parser%tokens(i)%text == "parameter" .or. &
                    parser%tokens(i)%text == "optional" .or. &
                    parser%tokens(i)%text == "intent" .or. &
                    parser%tokens(i)%text == "allocatable" .or. &
                    parser%tokens(i)%text == "pointer" .or. &
                    parser%tokens(i)%text == "target" .or. &
                    parser%tokens(i)%text == "dimension" .or. &
                    parser%tokens(i)%text == "in" .or. &
                    parser%tokens(i)%text == "out" .or. &
                    parser%tokens(i)%text == "inout") then
                    cycle  ! Continue searching
                else
                    exit   ! Stop on other keywords outside parentheses
                end if
            end if
        end do
    end function has_double_colon

    ! Check if this looks like a function definition (e.g., "real function foo()")
    logical function looks_like_function_definition(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: i
        
        looks_like_function_definition = .false.
        
        ! Look for "function" keyword within the next few tokens
        do i = parser%current_token + 1, min(parser%current_token + 10, size(parser%tokens))
            if (parser%tokens(i)%kind == TK_KEYWORD .and. parser%tokens(i)%text == "function") then
                looks_like_function_definition = .true.
                exit
            else if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text == "::") then
                ! Found :: before function - this is a declaration
                exit
            else if (parser%tokens(i)%kind == TK_EOF) then
                exit
            end if
        end do
    end function looks_like_function_definition


    ! Parse a comment token
    function parse_comment(parser, arena) result(comment_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: comment_index
        type(token_t) :: token
        type(comment_node) :: comment

        token = parser%consume()
        comment = create_comment(token%text, token%line, token%column)
        call arena%push(comment, "comment")
        comment_index = arena%size
    end function parse_comment

    ! Parse a blank line (newline token)
    function parse_blank_line(parser, arena) result(blank_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: blank_index
        type(token_t) :: token
        type(blank_line_node) :: blank_line
        integer :: count

        ! Count consecutive newlines
        count = 0
        do while (parser%current_token <= size(parser%tokens))
            token = parser%peek()
            if (token%kind /= TK_NEWLINE) exit
            count = count + 1
            token = parser%consume()
        end do

        ! Create blank line node with count of consecutive lines
        blank_line = create_blank_line(count, token%line, token%column)
        call arena%push(blank_line, "blank_line")
        blank_index = arena%size
    end function parse_blank_line

    ! Get additional indices from multi-declaration parsing
    function get_additional_indices() result(indices)
        integer, allocatable :: indices(:)
        
        if (allocated(additional_indices)) then
            allocate(indices(size(additional_indices)))
            indices = additional_indices
        else
            allocate(indices(0))
        end if
    end function get_additional_indices
    
    ! Clear additional indices after use
    subroutine clear_additional_indices()
        if (allocated(additional_indices)) then
            deallocate(additional_indices)
        end if
    end subroutine clear_additional_indices

    ! Check if the current position is a multi-variable assignment (dispatcher version)
    logical function is_multi_var_assignment_dispatcher(parser)
        type(parser_state_t), intent(in) :: parser
        integer :: pos
        type(token_t) :: token
        
        is_multi_var_assignment_dispatcher = .false.
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
                    is_multi_var_assignment_dispatcher = .true.
                    pos = pos + 1
                    cycle
                else if (token%kind == TK_OPERATOR .and. token%text == "=" .and. is_multi_var_assignment_dispatcher) then
                    ! Found = and we already saw a comma, this is multi-var assignment
                    return
                else if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    ! Found = but no comma, this is single assignment
                    is_multi_var_assignment_dispatcher = .false.
                    return
                else
                    ! Something else, not an assignment
                    is_multi_var_assignment_dispatcher = .false.
                    return
                end if
            else
                ! Not identifier, exit
                exit
            end if
        end do
        
        is_multi_var_assignment_dispatcher = .false.
    end function is_multi_var_assignment_dispatcher

    ! Parse multi-variable assignment like "a, b, c = 1, 2, 3" (dispatcher version)
    subroutine parse_multi_variable_assignment_dispatcher(parser, arena, stmt_index)
        use lexer_token_types, only: TK_NUMBER, TK_STRING, TK_KEYWORD, TK_NEWLINE
        use ast_types, only: LITERAL_INTEGER, LITERAL_REAL
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
                literal_type = get_literal_type_from_token_kind_dispatcher(token%kind, token%text)
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
        
        ! Return first assignment index and store rest in additional_indices
        if (size(assignment_indices) > 0) then
            stmt_index = assignment_indices(1)
            
            if (size(assignment_indices) > 1) then
                if (allocated(additional_indices)) deallocate(additional_indices)
                allocate(additional_indices(size(assignment_indices) - 1))
                additional_indices = assignment_indices(2:)
            end if
        end if
        
    end subroutine parse_multi_variable_assignment_dispatcher

    ! Helper function to determine literal type from token kind (dispatcher version)
    function get_literal_type_from_token_kind_dispatcher(token_kind, token_text) result(literal_type)
        use lexer_token_types, only: TK_NUMBER, TK_STRING, TK_OPERATOR
        use ast_types, only: LITERAL_INTEGER, LITERAL_REAL
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
                literal_type = LITERAL_LOGICAL
            else
                literal_type = LITERAL_STRING  ! Default fallback
            end if
        case default
            ! For identifiers and other tokens, check for boolean literals
            if (token_text == 'true' .or. token_text == 'false') then
                literal_type = LITERAL_LOGICAL
            else
                literal_type = LITERAL_STRING
            end if
        end select
    end function get_literal_type_from_token_kind_dispatcher

end module parser_dispatcher_module
