module parser_dispatcher_module
    ! Statement dispatcher that delegates to appropriate parsing modules
    ! This implements the SRP by separating the switch logic from the implementations
    use lexer_core
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
            ! Other type keywords - check if it's a declaration
            if (has_double_colon(parser)) then
                ! Check if this is single or multi-variable declaration
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
                ! Could be function definition like "real function foo()"
                stmt_index = parse_function_or_expression(parser, arena)
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
        integer :: i

        has_double_colon = .false.
    do i = parser%current_token + 1, min(parser%current_token + 10, size(parser%tokens))
      if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text == "::") then
                has_double_colon = .true.
                exit
 else if (parser%tokens(i)%kind == TK_KEYWORD .or. parser%tokens(i)%kind == TK_EOF) then
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
                    exit   ! Stop on other keywords
                end if
            end if
        end do
    end function has_double_colon


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

end module parser_dispatcher_module
