module parser_select_constructs_module
    ! Parser module for SELECT CASE constructs
    use iso_fortran_env, only: error_unit
    use lexer_core
    use ast_types, only: LITERAL_STRING
    use parser_state_module
    use parser_expressions_module, only: parse_primary, parse_expression
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement, &
                                           parse_read_statement
    use parser_control_statements_module, only: parse_cycle_statement, parse_exit_statement, &
                                                parse_return_statement, parse_stop_statement, &
                                                parse_goto_statement, parse_error_stop_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use parser_basic_statement_module, only: parse_statement_body
    use ast_core
    use ast_factory, only: push_select_case, push_select_case_with_default, &
                            push_case_block, push_case_default, &
                            push_identifier, push_literal, push_assignment
    implicit none
    private

    public :: parse_select_case

contains

    function parse_select_case(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: select_token, case_token, lparen_token, rparen_token
        integer :: expr_index, default_index
        integer, allocatable :: case_indices(:)
        integer :: case_count, line, column
        character(len=20), dimension(2) :: end_keywords

        ! Consume 'select'
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect 'case'
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. case_token%text /= "case") then
            ! Error: expected 'case' after 'select'
            select_index = 0
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '(' after 'select case'
            select_index = 0
            return
        end if

        ! Parse expression to match
        expr_index = parse_expression(parser%tokens(parser%current_token:), arena)
        if (expr_index <= 0) then
            ! Error: expected expression in select case
            select_index = 0
            return
        end if

        ! Advance parser past the expression
        ! For now, assume expression consumes tokens until ')'
        do while (parser%current_token <= size(parser%tokens))
            rparen_token = parser%peek()
            if (rparen_token%kind == TK_OPERATOR .and. rparen_token%text == ")") then
                rparen_token = parser%consume()
                exit
            end if
            parser%current_token = parser%current_token + 1
        end do

        ! Parse case blocks
        allocate (case_indices(0))
        case_count = 0
        default_index = 0

        ! Define end keywords for statement parsing
        end_keywords = [character(len=20) :: "case", "end"]

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                if (case_token%text == "case") then
                    ! Parse case block
                    block
                        type(token_t) :: value_token
                        integer :: case_block_index
                        integer, allocatable :: value_indices(:), body_indices(:)

                        case_token = parser%consume()  ! consume 'case'

                        ! Check for default case
                        value_token = parser%peek()
                        if (value_token%kind == TK_KEYWORD .and. value_token%text == "default") then
                            value_token = parser%consume()  ! consume 'default'
                            
                            ! Parse default case body using parse_statement_body
                            body_indices = parse_statement_body(parser, arena, end_keywords)
                            
                            ! Store default case index
                            default_index = push_case_default(arena, body_indices, &
                                                              line=case_token%line, &
                                                              column=case_token%column)
                        else
                            ! Regular case with values
                            allocate (value_indices(0))

                            ! Expect '('
                            if (value_token%kind == TK_OPERATOR .and. value_token%text == "(") then
                                value_token = parser%consume()  ! consume '('

                                ! Parse case values (may be multiple)
                                do while (parser%current_token <= size(parser%tokens))
                                    ! Parse a case value
                                    block
                                        integer :: value_index
                                        value_index = parse_primary(parser, arena)
                                        if (value_index > 0) then
                                            value_indices = [value_indices, value_index]
                                        end if
                                    end block

                                    ! Check for comma or closing paren
                                    value_token = parser%peek()
                                    if (value_token%kind == TK_OPERATOR) then
                                        if (value_token%text == ",") then
                                            value_token = parser%consume()  ! consume ','
                                        else if (value_token%text == ")") then
                                            value_token = parser%consume()  ! consume ')'
                                            exit
                                        else
                                            exit
                                        end if
                                    else
                                        exit
                                    end if
                                end do
                            end if

                            ! Parse case body statements using parse_statement_body
                            body_indices = parse_statement_body(parser, arena, end_keywords)

                            ! Create case block node and add to list
                            if (size(value_indices) > 0) then
                                case_block_index = push_case_block(arena, value_indices, body_indices, &
                                                                   line=case_token%line, &
                                                                   column=case_token%column)
                                case_indices = [case_indices, case_block_index]
                                case_count = case_count + 1
                            end if
                        end if
                    end block
                else if (case_token%text == "end") then
                    ! Check for 'end select'
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "select") then
                            ! Found 'end select', consume both tokens and exit
                            case_token = parser%consume()  ! consume 'end'
                            case_token = parser%consume()  ! consume 'select'
                            exit
                        end if
                    end if
                else
                    ! Other keyword, skip
                    parser%current_token = parser%current_token + 1
                end if
            else
                ! Not a keyword, skip
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select case node
        if (default_index > 0) then
            select_index = push_select_case_with_default(arena, expr_index, case_indices, &
                                                         default_index, line=line, column=column)
        else
            select_index = push_select_case(arena, expr_index, case_indices, line=line, column=column)
        end if
    end function parse_select_case

end module parser_select_constructs_module