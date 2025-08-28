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
    use ast_core
    use ast_factory, only: push_select_case, push_identifier, push_literal, push_assignment
    implicit none
    private

    public :: parse_select_case

contains

    ! Local implementation to avoid circular dependency
    function parse_basic_stmt_local(tokens, arena, parent_index) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, allocatable :: stmt_indices(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        integer :: stmt_index

        parser = create_parser_state(tokens)
        first_token = parser%peek()

        ! Check if this is a declaration that might have multiple variables
        if (first_token%kind == TK_KEYWORD) then
            if (first_token%text == "real" .or. first_token%text == "integer" .or. &
                first_token%text == "logical" .or. first_token%text == "character") then
                ! Check if this is a single declaration with initializer
                ! If so, use parse_declaration for proper initializer handling
                block
                    logical :: has_initializer, has_comma
                    
                    ! Look ahead to check for = (initializer) and comma (multi-var)
                    has_initializer = .false.
                    has_comma = .false.
                    
                    call analyze_declaration_structure(parser, has_initializer, has_comma)
                    
                    if (has_initializer .and. .not. has_comma) then
                        ! Single variable with initializer - use parse_declaration
                        allocate (stmt_indices(1))
                        stmt_indices(1) = parse_declaration(parser, arena)
                        return
                    else
                        ! Multi-variable declaration - use parse_multi_declaration
                        stmt_indices = parse_multi_declaration(parser, arena)
                        return
                    end if
                end block
            end if
        end if

        ! For all other statements, parse as single statement
        allocate (stmt_indices(1))
        stmt_index = 0

        ! Handle different statement types (excluding control flow to avoid circular deps)
        if (first_token%kind == TK_KEYWORD) then
            select case (first_token%text)
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                stmt_index = parse_read_statement(parser, arena)
            case ("cycle")
                stmt_index = parse_cycle_statement(parser, arena)
            case ("exit")
                stmt_index = parse_exit_statement(parser, arena)
            case ("return")
                stmt_index = parse_return_statement(parser, arena, parent_index)
            case ("stop")
                stmt_index = parse_stop_statement(parser, arena)
            case ("go")
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case default
                ! Unknown or control flow keyword - create placeholder
                stmt_index = 0
            end select
        else if (first_token%kind == TK_IDENTIFIER) then
            block
                type(token_t) :: id_token, op_token
                integer :: target_index, value_index

                id_token = parser%consume()
                op_token = parser%peek()

                if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                    op_token = parser%consume()  ! consume '='
                    target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column, parent_index)
                    ! Get remaining tokens for expression parsing
                    block
                        type(token_t), allocatable :: expr_tokens(:)
                        integer :: remaining_count
                        remaining_count = size(tokens) - parser%current_token + 1
                        if (remaining_count > 0) then
                            allocate (expr_tokens(remaining_count))
                            expr_tokens = tokens(parser%current_token:)
                            value_index = parse_expression(expr_tokens, arena)
                            if (value_index > 0) then
                                stmt_index = push_assignment(arena, target_index, value_index, &
                                                             id_token%line, id_token%column, &
                                                             parent_index)
                            end if
                        end if
                    end block
                end if
            end block
        end if

        ! If we couldn't parse it, create a placeholder with debug info
        if (stmt_index == 0) then
            block
                character(len=256) :: debug_msg
                character(len=64) :: token_text
                integer :: debug_len, i

                debug_msg = "! Unparsed: "
                debug_len = len_trim(debug_msg)

                ! Add first few tokens for debugging
                do i = 1, min(3, size(tokens))
                    if (tokens(i)%kind == TK_EOF) exit
                    if (len_trim(tokens(i)%text) > 0) then
                        token_text = trim(tokens(i)%text)
                        if (debug_len + len_trim(token_text) + 1 < 250) then
                            debug_msg = debug_msg(1:debug_len)//" "//trim(token_text)
                            debug_len = len_trim(debug_msg)
                        end if
                    end if
                end do

                stmt_index = push_literal(arena, trim(debug_msg), LITERAL_STRING, &
                                          first_token%line, first_token%column)
            end block
        end if

        stmt_indices(1) = stmt_index
    end function parse_basic_stmt_local

    function parse_select_case(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: select_token, case_token, lparen_token, rparen_token
        integer :: expr_index
        integer, allocatable :: case_indices(:)
        integer :: case_count, line, column

        ! Consume 'select'
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect 'case'
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. case_token%text /= "case") then
            ! Error: expected 'case' after 'select'
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '(' after 'select case'
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

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                if (case_token%text == "case") then
                    ! Parse case block
                    block
                        type(case_wrapper) :: new_case
                        type(token_t) :: value_token
                        class(ast_node), allocatable :: case_value

                        case_token = parser%consume()  ! consume 'case'

                        ! Check for default case
                        value_token = parser%peek()
            if (value_token%kind == TK_KEYWORD .and. value_token%text == "default") then
                            value_token = parser%consume()  ! consume 'default'
                            new_case%case_type = "case_default"
                        else
                            new_case%case_type = "case"

                            ! Expect '('
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == "(") then
                                value_token = parser%consume()  ! consume '('

                                ! Parse case value
                                block
                                    type(token_t), allocatable :: value_tokens(:)
                                    integer :: remaining_count, j

                                    ! Count remaining tokens until ')'
                                    remaining_count = 0
                                    do j = parser%current_token, size(parser%tokens)
                                        if (parser%tokens(j)%kind == TK_OPERATOR .and. &
                                            parser%tokens(j)%text == ")") then
                                            exit
                                        end if
                                        remaining_count = remaining_count + 1
                                    end do

                                    if (remaining_count > 0) then
                                        allocate (value_tokens(remaining_count))
                                        value_tokens = parser%tokens(&
                                            parser%current_token:parser%current_token+&
                                            remaining_count-1)
                                        ! Parse the value as a primary expression
                                        block
                                            integer :: value_index
                                            value_index = parse_primary(parser, arena)
                               if (value_index > 0 .and. value_index <= arena%size) then
                                    if (allocated(arena%entries(value_index)%node)) then
                                        new_case%value = arena%entries(value_index)%node
                                                end if
                                            end if
                                        end block
                                    end if
                                end block

                                ! Expect ')'
                                value_token = parser%peek()
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == ")") then
                                    value_token = parser%consume()  ! consume ')'
                                end if
                            end if
                        end if

                        ! Parse case body statements until next 'case' or 'end select'
                        block
                            integer, allocatable :: body_indices(:)
                            integer :: stmt_index, body_count
                            type(token_t) :: next_token

                            allocate (body_indices(0))
                            body_count = 0

                            ! Parse statements until next case or end select
                            do while (parser%current_token <= size(parser%tokens))
                                next_token = parser%peek()

                                if (next_token%kind == TK_KEYWORD) then
                       if (next_token%text == "case" .or. next_token%text == "end") then
                                        exit
                                    end if
                                end if

                                ! Parse a statement
                                block
                                    type(token_t), allocatable :: stmt_tokens(:)
                                    integer :: stmt_start, stmt_end, k

                                    stmt_start = parser%current_token
                                    stmt_end = stmt_start

                                    ! Find end of statement (same line)
                                    do k = stmt_start, size(parser%tokens)
                                        if (parser%tokens(k)%kind == TK_EOF) then
                                            stmt_end = k
                                            exit
                                        end if
   if (k > stmt_start .and. parser%tokens(k)%line > parser%tokens(stmt_start)%line) then
                                            stmt_end = k - 1
                                            exit
                                        end if
                                        stmt_end = k
                                    end do

                                    if (stmt_end >= stmt_start) then
                                       allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                                        stmt_tokens(stmt_end - stmt_start + 2)%text = ""

                                        block
                                            integer, allocatable :: stmt_indices(:)
                                            integer :: m
                          stmt_indices = parse_basic_stmt_local(stmt_tokens, arena)

                                            do m = 1, size(stmt_indices)
                                                if (stmt_indices(m) > 0) then
                                          body_indices = [body_indices, stmt_indices(m)]
                                                    body_count = body_count + 1
                                                end if
                                            end do
                                        end block

                                        deallocate (stmt_tokens)
                                    end if

                                    parser%current_token = stmt_end + 1
                                end block
                            end do

                            ! Convert body indices to ast_node_wrapper array
                            if (body_count > 0) then
                                allocate (new_case%body(body_count))
                                do body_count = 1, size(body_indices)
     if (body_indices(body_count) > 0 .and. body_indices(body_count) <= arena%size) then
                       if (allocated(arena%entries(body_indices(body_count))%node)) then
           new_case%body(body_count)%node = arena%entries(body_indices(body_count))%node
                        new_case%body(body_count)%stack_index = body_indices(body_count)
                                        end if
                                    end if
                                end do
                            else
                                allocate (new_case%body(0))
                            end if
                        end block

                        ! Add to cases array (simplified for now)
                        case_count = case_count + 1
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
        select_index = push_select_case(arena, expr_index, line=line, column=column)

        if (allocated(case_indices)) deallocate (case_indices)
    end function parse_select_case

end module parser_select_constructs_module