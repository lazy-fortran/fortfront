module parser_do_constructs_module
    ! Parser module for DO constructs (do loops, do while)
    use iso_fortran_env, only: error_unit
    use lexer_core
    use ast_types, only: LITERAL_STRING
    use parser_state_module
    use parser_expressions_module, only: parse_logical_or, parse_range, parse_expression
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement, &
                                           parse_read_statement
    use parser_control_statements_module, only: parse_cycle_statement, parse_exit_statement, &
                                                parse_return_statement, parse_stop_statement, &
                                                parse_goto_statement, parse_error_stop_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_do_loop, push_do_while, push_identifier, push_literal, push_assignment
    implicit none
    private

    public :: parse_do_loop, parse_do_while, parse_do_while_from_do

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

    function parse_do_loop(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token, var_token, eq_token, comma_token
        character(len=:), allocatable :: var_name
        integer :: start_index, end_index, step_index
        integer :: line, column

        step_index = 0  ! Initialize to 0 (no step)
        loop_index = 0  ! Initialize to 0 (failure) in case of early return

        ! Starting to parse do loop

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        ! Check if it's a do while loop
        var_token = parser%peek()
        if (var_token%kind == TK_KEYWORD .and. var_token%text == "while") then
            ! Parse as do while loop
            loop_index = parse_do_while_from_do(parser, arena, line, column)
            return
        end if

        ! Get variable name
        var_token = parser%consume()
        if (var_token%kind /= TK_IDENTIFIER) then
            ! Error: expected identifier
            ! ERROR - expected identifier
            return
        end if
        var_name = var_token%text
        ! Got variable name

        ! Expect '='
        eq_token = parser%consume()
        if (eq_token%kind /= TK_OPERATOR .or. eq_token%text /= "=") then
            ! Error: expected '='
            return
        end if

        ! Parse start expression (now handles full expressions)
        start_index = parse_range(parser, arena)
        
        if (start_index <= 0) then
            ! Failed to parse start expression
            loop_index = 0
            return
        end if

        ! Expect ','
        comma_token = parser%consume()
        if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ",") then
            ! Error: expected ','
            loop_index = 0
            return
        end if

        ! Parse end expression
        end_index = parse_range(parser, arena)
        
        if (end_index <= 0) then
            ! Failed to parse end expression
            loop_index = 0
            return
        end if

        ! Check for optional step
        if (.not. parser%is_at_end()) then
            comma_token = parser%peek()
            if (comma_token%kind == TK_OPERATOR .and. comma_token%text == ",") then
                comma_token = parser%consume()  ! consume comma
                step_index = parse_range(parser, arena)
            end if
        end if

        ! Parse body statements until 'end do'
        block
            integer, allocatable :: body_indices(:)
            integer :: stmt_index
            integer :: body_count, stmt_start, stmt_end, j
            type(token_t), allocatable :: stmt_tokens(:)

            allocate (body_indices(0))
            body_count = 0

            ! Create do loop node placeholder first to get the parent index
            if (step_index > 0) then
                loop_index = push_do_loop(arena, var_name, start_index, end_index, &
                                     step_index=step_index, body_indices=[integer::], &
                                          line=line, column=column)
            else
                loop_index = push_do_loop(arena, var_name, start_index, end_index, &
                                    body_indices=[integer::], line=line, column=column)
            end if

            ! Parse body statements
            do while (parser%current_token <= size(parser%tokens))
                ! Check for 'end do'
                block
                    type(token_t) :: current_token
                    current_token = parser%peek()

            if (current_token%kind == TK_KEYWORD .and. current_token%text == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                              parser%tokens(parser%current_token + 1)%text == "do") then
                                ! Found 'end do', consume both tokens and exit
                                current_token = parser%consume()  ! consume 'end'
                                current_token = parser%consume()  ! consume 'do'
                                exit
                            end if
                        end if
                    end if
                end block

                ! Parse statement until end of line or semicolon
                stmt_start = parser%current_token
                
                ! Skip leading semicolons to get to actual statement
                do while (stmt_start <= size(parser%tokens) .and. &
                         parser%tokens(stmt_start)%kind == TK_OPERATOR .and. &
                         parser%tokens(stmt_start)%text == ";")
                    stmt_start = stmt_start + 1
                end do
                
                stmt_end = stmt_start

                ! Find end of current statement (same line or semicolon boundary)
                do j = stmt_start, size(parser%tokens)
                    if (parser%tokens(j)%kind == TK_EOF) then
                        stmt_end = j
                        exit
                    end if
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) then
                        stmt_end = j - 1
                        exit
                    end if
                    ! Check for semicolon as statement separator
                    if (j > stmt_start .and. parser%tokens(j)%kind == TK_OPERATOR .and. &
                        parser%tokens(j)%text == ";") then
                        stmt_end = j - 1
                        exit
                    end if
                    stmt_end = j
                end do

                ! Extract statement tokens
                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""
              stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
      stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

                    ! Parse the statement (may return multiple indices for &
                    ! multi-variable declarations)
                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: k
                        stmt_indices = parse_basic_stmt_local(stmt_tokens, arena, loop_index)

                        ! Add all parsed statements to body
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                                body_count = body_count + 1
                            end if
                        end do
                    end block

                    deallocate (stmt_tokens)
                end if

                ! Move to next statement
                ! If we stopped at a semicolon, skip over it
                if (stmt_end + 1 <= size(parser%tokens)) then
                    if (parser%tokens(stmt_end + 1)%kind == TK_OPERATOR .and. &
                        parser%tokens(stmt_end + 1)%text == ";") then
                        parser%current_token = stmt_end + 2
                    else
                        parser%current_token = stmt_end + 1
                    end if
                else
                    parser%current_token = stmt_end + 1
                end if
            end do

            ! Update the do loop node with the actual body indices
            if (allocated(arena%entries(loop_index)%node)) then
                select type(node => arena%entries(loop_index)%node)
                type is (do_loop_node)
                    if (allocated(body_indices)) then
                        node%body_indices = body_indices
                    end if
                end select
            end if
            ! Successfully created do loop node
        end block

    end function parse_do_loop

    function parse_do_while(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token
        integer :: line, column

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        loop_index = parse_do_while_from_do(parser, arena, line, column)
    end function parse_do_while

    ! Helper function for parsing do while from do token
    function parse_do_while_from_do(parser, arena, line, column) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line, column
        integer :: loop_index

        type(token_t) :: while_token, lparen_token, rparen_token
        integer :: condition_index
        integer, allocatable :: body_indices(:)

        ! Consume 'while'
        while_token = parser%consume()
        if (while_token%kind /= TK_KEYWORD .or. while_token%text /= "while") then
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            return
        end if

        ! Parse condition
        condition_index = parse_logical_or(parser, arena)

        ! Expect ')'
        rparen_token = parser%consume()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            return
        end if

        ! Parse body statements until 'end' (same logic as if blocks)
        block
            integer, allocatable :: temp_body_indices(:)
            type(token_t) :: token
            integer :: stmt_count, stmt_index
            integer :: stmt_start, stmt_end, j
            type(token_t), allocatable :: stmt_tokens(:)

            allocate (temp_body_indices(0))
            stmt_count = 0

            do while (.not. parser%is_at_end())
                token = parser%peek()

                ! Check for 'end do' keywords
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Check if next token is 'do'
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "do") then
                            exit  ! Found 'end do'
                        end if
                    end if
                end if

                ! Parse statement until end of line (same approach as if blocks)
                stmt_start = parser%current_token
                stmt_end = stmt_start

                ! Find end of current statement (same line)
                do j = stmt_start, size(parser%tokens)
                    if (parser%tokens(j)%kind == TK_EOF) then
                        stmt_end = j
                        exit
                    end if
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) then
                        stmt_end = j - 1
                        exit
                    end if
                    stmt_end = j
                end do

                ! Extract statement tokens
                if (stmt_end >= stmt_start) then
                    allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                    stmt_tokens(stmt_end - stmt_start + 2)%text = ""
              stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
      stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

                    ! Parse the statement (may return multiple indices for &
                    ! multi-variable declarations)
                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: n
                        stmt_indices = parse_basic_stmt_local(stmt_tokens, arena)

                        ! Add all parsed statements to body
                        do n = 1, size(stmt_indices)
                            if (stmt_indices(n) > 0) then
                                temp_body_indices = [temp_body_indices, stmt_indices(n)]
                                stmt_count = stmt_count + 1
                            end if
                        end do
                    end block

                    deallocate (stmt_tokens)
                end if

                parser%current_token = stmt_end + 1
            end do

            body_indices = temp_body_indices
        end block

        ! Consume 'end do' tokens
        block
            type(token_t) :: token
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                while_token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "do") then
                    while_token = parser%consume()  ! consume 'do'
                end if
            end if
        end block

        ! Create do while node
        loop_index = push_do_while(arena, condition_index, body_indices=body_indices, &
                                   line=line, column=column)

        if (allocated(body_indices)) deallocate (body_indices)
    end function parse_do_while_from_do

end module parser_do_constructs_module