module parser_control_flow_module
    ! Parser module for control flow constructs (if, do, select case)
    use iso_fortran_env, only: error_unit
    use lexer_core
    use ast_types, only: LITERAL_STRING
    use parser_state_module
  use parser_expressions_module, only: parse_primary, parse_expression, &
                                       parse_logical_or, parse_range
    use parser_statements_module, only: parse_print_statement, parse_write_statement, &
                                        parse_read_statement, &
                                        parse_cycle_statement, parse_exit_statement, &
                                        parse_return_statement, parse_call_statement, &
                                        parse_stop_statement, parse_goto_statement, &
                                        parse_error_stop_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_if, push_do_loop, push_do_while, push_select_case, &
                           push_assignment, push_identifier, push_literal, push_where, &
                           push_case_block, push_case_default, push_select_case_with_default
    implicit none
    private

    public :: parse_if, parse_do_loop, parse_do_while, parse_select_case
    public :: parse_if_condition, parse_if_body, parse_elseif_block
    public :: parse_do_while_from_do, parse_basic_statement_multi, parse_statement_body
    public :: parse_where_construct, parse_associate

contains

    ! Parse if statement
    function parse_if(parser, arena) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index

        type(token_t) :: if_token, then_token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)
        integer :: elseif_count

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (should be in parentheses for standard if/then/endif)
        condition_index = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            ! Standard if/then/endif block
            then_token = parser%consume()

            ! Create if node placeholder first to get the parent index
            if_index = push_if(arena, condition_index, [integer::], &
                               line=if_token%line, column=if_token%column)

            ! Parse then body statements with the if node as parent
            then_body_indices = parse_if_body(parser, arena, if_index)

            ! Check for elseif/else blocks
            elseif_count = 0
            allocate (elseif_indices(0))

            do while (.not. parser%is_at_end())
                then_token = parser%peek()

                if (then_token%kind == TK_KEYWORD) then
                 if (then_token%text == "elseif" .or. then_token%text == "else if") then
                        ! Parse elseif block
                        block
                            integer :: elseif_pair(2)
                            elseif_pair = parse_elseif_block(parser, arena)
                            elseif_indices = [elseif_indices, elseif_pair]
                            elseif_count = elseif_count + 1
                        end block
                    else if (then_token%text == "else") then
                        ! Check if next token is "if" (for "else if")
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                              parser%tokens(parser%current_token + 1)%text == "if") then
                                ! Parse as elseif block
                                block
                                    integer :: elseif_pair(2)
                                    elseif_pair = parse_elseif_block(parser, arena)
                                    elseif_indices = [elseif_indices, elseif_pair]
                                    elseif_count = elseif_count + 1
                                end block
                                cycle  ! Continue looking for more elseif/else blocks
                            end if
                        end if
                        ! Parse else block
                        then_token = parser%consume()  ! consume 'else'
                        else_body_indices = parse_if_body(parser, arena, if_index)
                        exit
              else if (then_token%text == "endif") then
                        ! End of if statement - single token
                        then_token = parser%consume()
                        exit
                    else if (then_token%text == "end") then
                        ! Check if next token is "if" (two tokens: "end if")
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "if") then
                                ! End of if statement - two tokens
                                then_token = parser%consume()  ! consume "end"
                                then_token = parser%consume()  ! consume "if"
                                exit
                            end if
                        end if
                    else
                        ! Other statement, stop parsing if block
                        exit
                    end if
                else
                    ! Not a keyword, continue parsing body
                    exit
                end if
            end do

            ! Update the if node with the actual body indices
            if (allocated(arena%entries(if_index)%node)) then
                select type(node => arena%entries(if_index)%node)
                type is (if_node)
                    if (allocated(then_body_indices)) then
                        node%then_body_indices = then_body_indices
                    end if
                    if (allocated(else_body_indices)) then
                        node%else_body_indices = else_body_indices  
                    end if
                    if (allocated(elseif_indices)) then
                        if (size(elseif_indices) > 0 .and. mod(size(elseif_indices), 2) == 0) then
                            allocate (node%elseif_blocks(size(elseif_indices)/2))
                            do elseif_count = 1, size(elseif_indices)/2
                                node%elseif_blocks(elseif_count)%condition_index = elseif_indices(2*elseif_count - 1)
                                node%elseif_blocks(elseif_count)%body_indices = [elseif_indices(2*elseif_count)]
                            end do
                        end if
                    end if
                end select
            end if
        else
            ! One-line if statement (no then keyword)
            allocate (then_body_indices(1))

            ! Parse the single statement
            block
                integer :: stmt_index
                type(token_t), allocatable :: remaining_tokens(:)
                integer :: i, n

                ! Count remaining tokens
                n = 0
                do i = parser%current_token, size(parser%tokens)
                    n = n + 1
                end do

                ! Extract remaining tokens
                allocate (remaining_tokens(n))
                remaining_tokens = parser%tokens(parser%current_token:)

                ! Use parse_basic_statement_multi
                block
                    integer, allocatable :: stmt_indices(:)
                    stmt_indices = parse_basic_statement_multi(remaining_tokens, arena)
                    if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0) then
                        then_body_indices(1) = stmt_indices(1)
                    end if
                end block
            end block

            ! Create if node with no elseif/else blocks
            allocate (elseif_indices(0))
            allocate (else_body_indices(0))
            if_index = push_if(arena, condition_index, then_body_indices, &
                               elseif_indices=elseif_indices, &
                               else_body_indices=else_body_indices, &
                               line=if_token%line, column=if_token%column)
        end if

    end function parse_if

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable :: remaining_tokens(:)
        integer :: i, n

        ! Check for opening parenthesis
        paren_token = parser%peek()
        if (paren_token%kind == TK_OPERATOR .and. paren_token%text == "(") then
            paren_token = parser%consume()  ! consume '('

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse the condition expression
            condition_index = parse_expression(remaining_tokens, arena)

            ! Advance parser past the condition tokens
            ! Simple approach: advance until we find the closing paren or 'then'
            do while (.not. parser%is_at_end())
                paren_token = parser%peek()
                if (paren_token%kind == TK_OPERATOR .and. paren_token%text == ")") then
                    paren_token = parser%consume()  ! consume the ')'
                    exit
          else if (paren_token%kind == TK_KEYWORD .and. paren_token%text == "then") then
                    exit  ! Don't consume 'then', let caller handle it
                end if
                paren_token = parser%consume()
            end do
        else
            ! No parentheses, just parse the expression
            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            condition_index = parse_expression(remaining_tokens, arena)

            ! Advance parser past the condition tokens until 'then'
            do while (.not. parser%is_at_end())
                paren_token = parser%peek()
               if (paren_token%kind == TK_KEYWORD .and. paren_token%text == "then") then
                    exit  ! Don't consume 'then', let caller handle it
                end if
                paren_token = parser%consume()
            end do
        end if

    end function parse_if_condition

    ! Parse if/elseif/else body statements
    function parse_if_body(parser, arena, parent_index) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        type(token_t), allocatable :: remaining_tokens(:)
        integer :: stmt_count, i, n
        integer :: stmt_index


        allocate (body_indices(0))
        stmt_count = 0

        ! Use parse_basic_statement instead of parse_statement

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of body
            if (token%kind == TK_KEYWORD) then
                if (token%text == "elseif" .or. token%text == "endif" .or. token%text == "end if") then
                    exit
                else if (token%text == "else") then
                    ! Check if next token is "if" (for "else if")
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "if") then
                            exit  ! Found "else if"
                        end if
                    end if
                    ! If not "else if", it's just "else" - also exit
                    exit
                else if (token%text == "end") then
                    ! Check if next token is "if"
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "if") then
                            exit  ! Found "end if"
                        end if
                    end if
                end if
            end if

            ! Parse statement until end of line (same approach as do loop)
            block
                integer :: stmt_start, stmt_end, j
                type(token_t), allocatable :: stmt_tokens(:)

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
                        stmt_indices = parse_basic_statement_multi(stmt_tokens, arena, parent_index)

                        ! Add all parsed statements to body
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                                stmt_count = stmt_count + 1
                            end if
                        end do
                    end block

                    if (allocated(stmt_tokens)) deallocate (stmt_tokens)
                end if

                ! Move to next statement
                parser%current_token = stmt_end + 1
            end block
        end do

    end function parse_if_body

    ! Parse elseif block
    function parse_elseif_block(parser, arena) result(elseif_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: elseif_indices(2) ! condition_index, body_indices_start
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()
        ! Check if we consumed 'else' and need to consume 'if' as well
        if (elseif_token%text == "else") then
            ! This should be "else if", consume the "if" token too
            elseif_token = parser%consume()
        end if

        ! Parse condition
        elseif_indices(1) = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. elseif_token%text == "then") then
            elseif_token = parser%consume()
        end if

        ! Parse body
        block
            integer, allocatable :: body_indices(:)
            body_indices = parse_if_body(parser, arena)
            if (allocated(body_indices) .and. size(body_indices) > 0) then
                elseif_indices(2) = body_indices(1)  ! Store first body statement index
            else
                elseif_indices(2) = 0
            end if
        end block

    end function parse_elseif_block

    ! Additional control flow parsing functions

    recursive function parse_do_loop(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token, var_token, eq_token, comma_token
        character(len=:), allocatable :: var_name
        integer :: start_index, end_index, step_index
        integer :: line, column

        step_index = 0  ! Initialize to 0 (no step)

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

        ! Parse start expression (simplified - just parse next token as literal)
        start_index = parse_primary(parser, arena)

        ! Expect ','
        comma_token = parser%consume()
        if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ",") then
            ! Error: expected ','
            return
        end if

        ! Parse end expression
        end_index = parse_primary(parser, arena)

        ! Check for optional step
        if (.not. parser%is_at_end()) then
            comma_token = parser%peek()
            if (comma_token%kind == TK_OPERATOR .and. comma_token%text == ",") then
                comma_token = parser%consume()  ! consume comma
                step_index = parse_primary(parser, arena)
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

                ! Check if this is a control structure that needs special handling
                block
                    type(token_t) :: current_token
                    integer :: stmt_index
                    integer :: old_pos
                    
                    current_token = parser%peek()
                    old_pos = parser%current_token
                    stmt_index = 0
                    
                    ! Handle control structures as complete units
                    if (current_token%kind == TK_KEYWORD) then
                        if (current_token%text == "if") then
                            ! Parse complete if statement
                            stmt_index = parse_if(parser, arena)
                            if (stmt_index > 0) then
                                ! Set parent relationship
                                arena%entries(stmt_index)%parent_index = loop_index
                                call arena%add_child(loop_index, stmt_index)
                                body_indices = [body_indices, stmt_index]
                                body_count = body_count + 1
                            end if
                        else if (current_token%text == "do") then
                            ! Parse nested do loop
                            stmt_index = parse_do_loop(parser, arena)
                            if (stmt_index > 0) then
                                ! Set parent relationship
                                arena%entries(stmt_index)%parent_index = loop_index
                                call arena%add_child(loop_index, stmt_index)
                                body_indices = [body_indices, stmt_index]
                                body_count = body_count + 1
                            end if
                        else if (current_token%text == "select") then
                            ! Parse select case
                            stmt_index = parse_select_case(parser, arena)
                            if (stmt_index > 0) then
                                ! Set parent relationship
                                arena%entries(stmt_index)%parent_index = loop_index
                                call arena%add_child(loop_index, stmt_index)
                                body_indices = [body_indices, stmt_index]
                                body_count = body_count + 1
                            end if
                        else
                            ! Parse other statements line by line
                            call parse_simple_statement_in_loop(parser, arena, loop_index, &
                                                                 body_indices, body_count)
                        end if
                    else
                        ! Parse non-keyword statements line by line
                        call parse_simple_statement_in_loop(parser, arena, loop_index, &
                                                             body_indices, body_count)
                    end if
                    
                    ! If parser position didn't advance, force advance to prevent infinite loop
                    if (parser%current_token == old_pos) then
                        parser%current_token = parser%current_token + 1
                    end if
                end block
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
        
        block
            integer :: default_case_index
            default_case_index = 0

            do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()
            
            ! Safety check for EOF
            if (case_token%kind == TK_EOF) then
                exit
            end if

            if (case_token%kind == TK_KEYWORD) then
                if (case_token%text == "case") then
                    ! Parse case block
                    block
                        type(token_t) :: value_token
                        integer :: case_block_index
                        integer, allocatable :: value_indices(:)
                        integer, allocatable :: body_indices(:)

                        case_token = parser%consume()  ! consume 'case'

                        ! Check for default case
                        value_token = parser%peek()
            if (value_token%kind == TK_KEYWORD .and. value_token%text == "default") then
                            ! Handle default case
                            value_token = parser%consume()  ! consume 'default'
                            
                            ! Parse case body statements
                            block
                                character(len=16) :: end_keywords(2)
                                
                                ! Define end keywords for case blocks
                                end_keywords(1) = "case"
                                end_keywords(2) = "end"
                                
                                ! Use the unified statement body parser
                                body_indices = parse_statement_body(parser, arena, end_keywords)
                            end block
                            
                            ! Create default case node
                            default_case_index = push_case_default(arena, body_indices, &
                                                                   line=case_token%line, &
                                                                   column=case_token%column)
                        else
                            ! Handle regular case
                            
                            ! Expect '('
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == "(") then
                                value_token = parser%consume()  ! consume '('

                                ! Parse case value
                                block
                                    integer :: value_index
                                    value_index = parse_primary(parser, arena)
                                    if (value_index > 0) then
                                        allocate(value_indices(1))
                                        value_indices(1) = value_index
                                    else
                                        allocate(value_indices(0))
                                    end if
                                end block

                                ! Expect ')'
                                value_token = parser%peek()
                 if (value_token%kind == TK_OPERATOR .and. value_token%text == ")") then
                                    value_token = parser%consume()  ! consume ')'
                                end if
                            else
                                ! Error: expected '(' after case
                                allocate(value_indices(0))
                            end if
                            
                            ! Parse case body statements
                            block
                                character(len=16) :: end_keywords(2)
                                
                                ! Define end keywords for case blocks
                                end_keywords(1) = "case"
                                end_keywords(2) = "end"
                                
                                ! Use the unified statement body parser
                                body_indices = parse_statement_body(parser, arena, end_keywords)
                            end block
                            
                            ! Create case block node
                            if (allocated(value_indices) .and. size(value_indices) > 0) then
                                case_block_index = push_case_block(arena, value_indices, &
                                                                   body_indices, &
                                                                   line=case_token%line, &
                                                                   column=case_token%column)
                                ! Add to case indices array
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
                        else
                            ! Not 'end select', skip this 'end' token
                            parser%current_token = parser%current_token + 1
                        end if
                    else
                        ! No token after 'end', skip
                        parser%current_token = parser%current_token + 1
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

            ! Create select case node with case indices
            if (default_case_index > 0) then
                select_index = push_select_case_with_default(arena, expr_index, case_indices, &
                                                            default_case_index, &
                                                            line=line, column=column)
            else
                select_index = push_select_case(arena, expr_index, case_indices, &
                                                line=line, column=column)
            end if
        end block

        if (allocated(case_indices)) deallocate (case_indices)
    end function parse_select_case

    ! Parse basic statement with support for multi-variable declarations
    function parse_basic_statement_multi(tokens, arena, parent_index) result(stmt_indices)
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
                    integer :: lookahead_pos
                    type(token_t) :: lookahead_token
                    
                    ! Look ahead to check for = (initializer) and comma (multi-var)
                    has_initializer = .false.
                    has_comma = .false.
                    lookahead_pos = parser%current_token
                    
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

        ! Handle different statement types
        if (first_token%kind == TK_KEYWORD) then
            select case (first_token%text)
            case ("if")
                ! Parse if statement - need to handle parent index
                if (present(parent_index)) then
                    ! For nested if statements, we need special handling
                    block
                        integer :: nested_if_index
                        nested_if_index = parse_if(parser, arena)
                        if (nested_if_index > 0 .and. present(parent_index)) then
                            ! Update the nested if's parent_index
                            arena%entries(nested_if_index)%parent_index = parent_index
                            ! Add it as a child to the parent
                            call arena%add_child(parent_index, nested_if_index)
                            stmt_index = nested_if_index
                        else
                            stmt_index = 0
                        end if
                    end block
                else
                    stmt_index = parse_if(parser, arena)
                    if (stmt_index <= 0) stmt_index = 0
                end if
            case ("do")
                ! Parse do loop statement - need to handle parent index  
                if (present(parent_index)) then
                    block
                        integer :: nested_loop_index
                        nested_loop_index = parse_do_loop(parser, arena)
                        if (nested_loop_index > 0 .and. present(parent_index)) then
                            ! Update the nested loop's parent_index
                            arena%entries(nested_loop_index)%parent_index = parent_index
                            ! Add it as a child to the parent
                            call arena%add_child(parent_index, nested_loop_index)
                            stmt_index = nested_loop_index
                        else
                            stmt_index = 0
                        end if
                    end block
                else
                    stmt_index = parse_do_loop(parser, arena)
                    if (stmt_index <= 0) stmt_index = 0
                end if
            case ("select")
                ! Parse select case statement - need to handle parent index
                if (present(parent_index)) then
                    block
                        integer :: nested_select_index
                        nested_select_index = parse_select_case(parser, arena)
                        if (nested_select_index > 0 .and. present(parent_index)) then
                            ! Update the nested select's parent_index
                            arena%entries(nested_select_index)%parent_index = parent_index
                            ! Add it as a child to the parent
                            call arena%add_child(parent_index, nested_select_index)
                            stmt_index = nested_select_index
                        else
                            stmt_index = 0
                        end if
                    end block
                else
                    stmt_index = parse_select_case(parser, arena)
                    if (stmt_index <= 0) stmt_index = 0
                end if
            case ("print")
                ! Parse print statement
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                ! Parse write statement
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                ! Parse read statement
                stmt_index = parse_read_statement(parser, arena)
            case ("cycle")
                ! Parse cycle statement
                stmt_index = parse_cycle_statement(parser, arena)
            case ("exit")
                ! Parse exit statement
                stmt_index = parse_exit_statement(parser, arena)
            case ("return")
                ! Parse return statement
                stmt_index = parse_return_statement(parser, arena, parent_index)
            case ("call")
                ! Parse call statement
                stmt_index = parse_call_statement(parser, arena)
            case ("stop")
                ! Parse stop statement
                stmt_index = parse_stop_statement(parser, arena)
            case ("go")
                ! Parse goto statement
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                ! Parse error stop statement
                stmt_index = parse_error_stop_statement(parser, arena)
            case default
                ! Unknown keyword
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

    end function parse_basic_statement_multi

    ! Unified function for parsing statement bodies (used by if blocks, &
    ! do while loops, etc.)
    function parse_statement_body(parser, arena, end_keywords) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: end_keywords(:)
        integer, allocatable :: body_indices(:)

        type(token_t) :: token
        integer :: stmt_count, stmt_index
        integer :: stmt_start, stmt_end, j
        type(token_t), allocatable :: stmt_tokens(:)
        logical :: found_end

        allocate (body_indices(0))
        stmt_count = 0

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end keywords
            found_end = .false.
            if (token%kind == TK_KEYWORD) then
                do j = 1, size(end_keywords)
                    if (token%text == trim(end_keywords(j))) then
                        found_end = .true.
                        exit
                    end if
                end do
                if (found_end) exit
            end if

            ! Parse statement until end of line
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

            ! Extract and parse statement tokens
            if (stmt_end >= stmt_start) then
                allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                stmt_tokens(stmt_end - stmt_start + 2)%text = ""
              stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
      stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

                ! Parse the statement (may return multiple indices for multi-variable declarations)
                block
                    integer, allocatable :: stmt_indices(:)
                    integer :: k
                    stmt_indices = parse_basic_statement_multi(stmt_tokens, arena)

                    ! Add all parsed statements to body
                    do k = 1, size(stmt_indices)
                        if (stmt_indices(k) > 0) then
                            body_indices = [body_indices, stmt_indices(k)]
                            stmt_count = stmt_count + 1
                        end if
                    end do
                end block

                deallocate (stmt_tokens)
            end if

            parser%current_token = stmt_end + 1
        end do
    end function parse_statement_body

    ! Helper function for parsing do while from do token
    function parse_do_while_from_do(parser, arena, line, column) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line, column
        integer :: loop_index

        type(token_t) :: while_token, lparen_token, rparen_token
        integer :: condition_index
        integer, allocatable :: body_indices(:)
        character(len=16) :: end_keywords(1)

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
                        stmt_indices = parse_basic_statement_multi(stmt_tokens, arena)

                        ! Add all parsed statements to body
                        do n = 1, size(stmt_indices)
                            if (stmt_indices(n) > 0) then
                                temp_body_indices = [temp_body_indices, stmt_indices(n)]
                                stmt_count = stmt_count + 1
                            end if
                        end do
                    end block

                    if (allocated(stmt_tokens)) deallocate (stmt_tokens)
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
    
    ! Parse WHERE construct (enhanced version)
    function parse_where_construct(parser, arena) result(where_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: where_index
        
        type(token_t) :: token
        integer :: line, column
        integer :: mask_expr_index
        integer, allocatable :: where_body_indices(:)
        integer, allocatable :: elsewhere_body_indices(:)
        integer :: body_count
        
        ! Consume 'where' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        if (token%kind /= TK_KEYWORD .or. token%text /= "where") then
            where_index = 0
            return
        end if
        token = parser%consume()
        
        ! Check for single-line WHERE by looking for parentheses
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            ! Parse mask expression
            mask_expr_index = parse_if_condition(parser, arena)
            if (mask_expr_index <= 0) then
                where_index = 0
                return
            end if
            
            ! Check if this is single-line WHERE
            token = parser%peek()
            if (token%kind /= TK_KEYWORD .or. token%text == "elsewhere" .or. &
                token%text == "end" .or. parser%is_at_end()) then
                ! Single-line WHERE - parse single statement
                block
                    integer :: stmt_index
                    type(token_t), allocatable :: remaining_tokens(:)
                    integer, allocatable :: stmt_indices(:)
                    integer :: j, n
                    
                    ! Count remaining tokens
                    n = 0
                    do j = parser%current_token, size(parser%tokens)
                        n = n + 1
                    end do
                    
                    ! Extract remaining tokens
                    allocate(remaining_tokens(n))
                    do j = 1, n
                        remaining_tokens(j) = parser%tokens(&
                            parser%current_token + j - 1)
                    end do
                    
                    ! Parse statement
                    stmt_indices = parse_basic_statement_multi(remaining_tokens, arena)
                    
                    if (allocated(stmt_indices) .and. size(stmt_indices) > 0) then
                        allocate(where_body_indices(size(stmt_indices)))
                        where_body_indices = stmt_indices
                        ! Advance parser position
                        parser%current_token = size(parser%tokens) + 1  ! End of tokens
                    else
                        allocate(where_body_indices(0))
                    end if
                end block
                
                ! Create WHERE node with single statement
                where_index = push_where(arena, mask_expr_index, &
                                       where_body_indices=where_body_indices, &
                                       line=line, column=column)
                
                deallocate(where_body_indices)
                return
            end if
        else
            where_index = 0
            return
        end if
        
        ! Multi-line WHERE - parse body statements
        body_count = 0
        allocate(where_body_indices(0))
        
        do
            token = parser%peek()
            if (parser%is_at_end()) exit
            
            ! Check for ELSEWHERE or END WHERE
            if (token%kind == TK_KEYWORD) then
                if (token%text == "elsewhere") then
                    ! Parse ELSEWHERE block
                    token = parser%consume()  ! Consume 'elsewhere'
                    
                    body_count = 0
                    allocate(elsewhere_body_indices(0))
                    
                    do
                        token = parser%peek()
                        if (parser%is_at_end()) exit
                        
                        if (token%kind == TK_KEYWORD .and. token%text == "end") then
                            exit
                        end if
                        
                        ! Parse statement in ELSEWHERE block
                        block
                            type(token_t), allocatable :: stmt_tokens(:)
                            integer, allocatable :: stmt_indices(:)
                            integer :: j, n, k
                            
                            ! Extract tokens for current statement
                            n = 0
                            do j = parser%current_token, size(parser%tokens)
                                if (parser%tokens(j)%kind == TK_NEWLINE) then
                                    n = j - parser%current_token + 1
                                    exit
                                end if
                                n = n + 1
                            end do
                            
                            if (n > 0) then
                                allocate(stmt_tokens(n))
                                do j = 1, n
                                    stmt_tokens(j) = parser%tokens(&
                                        parser%current_token + j - 1)
                                end do
                                
                                stmt_indices = &
                                    parse_basic_statement_multi(stmt_tokens, arena)
                                
                                ! Add all parsed statements
                                do k = 1, size(stmt_indices)
                                    if (stmt_indices(k) > 0) then
                                        body_count = body_count + 1
                                        elsewhere_body_indices = &
                                            [elsewhere_body_indices, stmt_indices(k)]
                                    end if
                                end do
                                
                                ! Advance parser position
                                parser%current_token = parser%current_token + n
                            end if
                        end block
                    end do
                    
                    exit
                else if (token%text == "end") then
                    exit
                end if
            end if
            
            ! Parse statement in WHERE block
            block
                type(token_t), allocatable :: stmt_tokens(:)
                integer, allocatable :: stmt_indices(:)
                integer :: j, n, k
                
                ! Extract tokens for current statement
                n = 0
                do j = parser%current_token, size(parser%tokens)
                    if (parser%tokens(j)%kind == TK_NEWLINE) then
                        n = j - parser%current_token + 1
                        exit
                    end if
                    n = n + 1
                end do
                
                if (n > 0) then
                    allocate(stmt_tokens(n))
                    do j = 1, n
                        stmt_tokens(j) = parser%tokens(parser%current_token + j - 1)
                    end do
                    
                    stmt_indices = parse_basic_statement_multi(stmt_tokens, arena)
                    
                    ! Add all parsed statements
                    do k = 1, size(stmt_indices)
                        if (stmt_indices(k) > 0) then
                            body_count = body_count + 1
                            where_body_indices = [where_body_indices, stmt_indices(k)]
                        end if
                    end do
                    
                    ! Advance parser position
                    parser%current_token = parser%current_token + n
                end if
            end block
        end do
        
        ! Consume 'end where'
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "end") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "where") then
                token = parser%consume()
            end if
        end if
        
        ! Create WHERE node
        if (allocated(elsewhere_body_indices)) then
            where_index = push_where(arena, mask_expr_index, &
                                   where_body_indices=where_body_indices, &
                                   elsewhere_body_indices=elsewhere_body_indices, &
                                   line=line, column=column)
            deallocate(elsewhere_body_indices)
        else
            where_index = push_where(arena, mask_expr_index, &
                                   where_body_indices=where_body_indices, &
                                   line=line, column=column)
        end if
        
        if (allocated(where_body_indices)) deallocate(where_body_indices)
    end function parse_where_construct

    ! Parse ASSOCIATE construct
    recursive function parse_associate(parser, arena) result(assoc_index)
        use ast_nodes_control, only: association_t
        use ast_factory, only: push_associate
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assoc_index
        
        type(token_t) :: token
        type(association_t), allocatable :: associations(:)
        integer, allocatable :: body_indices(:)
        integer :: assoc_count, body_count
        integer :: line, column
        
        ! Get position
        token = parser%peek()
        line = token%line
        column = token%column
        
        ! Consume 'associate'
        token = parser%consume()
        
        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            assoc_index = 0
            return
        end if
        token = parser%consume()
        
        ! Parse associations
        assoc_count = 0
        allocate(associations(10))  ! Initial allocation
        
        do while (.not. parser%is_at_end())
            ! Parse association name
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER) exit
            
            block
                character(len=:), allocatable :: assoc_name
                integer :: expr_index
                
                assoc_name = token%text
                token = parser%consume()
                
                ! Expect '=>'
                token = parser%peek()
                if (token%kind /= TK_OPERATOR .or. token%text /= "=>") then
                    deallocate(associations)
                    assoc_index = 0
                    return
                end if
                token = parser%consume()
                
                ! Parse expression
                expr_index = parse_expression(&
                    parser%tokens(parser%current_token:), arena)
                if (expr_index <= 0) then
                    deallocate(associations)
                    deallocate(body_indices)
                    assoc_index = 0
                    return
                end if
                
                ! Advance parser position - for simple expressions like &
                ! "a + b", consume 3 tokens
                ! This is a simplified approach that works for basic expressions
                block
                    integer :: tokens_to_consume
                    integer :: depth, j
                    type(token_t) :: current_token
                    
                    ! Count tokens in the expression manually
                    tokens_to_consume = 0
                    depth = 0
                    do j = parser%current_token, size(parser%tokens)
                        current_token = parser%tokens(j)
                        if (current_token%kind == TK_EOF) exit
                        
                        ! Track parentheses depth
                        if (current_token%kind == TK_OPERATOR) then
                            if (current_token%text == "(") then
                                depth = depth + 1
                            else if (current_token%text == ")") then
                                if (depth == 0) exit  ! Found closing paren &
                                                       ! of association
                                depth = depth - 1
                            else if (current_token%text == "," .and. depth == 0) then
                                exit  ! Found comma at same level
                            end if
                        end if
                        
                        tokens_to_consume = tokens_to_consume + 1
                    end do
                    
                    if (tokens_to_consume == 0) tokens_to_consume = 1
                    parser%current_token = parser%current_token + tokens_to_consume
                end block
                
                ! Add association
                assoc_count = assoc_count + 1
                if (assoc_count > size(associations)) then
                    ! Resize array
                    block
                        type(association_t), allocatable :: temp(:)
                        allocate(temp(size(associations) * 2))
                        temp(1:size(associations)) = associations
                        call move_alloc(temp, associations)
                    end block
                end if
                
                associations(assoc_count)%name = assoc_name
                associations(assoc_count)%expr_index = expr_index
            end block
            
            ! Check for comma or closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                cycle
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                deallocate(associations)
                assoc_index = 0
                return
            end if
        end do
        
        ! Parse body statements until 'end associate'
        body_count = 0
        allocate(body_indices(100))  ! Initial allocation
        
        do while (.not. parser%is_at_end())
            ! Track position to prevent infinite loops
            block
                integer :: old_pos
                old_pos = parser%current_token
                
                token = parser%peek()
                
                ! Check for 'end associate'
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == &
                            TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == &
                            "associate") then
                            token = parser%consume()  ! consume 'end'
                            token = parser%consume()  ! consume 'associate'
                            exit
                        end if
                    end if
                end if
                
                ! Handle EOF
                if (token%kind == TK_EOF) then
                    exit
                end if
            
            ! Parse a single statement by trying to identify its end
            block
                integer :: stmt_start, stmt_end, j
                type(token_t), allocatable :: stmt_tokens(:)
                integer, allocatable :: stmt_indices(:)
                
                stmt_start = parser%current_token
                stmt_end = stmt_start
                
                ! Find end of current statement 
                ! Since we don't have newlines, we need to use heuristics:
                ! 1. Look for certain keywords that indicate a new statement
                ! 2. Look for assignment patterns
                ! 3. Use a reasonable token limit for simple statements
                
                do j = stmt_start, size(parser%tokens)
                    if (j > stmt_start) then
                        ! Check if we've hit an end condition
                        if (parser%tokens(j)%kind == TK_EOF) then
                            stmt_end = j - 1
                            exit
                        end if
                        
                        ! Check for keywords that typically start new statements
                        if (parser%tokens(j)%kind == TK_KEYWORD) then
                            if (parser%tokens(j)%text == "print" .or. &
                                parser%tokens(j)%text == "write" .or. &
                                parser%tokens(j)%text == "read" .or. &
                                parser%tokens(j)%text == "call" .or. &
                                parser%tokens(j)%text == "if" .or. &
                                parser%tokens(j)%text == "do" .or. &
                                parser%tokens(j)%text == "associate" .or. &
                                parser%tokens(j)%text == "end") then
                                stmt_end = j - 1
                                exit
                            end if
                        end if
                        
                        ! Check for identifier followed by = (assignment)
                        if (j + 1 <= size(parser%tokens)) then
                            if (parser%tokens(j)%kind == TK_IDENTIFIER .and. &
                                parser%tokens(j + 1)%kind == TK_OPERATOR .and. &
                                parser%tokens(j + 1)%text == "=" .and. &
                                j > stmt_start + 2) then  ! Ensure it's not &
                                                           ! the first assignment
                                stmt_end = j - 1
                                exit
                            end if
                        end if
                    end if
                    stmt_end = j
                end do
                
                ! Extract statement tokens with bounds validation
                if (stmt_end >= stmt_start .and. stmt_start >= 1 .and. &
                    stmt_end <= size(parser%tokens)) then
                    allocate(stmt_tokens(stmt_end - stmt_start + 1))
                    do j = 1, stmt_end - stmt_start + 1
                        if (stmt_start + j - 1 >= 1 .and. &
                            stmt_start + j - 1 <= size(parser%tokens)) then
                            stmt_tokens(j) = parser%tokens(stmt_start + j - 1)
                        end if
                    end do
                    
                    ! Parse the statement - handle ASSOCIATE constructs specially
                    block
                        integer :: single_stmt_index
                        
                        ! Check if this is an ASSOCIATE construct
                        if (size(stmt_tokens) > 0 .and. &
                            stmt_tokens(1)%kind == TK_KEYWORD .and. &
                            stmt_tokens(1)%text == "associate") then
                            ! Parse as ASSOCIATE construct
                            block
                                type(parser_state_t) :: stmt_parser
                                stmt_parser = create_parser_state(stmt_tokens)
                                single_stmt_index = parse_associate(stmt_parser, arena)
                            end block
                        else
                            ! Parse as basic statement
                            stmt_indices = &
                                parse_basic_statement_multi(stmt_tokens, arena)
                            if (allocated(stmt_indices) .and. &
                                size(stmt_indices) > 0) then
                                single_stmt_index = stmt_indices(1)
                            else
                                single_stmt_index = 0
                            end if
                        end if
                        
                        ! Convert to array format
                        if (single_stmt_index > 0) then
                            if (.not. allocated(stmt_indices)) allocate(stmt_indices(1))
                            stmt_indices(1) = single_stmt_index
                        else
                            if (.not. allocated(stmt_indices)) allocate(stmt_indices(0))
                        end if
                    end block
                    
                    ! Add successful statements to body
                    if (allocated(stmt_indices) .and. size(stmt_indices) > 0) then
                        do j = 1, size(stmt_indices)
                            if (stmt_indices(j) > 0) then
                                body_count = body_count + 1
                                if (body_count > size(body_indices)) then
                                    ! Resize array - extend by 100 elements
                                    block
                                        integer, allocatable :: temp(:)
                                        allocate(temp(size(body_indices) + 100))
                                        temp(1:size(body_indices)) = body_indices
                                        temp(size(body_indices)+1:) = 0
                                        call move_alloc(temp, body_indices)
                                    end block
                                end if
                                body_indices(body_count) = stmt_indices(j)
                            end if
                        end do
                    end if
                    
                    ! Advance parser
                    parser%current_token = stmt_end + 1
                else
                    ! Safety: advance by one token if we can't parse
                    parser%current_token = parser%current_token + 1
                end if
            end block
            
            ! If parser position didn't advance, force advance to prevent infinite loop
            if (parser%current_token == old_pos) then
                parser%current_token = parser%current_token + 1
            end if
        end block
        end do
        
        ! Create ASSOCIATE node
        if (assoc_count > 0) then
            block
                type(association_t), allocatable :: final_assocs(:)
                integer, allocatable :: final_body(:)
                
                allocate(final_assocs(assoc_count))
                final_assocs = associations(1:assoc_count)
                
                if (body_count > 0) then
                    allocate(final_body(body_count))
                    final_body = body_indices(1:body_count)
                    assoc_index = push_associate(arena, final_assocs, &
                                                  final_body, line, column)
                else
                    assoc_index = push_associate(arena, final_assocs, &
                                                  line=line, column=column)
                end if
            end block
        else
            assoc_index = 0
        end if
        
        deallocate(associations)
        deallocate(body_indices)
    end function parse_associate

    ! Helper function to determine how many tokens an expression consumes
    function parse_expression_length(parser, arena) result(length)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: length
        integer :: start_pos, expr_index
        
        start_pos = parser%current_token
        expr_index = parse_range(parser, arena)
        length = parser%current_token - start_pos
        if (length == 0) length = 1  ! At least one token
    end function parse_expression_length

    
    ! Helper subroutine for parsing simple statements in do loop
    subroutine parse_simple_statement_in_loop(parser, arena, loop_index, body_indices, body_count)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: loop_index
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(inout) :: body_count
        
        integer :: stmt_start, stmt_end, j
        type(token_t), allocatable :: stmt_tokens(:)
        integer, allocatable :: stmt_indices(:)
        integer :: k
        
        ! Parse statement until end of line
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
            ! Add EOF token
            stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
            stmt_tokens(stmt_end - stmt_start + 2)%text = ""
            stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
            stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

            ! Parse the statement (may return multiple indices for multi-variable declarations)
            stmt_indices = parse_basic_statement_multi(stmt_tokens, arena, loop_index)

            ! Add all parsed statements to body
            do k = 1, size(stmt_indices)
                if (stmt_indices(k) > 0) then
                    body_indices = [body_indices, stmt_indices(k)]
                    body_count = body_count + 1
                end if
            end do

            deallocate (stmt_tokens)
        end if

        ! Move to next statement
        parser%current_token = stmt_end + 1
    end subroutine parse_simple_statement_in_loop
    
end module parser_control_flow_module
