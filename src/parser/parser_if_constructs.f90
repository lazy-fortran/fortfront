module parser_if_constructs_module
    ! Parser module for IF constructs (if/then/else/elseif/endif)
    use iso_fortran_env, only: error_unit
    use lexer_core
    use ast_types, only: LITERAL_STRING
    use parser_state_module
    use parser_expressions_module, only: parse_expression
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement, &
                                           parse_read_statement
    use parser_control_statements_module, only: parse_cycle_statement, parse_exit_statement, &
                                                parse_return_statement, parse_stop_statement, &
                                                parse_goto_statement, parse_error_stop_statement
    use parser_call_module, only: parse_call_statement
    use parser_do_constructs_module, only: parse_do_loop
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_if, push_identifier, push_literal, push_assignment
    implicit none
    private

    public :: parse_if, parse_if_condition, parse_if_body, parse_elseif_block

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
                        if (.not. allocated(stmt_indices)) allocate (stmt_indices(1))
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
        if (.not. allocated(stmt_indices)) allocate (stmt_indices(1))
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
            case ("call")
                stmt_index = parse_call_statement(parser, arena)
            case ("stop")
                stmt_index = parse_stop_statement(parser, arena)
            case ("go")
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case ("if")
                ! Handle nested if statements
                stmt_index = parse_if(parser, arena, parent_index)
            case ("do")
                ! Handle nested do loops
                stmt_index = parse_do_loop(parser, arena)
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
                        type(token_t), allocatable, target :: expr_tokens(:)
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
                logical :: is_terminator
                character(len=:), allocatable :: lowered
                character(len=256) :: debug_msg
                character(len=64) :: token_text
                integer :: debug_len, i

                is_terminator = .false.
                if (first_token%kind == TK_KEYWORD) then
                    lowered = to_lower(first_token%text)
                    select case (lowered)
                    case ("else", "elseif")
                        is_terminator = .true.
                    case ("end")
                        if (size(tokens) >= 2) then
                            select case (to_lower(tokens(2)%text))
                            case ("if", "do", "select", "where", "forall")
                                is_terminator = .true.
                            end select
                        else
                            is_terminator = .true.
                        end if
                    case ("endif", "enddo", "endselect")
                        is_terminator = .true.
                    end select
                end if

                if (.not. is_terminator) then
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
                end if
            end block
        end if

        stmt_indices(1) = stmt_index
    end function parse_basic_stmt_local

    ! Parse if statement
    function parse_if(parser, arena, parent_index) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
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
            
            ! Skip optional semicolon after 'then'
            if (.not. parser%is_at_end()) then
                then_token = parser%peek()
                if (then_token%kind == TK_OPERATOR .and. then_token%text == ";") then
                    then_token = parser%consume()  ! Skip the semicolon
                end if
            end if

            ! Create if node placeholder first to get the parent index
            if_index = push_if(arena, condition_index, [integer::], &
                               line=if_token%line, column=if_token%column, &
                               parent_index=parent_index)

            ! Parse then body statements with the if node as parent
            then_body_indices = parse_if_body(parser, arena, if_index)

            ! Check for elseif/else blocks
            elseif_count = 0
            allocate (elseif_indices(0))

            block
                integer :: safety_counter
                safety_counter = 0
                do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                    safety_counter = safety_counter + 1
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
                        
                        ! Skip optional semicolon after 'else'
                        if (.not. parser%is_at_end()) then
                            then_token = parser%peek()
                            if (then_token%kind == TK_OPERATOR .and. then_token%text == ";") then
                                then_token = parser%consume()  ! Skip the semicolon
                            end if
                        end if
                        
                        else_body_indices = parse_if_body(parser, arena, if_index)
                        exit
              else if (then_token%text == "endif" .or. then_token%text == "end if") then
                        ! End of if statement
                        then_token = parser%consume()
                        exit
                    else
                        ! Other statement, stop parsing if block
                        exit
                    end if
                else
                    ! Not a keyword, continue parsing body
                    exit
                end if
                end do
            end block

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
                type(token_t), allocatable, target :: remaining_tokens(:)
                integer :: i, n

                ! Count remaining tokens
                n = 0
                do i = parser%current_token, size(parser%tokens)
                    n = n + 1
                end do

                ! Extract remaining tokens
                allocate (remaining_tokens(n))
                remaining_tokens = parser%tokens(parser%current_token:)

                ! Use local basic statement parser
                block
                    integer, allocatable :: stmt_indices(:)
                    stmt_indices = parse_basic_stmt_local(remaining_tokens, arena)
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
                               line=if_token%line, column=if_token%column, &
                               parent_index=parent_index)
        end if

    end function parse_if

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable, target :: remaining_tokens(:)
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
        integer :: stmt_count

        allocate (body_indices(0))
        stmt_count = 0
        
        block
            integer :: safety_counter
            safety_counter = 0
            
            do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                safety_counter = safety_counter + 1
            token = parser%peek()

            ! Check for end of body
            if (token%kind == TK_KEYWORD) then
                if (token%text == "elseif" .or. token%text == "else if" .or. &
                    token%text == "endif" .or. token%text == "end if") then
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
                type(token_t), allocatable, target :: stmt_tokens(:)

                stmt_start = parser%current_token
                
                ! Skip leading semicolons and newlines to get to actual statement
                do while (stmt_start <= size(parser%tokens) .and. &
                         ((parser%tokens(stmt_start)%kind == TK_OPERATOR .and. &
                          parser%tokens(stmt_start)%text == ";") .or. &
                          parser%tokens(stmt_start)%kind == TK_NEWLINE))
                    stmt_start = stmt_start + 1
                end do
                
                ! Check if we've gone past the end of tokens
                if (stmt_start > size(parser%tokens)) then
                    parser%current_token = stmt_start
                    exit  ! Exit the main body parsing loop
                end if

                ! After skipping leading delimiters, bail out if we hit a terminator
                token = parser%tokens(stmt_start)
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("else", "elseif", "else if", "endif", "end if")
                        parser%current_token = stmt_start
                        exit  ! Let caller handle control-flow keyword
                    case ("end")
                        if (stmt_start + 1 <= size(parser%tokens)) then
                            if (parser%tokens(stmt_start + 1)%kind == TK_KEYWORD .and. &
                                parser%tokens(stmt_start + 1)%text == "if") then
                                parser%current_token = stmt_start
                                exit
                            end if
                        else
                            parser%current_token = stmt_start
                            exit
                        end if
                    end select
                end if

                stmt_end = stmt_start

                ! Find end of current statement (handle multi-line properly)
                block
                    logical :: is_nested_if
                    integer :: nesting_depth
                    
                    is_nested_if = .false.
                    nesting_depth = 0
                    
                    ! Check if statement starts with 'if' (for same-line nested ifs only)
                    if (parser%tokens(stmt_start)%kind == TK_KEYWORD .and. &
                        parser%tokens(stmt_start)%text == "if") then
                        ! Check if there are semicolons on the same line (same-line nesting)
                        block
                            integer :: k
                            logical :: has_semicolon
                            has_semicolon = .false.
                            do k = stmt_start + 1, min(stmt_start + 20, size(parser%tokens))
                                if (parser%tokens(k)%line > parser%tokens(stmt_start)%line) exit
                                if (parser%tokens(k)%kind == TK_OPERATOR .and. &
                                    parser%tokens(k)%text == ";") then
                                    has_semicolon = .true.
                                    exit
                                end if
                            end do
                            if (has_semicolon) then
                                is_nested_if = .true.
                                nesting_depth = 1
                            end if
                        end block
                    end if
                    
                    do j = stmt_start, size(parser%tokens)
                        ! For nested if, need to find matching end if
                        if (is_nested_if .and. j > stmt_start) then
                            if (parser%tokens(j)%kind == TK_KEYWORD) then
                                if (parser%tokens(j)%text == "if") then
                                    ! Check if it's a new if (has 'then' later)
                                    block
                                        integer :: k
                                        logical :: has_then
                                        has_then = .false.
                                        do k = j + 1, min(j + 10, size(parser%tokens))
                                            if (parser%tokens(k)%kind == TK_KEYWORD .and. &
                                                parser%tokens(k)%text == "then") then
                                                has_then = .true.
                                                exit
                                            else if (parser%tokens(k)%kind == TK_NEWLINE .or. &
                                                    parser%tokens(k)%kind == TK_EOF) then
                                                exit
                                            end if
                                        end do
                                        if (has_then) then
                                            nesting_depth = nesting_depth + 1
                                        end if
                                    end block
                                else if (parser%tokens(j)%text == "endif" .or. &
                                        parser%tokens(j)%text == "end") then
                                    if (parser%tokens(j)%text == "endif") then
                                        nesting_depth = nesting_depth - 1
                                        if (nesting_depth == 0) then
                                            stmt_end = j
                                            exit
                                        end if
                                    else if (j + 1 <= size(parser%tokens) .and. &
                                            parser%tokens(j+1)%kind == TK_KEYWORD .and. &
                                            parser%tokens(j+1)%text == "if") then
                                        nesting_depth = nesting_depth - 1
                                        if (nesting_depth == 0) then
                                            stmt_end = j + 1
                                            exit
                                        end if
                                    end if
                                end if
                            end if
                        else if (.not. is_nested_if) then
                            ! Non-nested statement - original logic
                            ! Treat control-flow keywords as hard boundaries
                            if (parser%tokens(j)%kind == TK_KEYWORD) then
                                select case (parser%tokens(j)%text)
                                case ("else", "elseif", "else if")
                                    if (j > stmt_start) then
                                        stmt_end = j - 1
                                        exit
                                    end if
                                case ("endif", "end if")
                                    if (j > stmt_start) then
                                        stmt_end = j - 1
                                        exit
                                    end if
                                case ("end")
                                    if (j + 1 <= size(parser%tokens)) then
                                        if (parser%tokens(j+1)%kind == TK_KEYWORD .and. &
                                            parser%tokens(j+1)%text == "if") then
                                            if (j > stmt_start) then
                                                stmt_end = j - 1
                                                exit
                                            end if
                                        end if
                                    end if
                                end select
                            end if
                        end if
                        if (parser%tokens(j)%kind == TK_EOF) then
                            stmt_end = j
                            exit
                        end if
                        ! For non-nested statements, check for newlines and semicolons
                        if (.not. is_nested_if) then
                            ! For multi-line control flow bodies, treat NEWLINE as a statement boundary
                            ! but only after we've seen some content
                            if (j > stmt_start .and. parser%tokens(j)%kind == TK_NEWLINE) then
                                stmt_end = j - 1
                                exit
                            end if
                            ! Check for semicolon as statement separator
                            if (j > stmt_start .and. parser%tokens(j)%kind == TK_OPERATOR .and. &
                                parser%tokens(j)%text == ";") then
                                stmt_end = j - 1
                                exit
                            end if
                        end if
                        stmt_end = j
                    end do
                end block

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
                        stmt_indices = parse_basic_stmt_local(stmt_tokens, arena, parent_index)

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
            end block
            end do
        end block

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

end module parser_if_constructs_module
