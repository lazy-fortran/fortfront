module parser_array_constructs_module
    ! Parser module for WHERE and ASSOCIATE constructs
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
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_where, push_associate, push_identifier, push_literal, push_assignment
    implicit none
    private

    public :: parse_where_construct, parse_associate

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
    
    ! Parse WHERE construct (enhanced version)
    function parse_where_construct(parser, arena) result(where_index)
        use parser_if_constructs_module, only: parse_if_condition
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
                    stmt_indices = parse_basic_stmt_local(remaining_tokens, arena)
                    
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
                block
                    where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                     line=line, column=column)
                end block
                
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
                                    parse_basic_stmt_local(stmt_tokens, arena)
                                
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
                    
                    stmt_indices = parse_basic_stmt_local(stmt_tokens, arena)
                    
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
            where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                   elsewhere_body_indices, line=line, column=column)
            deallocate(elsewhere_body_indices)
        else
            where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                   line=line, column=column)
        end if
        
        if (allocated(where_body_indices)) deallocate(where_body_indices)
    end function parse_where_construct

    ! Parse ASSOCIATE construct
    recursive function parse_associate(parser, arena) result(assoc_index)
        use ast_nodes_control, only: association_t
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
                    if (allocated(body_indices)) deallocate(body_indices)
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
                
                ! Extract statement tokens
                if (stmt_end >= stmt_start) then
                    allocate(stmt_tokens(stmt_end - stmt_start + 1))
                    do j = 1, stmt_end - stmt_start + 1
                        stmt_tokens(j) = parser%tokens(stmt_start + j - 1)
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
                                parse_basic_stmt_local(stmt_tokens, arena)
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

end module parser_array_constructs_module