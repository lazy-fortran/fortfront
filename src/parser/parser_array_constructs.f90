module parser_array_constructs_module
    ! Parser module for WHERE and ASSOCIATE constructs
    use lexer_core, only: token_t, TK_EOF, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, TK_IDENTIFIER
    use parser_state_module
    use parser_expressions_module, only: parse_expression
    use parser_statement_core_module, only: parse_basic_statement_core, &
        statement_callbacks_t, null_statement_callbacks, find_statement_end
    use parser_if_constructs_module, only: parse_if, parse_if_condition
    use parser_select_constructs_module, only: parse_select_case
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_where, push_associate
    implicit none
    private

    public :: parse_where_construct, parse_associate

contains

    ! Local implementation to avoid circular dependency
    function build_where_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_where => parse_where_construct
        callbacks%parse_associate => parse_associate
        callbacks%parse_select_case => parse_select_case
    end function build_where_callbacks

    function build_associate_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks
        callbacks = build_where_callbacks()
    end function build_associate_callbacks
    
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
            block
                integer :: lookahead

                lookahead = parser%current_token
                do while (token%kind == TK_WHITESPACE .and. &
                          lookahead <= size(parser%tokens))
                    lookahead = lookahead + 1
                    if (lookahead > size(parser%tokens)) then
                        token%kind = TK_EOF
                        token%text = ""
                        exit
                    end if
                    token = parser%tokens(lookahead)
                end do
            end block

            if (token%kind == TK_NEWLINE .or. token%kind == TK_COMMENT) then
                ! Multi-line WHERE: body begins on following line
            else if (token%kind /= TK_KEYWORD .or. token%text == "elsewhere" .or. &
                     token%text == "end" .or. parser%is_at_end()) then
                ! Single-line WHERE - parse single statement
                block
                    type(token_t), allocatable, target :: remaining_tokens(:)
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
                        remaining_tokens(j) = &
                            parser%tokens(parser%current_token + j - 1)
                    end do
                    
                    ! Parse statement
                    block
                        type(statement_callbacks_t) :: callbacks
                        callbacks = build_where_callbacks()
                        stmt_indices = parse_basic_statement_core( &
                            remaining_tokens, arena, callbacks=callbacks)
                    end block
                    
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
                    where_index = push_where(arena, mask_expr_index, &
                                     where_body_indices, line=line, column=column)
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

                        if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE .or. &
                            token%kind == TK_COMMENT) then
                            token = parser%consume()
                            cycle
                        end if

                        ! Parse statement in ELSEWHERE block
                        block
                            type(token_t), allocatable, target :: stmt_tokens(:)
                            integer, allocatable :: stmt_indices(:)
                            integer :: j, n, k
                            integer :: last_token_index
                            
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
                                allocate(stmt_tokens(n + 1))
                                do j = 1, n
                                    stmt_tokens(j) = &
                                        parser%tokens(parser%current_token + j - 1)
                                end do
                                stmt_tokens(n + 1)%kind = TK_EOF
                                stmt_tokens(n + 1)%text = ""
                                last_token_index = parser%current_token + n - 1
                                stmt_tokens(n + 1)%line = &
                                    parser%tokens(last_token_index)%line
                                stmt_tokens(n + 1)%column = &
                                    parser%tokens(last_token_index)%column + 1

                                stmt_indices = parse_basic_statement_core( &
                                    stmt_tokens, arena, callbacks= &
                                    build_where_callbacks())

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
                                deallocate(stmt_tokens)
                            end if
                        end block
                    end do
                    
                    exit
                else if (token%text == "end") then
                    exit
                end if
            end if

            if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE) then
                token = parser%consume()
                cycle
            else if (token%kind == TK_COMMENT) then
                token = parser%consume()
                cycle
            end if

            ! Parse statement in WHERE block
            block
                type(token_t), allocatable, target :: stmt_tokens(:)
                integer, allocatable :: stmt_indices(:)
                integer :: j, n, k
                integer :: last_token_index
                
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
                    allocate(stmt_tokens(n + 1))
                    do j = 1, n
                        stmt_tokens(j) = &
                            parser%tokens(parser%current_token + j - 1)
                    end do
                    stmt_tokens(n + 1)%kind = TK_EOF
                    stmt_tokens(n + 1)%text = ""
                    last_token_index = parser%current_token + n - 1
                    stmt_tokens(n + 1)%line = parser%tokens(last_token_index)%line
                    stmt_tokens(n + 1)%column = &
                        parser%tokens(last_token_index)%column + 1

                    stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                        callbacks=build_where_callbacks())

                    ! Add all parsed statements
                    do k = 1, size(stmt_indices)
                        if (stmt_indices(k) > 0) then
                            body_count = body_count + 1
                            where_body_indices = [where_body_indices, stmt_indices(k)]
                        end if
                    end do

                    ! Advance parser position
                    parser%current_token = parser%current_token + n
                    deallocate(stmt_tokens)
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
                if (token%kind == TK_OPERATOR .and. token%text == "=>") then
                    token = parser%consume()
                else if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    if (parser%current_token + 1 > size(parser%tokens)) then
                        deallocate(associations)
                        assoc_index = 0
                        return
                    end if
                    block
                        type(token_t) :: next_token
                        next_token = parser%tokens(parser%current_token + 1)
                        if (next_token%kind /= TK_OPERATOR .or. &
                            next_token%text /= ">") then
                            deallocate(associations)
                            assoc_index = 0
                            return
                        end if
                    end block
                    token = parser%consume()
                    token = parser%consume()
                else
                    deallocate(associations)
                    assoc_index = 0
                    return
                end if
                
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
            do while (parser%current_token <= size(parser%tokens))
                select case (parser%tokens(parser%current_token)%kind)
                case (TK_NEWLINE, TK_COMMENT, TK_WHITESPACE)
                    parser%current_token = parser%current_token + 1
                case (TK_OPERATOR)
                    if (parser%tokens(parser%current_token)%text == ";") then
                        parser%current_token = parser%current_token + 1
                    else
                        exit
                    end if
                case default
                    exit
                end select
            end do
            if (parser%current_token > size(parser%tokens)) exit

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
            
            block
                type(token_t), allocatable, target :: stmt_tokens(:)
                integer, allocatable :: stmt_indices(:)
                integer :: remaining_count, consumed_tokens, k
                integer :: stmt_end, last_token_index
                type(statement_callbacks_t) :: callbacks

                stmt_end = find_statement_end(parser%tokens, parser%current_token)
                if (stmt_end < parser%current_token) then
                    stmt_end = parser%current_token
                end if

                remaining_count = stmt_end - parser%current_token + 1
                if (remaining_count <= 0) exit

                allocate(stmt_tokens(remaining_count + 1))
                stmt_tokens(1:remaining_count) = &
                    parser%tokens(parser%current_token:stmt_end)
                stmt_tokens(remaining_count + 1)%kind = TK_EOF
                stmt_tokens(remaining_count + 1)%text = ""
                last_token_index = stmt_end
                stmt_tokens(remaining_count + 1)%line = &
                    parser%tokens(last_token_index)%line
                stmt_tokens(remaining_count + 1)%column = &
                    parser%tokens(last_token_index)%column + 1

                callbacks = build_associate_callbacks()
                stmt_indices = parse_basic_statement_core( &
                    stmt_tokens, arena, callbacks=callbacks, &
                    consumed_count=consumed_tokens)

                if (allocated(stmt_indices) .and. size(stmt_indices) > 0) then
                    do k = 1, size(stmt_indices)
                        if (stmt_indices(k) > 0) then
                            body_count = body_count + 1
                            if (body_count > size(body_indices)) then
                                block
                                    integer, allocatable :: temp(:)
                                    allocate(temp(size(body_indices) + 100))
                                    temp(1:size(body_indices)) = body_indices
                                    temp(size(body_indices)+1:) = 0
                                    call move_alloc(temp, body_indices)
                                end block
                            end if
                            body_indices(body_count) = stmt_indices(k)
                        end if
                    end do
                end if

                parser%current_token = stmt_end + 1

                deallocate(stmt_tokens)
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
