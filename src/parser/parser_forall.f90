module parser_forall_module
    ! Parser module for FORALL constructs
    use iso_fortran_env, only: error_unit
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_expression, parse_expression_until, parse_range, parse_postfix_chain
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_forall, push_assignment, push_identifier
    use ast_nodes_loops, only: forall_triplet_t
    implicit none
    private

    public :: parse_forall

contains

    ! Parse FORALL construct
    function parse_forall(parser, arena) result(forall_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: forall_index
        
        type(token_t) :: token
        type(forall_triplet_t), allocatable :: triplets(:)
        integer :: mask_index
        integer, allocatable :: body_indices(:)
        integer :: triplet_count, body_count
        integer :: line, column
        logical :: is_single_statement
        
        ! Initialize
        forall_index = 0
        mask_index = 0
        triplet_count = 0
        allocate(triplets(10))  ! Initial allocation
        allocate(body_indices(0))
        
        ! Get position
        token = parser%peek()
        line = token%line
        column = token%column
        
        ! Consume 'forall'
        if (token%kind /= TK_KEYWORD .or. token%text /= "forall") then
            return
        end if
        token = parser%consume()
        
        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            return
        end if
        token = parser%consume()
        
        ! Parse forall triplets
        do while (.not. parser%is_at_end())
            ! Parse index name
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER) exit
            
            triplet_count = triplet_count + 1
            if (triplet_count > size(triplets)) then
                ! Resize array
                block
                    type(forall_triplet_t), allocatable :: temp(:)
                    allocate(temp(size(triplets) * 2))
                    temp(1:size(triplets)) = triplets
                    call move_alloc(temp, triplets)
                end block
            end if
            
            triplets(triplet_count)%index_name = token%text
            token = parser%consume()
            
            ! Expect '='
            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= "=") then
                deallocate(triplets)
                deallocate(body_indices)
                return
            end if
            token = parser%consume()
            
            ! Parse lower bound expression (stop before colon, comma, or closing paren)
            block
                character(len=1), dimension(3) :: lower_terms
                lower_terms = [":", ",", ")"]
                triplets(triplet_count)%lower_expr_index = parse_expression_until(parser, arena, lower_terms)
            end block

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ":") then
                deallocate(triplets)
                deallocate(body_indices)
                return
            end if
            token = parser%consume()

            ! Parse upper bound expression (stop before stride, comma, or closing paren)
            block
                character(len=1), dimension(3) :: upper_terms
                upper_terms = [":", ",", ")"]
                triplets(triplet_count)%upper_expr_index = parse_expression_until(parser, arena, upper_terms)
            end block

            if (triplets(triplet_count)%upper_expr_index <= 0) then
                deallocate(triplets)
                deallocate(body_indices)
                return
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ":") then
                token = parser%consume()
                block
                    character(len=1), dimension(2) :: stride_terms
                    stride_terms = [",", ")"]
                    triplets(triplet_count)%stride_expr_index = parse_expression_until(parser, arena, stride_terms)
                end block
            else
                triplets(triplet_count)%stride_expr_index = 0
            end if

            ! Check for comma (more triplets) or mask condition
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == TK_OPERATOR .and. &
                            parser%tokens(parser%current_token + 1)%text == "=") then
                            cycle
                        end if
                    end if
                end if

                block
                    character(len=1), dimension(1) :: mask_terms
                    mask_terms = [")"]
                    mask_index = parse_expression_until(parser, arena, mask_terms)
                end block
                exit
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                exit
            end if
        end do
        
        ! Consume closing parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ")") then
            token = parser%consume()
        end if
        
        ! Check if this is single-statement or block FORALL
        token = parser%peek()
        is_single_statement = .true.
        if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) then
            is_single_statement = .false.
        else if (token%kind == TK_KEYWORD) then
            ! Check for nested constructs that would make this a block forall
            if (token%text == "forall" .or. token%text == "where" .or. &
                token%text == "if" .or. token%text == "do") then
                is_single_statement = .false.
            end if
        end if
        
        if (is_single_statement) then
            ! Parse single statement (usually an assignment)
            block
                integer :: stmt_index, target_index, value_index
                type(token_t) :: id_token, next_token

                id_token = parser%peek()
                if (id_token%kind == TK_IDENTIFIER) then
                    id_token = parser%consume()

                    target_index = push_identifier(arena, id_token%text, &
                                                  id_token%line, id_token%column)
                    target_index = parse_postfix_chain(parser, arena, target_index)

                    next_token = parser%peek()
                    if (next_token%kind == TK_OPERATOR .and. next_token%text == "=") then
                        next_token = parser%consume()

                        value_index = parse_range(parser, arena)

                        if (value_index > 0) then
                            stmt_index = push_assignment(arena, target_index, value_index, &
                                                        id_token%line, id_token%column)
                            deallocate(body_indices)
                            allocate(body_indices(1))
                            body_indices(1) = stmt_index
                        end if
                    end if
                end if
            end block
        else
            ! Parse block FORALL body until 'end forall'
            body_count = 0
            deallocate(body_indices)
            allocate(body_indices(100))  ! Initial allocation
            
            do while (.not. parser%is_at_end())
                token = parser%peek()
                
                ! Check for 'end forall'
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "forall") then
                            token = parser%consume()  ! consume 'end'
                            token = parser%consume()  ! consume 'forall'
                            exit
                        end if
                    end if
                end if
                
                ! Parse statement in body
                block
                    integer :: stmt_index
                    type(token_t), allocatable, target :: stmt_tokens(:)
                    integer :: stmt_end, j
                    
                    stmt_end = parser%current_token
                    
                    ! Find end of current statement
                    do j = parser%current_token, size(parser%tokens)
                        if (parser%tokens(j)%kind == TK_NEWLINE) then
                            stmt_end = j
                            exit
                        else if (parser%tokens(j)%kind == TK_EOF) then
                            stmt_end = j - 1
                            exit
                        else if (parser%tokens(j)%kind == TK_KEYWORD) then
                            if (j > parser%current_token .and. &
                                (parser%tokens(j)%text == "forall" .or. &
                                 parser%tokens(j)%text == "where" .or. &
                                 parser%tokens(j)%text == "if" .or. &
                                 parser%tokens(j)%text == "do" .or. &
                                 parser%tokens(j)%text == "end")) then
                                stmt_end = j - 1
                                exit
                            end if
                        end if
                        stmt_end = j
                    end do
                    
                    if (stmt_end >= parser%current_token) then
                        allocate(stmt_tokens(stmt_end - parser%current_token + 1))
                        do j = 1, stmt_end - parser%current_token + 1
                            stmt_tokens(j) = parser%tokens(parser%current_token + j - 1)
                        end do
                        
                        ! Parse the statement (usually an assignment)
                        block
                            type(parser_state_t) :: stmt_parser
                            integer :: target_idx, value_idx

                            stmt_parser = create_parser_state(stmt_tokens)
                            
                            ! Try to parse as assignment
                            if (stmt_parser%current_token <= size(stmt_tokens)) then
                                if (stmt_tokens(1)%kind == TK_IDENTIFIER) then
                                    target_idx = parse_expression(stmt_tokens(1:), arena)
                                    
                                    ! Find the = operator
                                    do j = 1, size(stmt_tokens)
                                        if (stmt_tokens(j)%kind == TK_OPERATOR .and. &
                                            stmt_tokens(j)%text == "=") then
                                            ! Parse value after =
                                            if (j < size(stmt_tokens)) then
                                                value_idx = parse_expression(stmt_tokens(j+1:), arena)
                                                if (target_idx > 0 .and. value_idx > 0) then
                                                    stmt_index = push_assignment(arena, target_idx, value_idx, &
                                                                               stmt_tokens(1)%line, stmt_tokens(1)%column)
                                                end if
                                            end if
                                            exit
                                        end if
                                    end do
                                end if
                            end if
                        end block
                        if (stmt_index > 0) then
                            body_count = body_count + 1
                            if (body_count > size(body_indices)) then
                                ! Resize array
                                block
                                    integer, allocatable :: temp(:)
                                    allocate(temp(size(body_indices) + 100))
                                    temp(1:size(body_indices)) = body_indices
                                    temp(size(body_indices)+1:) = 0
                                    call move_alloc(temp, body_indices)
                                end block
                            end if
                            body_indices(body_count) = stmt_index
                        end if
                        
                        parser%current_token = stmt_end + 1
                    else
                        parser%current_token = parser%current_token + 1
                    end if
                end block
            end do
            
            ! Trim body_indices to actual size
            if (body_count > 0) then
                block
                    integer, allocatable :: final_body(:)
                    allocate(final_body(body_count))
                    final_body = body_indices(1:body_count)
                    call move_alloc(final_body, body_indices)
                end block
            else
                deallocate(body_indices)
                allocate(body_indices(0))
            end if
        end if
        
        ! Create FORALL node
        if (triplet_count > 0) then
            ! For now, only handle single-index FORALL using existing push_forall
            ! Multi-index support requires extending push_forall
            if (triplet_count == 1) then
                forall_index = push_forall(arena, triplets(1)%index_name, &
                                          triplets(1)%lower_expr_index, &
                                          triplets(1)%upper_expr_index, &
                                          triplets(1)%stride_expr_index, &
                                          mask_index, body_indices, line, column)
            else
                ! For multi-index, create a placeholder for now
                ! TODO: Extend push_forall to support multi-index FORALL
                write(error_unit, '(A)') "WARNING: Multi-index FORALL not yet fully supported"
                forall_index = 0
            end if
        end if
        
        deallocate(triplets)
        if (allocated(body_indices)) deallocate(body_indices)
        
    end function parse_forall
    
end module parser_forall_module
