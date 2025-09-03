module parser_forall_module
    ! Parser module for FORALL constructs
    use iso_fortran_env, only: error_unit
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_expression
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_forall
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
            
            ! Parse lower bound expression
            triplets(triplet_count)%lower_expr_index = &
                parse_expression(parser%tokens(parser%current_token:), arena)
            
            ! Skip tokens consumed by expression parsing
            call skip_to_operator(parser, ":")
            
            ! Expect ':'
            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ":") then
                deallocate(triplets)
                deallocate(body_indices)
                return
            end if
            token = parser%consume()
            
            ! Parse upper bound expression
            triplets(triplet_count)%upper_expr_index = &
                parse_expression(parser%tokens(parser%current_token:), arena)
            
            ! Check for optional stride
            call skip_to_next_delimiter(parser)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ":") then
                token = parser%consume()
                triplets(triplet_count)%stride_expr_index = &
                    parse_expression(parser%tokens(parser%current_token:), arena)
                call skip_to_next_delimiter(parser)
            else
                triplets(triplet_count)%stride_expr_index = 0
            end if
            
            ! Check for comma (more triplets) or mask condition
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                ! Check if this is another triplet or a mask
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    ! Look ahead to see if it's a triplet (has '=') or mask
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == TK_OPERATOR .and. &
                            parser%tokens(parser%current_token + 1)%text == "=") then
                            cycle  ! It's another triplet
                        else
                            ! It's a mask condition
                            mask_index = parse_expression(parser%tokens(parser%current_token:), arena)
                            call skip_to_operator(parser, ")")
                            exit
                        end if
                    end if
                else
                    ! It's a mask condition (not starting with identifier)
                    mask_index = parse_expression(parser%tokens(parser%current_token:), arena)
                    call skip_to_operator(parser, ")")
                    exit
                end if
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
            ! Parse single statement
            block
                integer :: stmt_index
                type(token_t), allocatable :: remaining_tokens(:)
                integer :: n, j
                
                ! Count remaining tokens until newline or EOF
                n = 0
                do j = parser%current_token, size(parser%tokens)
                    if (parser%tokens(j)%kind == TK_NEWLINE .or. &
                        parser%tokens(j)%kind == TK_EOF) exit
                    n = n + 1
                end do
                
                if (n > 0) then
                    allocate(remaining_tokens(n))
                    do j = 1, n
                        remaining_tokens(j) = parser%tokens(parser%current_token + j - 1)
                    end do
                    
                    stmt_index = parse_expression(remaining_tokens, arena)
                    if (stmt_index > 0) then
                        deallocate(body_indices)
                        allocate(body_indices(1))
                        body_indices(1) = stmt_index
                    end if
                    
                    parser%current_token = parser%current_token + n
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
                    type(token_t), allocatable :: stmt_tokens(:)
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
                        
                        stmt_index = parse_expression(stmt_tokens, arena)
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
    
    ! Helper subroutine to skip tokens until we find a specific operator
    subroutine skip_to_operator(parser, op)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: op
        type(token_t) :: token
        integer :: depth
        
        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            if (token%kind == TK_OPERATOR) then
                if (token%text == "(") then
                    depth = depth + 1
                else if (token%text == ")") then
                    depth = depth - 1
                else if (token%text == op .and. depth == 0) then
                    return
                end if
            else if (token%kind == TK_EOF) then
                return
            end if
            
            token = parser%consume()
        end do
    end subroutine skip_to_operator
    
    ! Helper subroutine to skip to next delimiter (comma, colon, or paren)
    subroutine skip_to_next_delimiter(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        integer :: depth
        
        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            if (token%kind == TK_OPERATOR) then
                if (token%text == "(") then
                    depth = depth + 1
                    token = parser%consume()
                else if (token%text == ")") then
                    if (depth == 0) return
                    depth = depth - 1
                    token = parser%consume()
                else if ((token%text == "," .or. token%text == ":") .and. depth == 0) then
                    return
                else
                    token = parser%consume()
                end if
            else if (token%kind == TK_EOF) then
                return
            else
                token = parser%consume()
            end if
        end do
    end subroutine skip_to_next_delimiter

end module parser_forall_module