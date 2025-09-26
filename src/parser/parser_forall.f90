module parser_forall_module
    ! Parser module for FORALL constructs
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE, to_lower
    use parser_state_module
    use parser_expressions_module, only: parse_expression_until
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_forall
    use ast_nodes_loops, only: forall_triplet_t
    use parser_statement_core_module, only: parse_basic_statement_core, &
        statement_callbacks_t, null_statement_callbacks, find_statement_end
    use parser_array_constructs_module, only: parse_where_construct, &
        parse_associate
    implicit none
    private

    public :: parse_forall

contains

    function build_forall_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_forall => parse_forall
        callbacks%parse_where => parse_where_construct
        callbacks%parse_associate => parse_associate
    end function build_forall_callbacks

    ! Parse FORALL construct
    function parse_forall(parser, arena) result(forall_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: forall_index
        
        type(token_t) :: token, next_token
        type(forall_triplet_t), allocatable :: triplets(:)
        integer :: mask_index
        integer, allocatable :: body_indices(:)
        integer, allocatable :: stmt_indices(:)
        type(token_t), allocatable, target :: stmt_tokens(:)
        integer :: triplet_count
        integer :: line, column
        integer :: stmt_start, stmt_end, token_count, k
        logical :: is_inline
        type(statement_callbacks_t) :: callbacks
        
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
        
        callbacks = build_forall_callbacks()
        if (allocated(body_indices)) deallocate(body_indices)
        allocate(body_indices(0))

        stmt_start = parser%current_token
        is_inline = .true.
        do while (stmt_start <= size(parser%tokens))
            select case (parser%tokens(stmt_start)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                stmt_start = stmt_start + 1
            case (TK_NEWLINE)
                is_inline = .false.
                stmt_start = stmt_start + 1
                exit
            case default
                exit
            end select
        end do
        parser%current_token = stmt_start
        if (parser%current_token > size(parser%tokens)) then
            is_inline = .false.
        end if

        if (is_inline) then
            if (parser%current_token <= size(parser%tokens)) then
                stmt_end = find_statement_end(parser%tokens, parser%current_token)
                if (stmt_end < parser%current_token) stmt_end = parser%current_token
                token_count = stmt_end - parser%current_token + 1
                if (token_count > 0) then
                    allocate(stmt_tokens(token_count + 1))
                    stmt_tokens(1:token_count) = &
                        parser%tokens(parser%current_token:stmt_end)
                    stmt_tokens(token_count + 1)%kind = TK_EOF
                    stmt_tokens(token_count + 1)%text = ""
                    stmt_tokens(token_count + 1)%line = &
                        parser%tokens(stmt_end)%line
                    stmt_tokens(token_count + 1)%column = &
                        parser%tokens(stmt_end)%column + 1
                    stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                        callbacks=callbacks)
                    if (allocated(stmt_indices)) then
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                            end if
                        end do
                        deallocate(stmt_indices)
                    end if
                    deallocate(stmt_tokens)
                end if
                parser%current_token = stmt_end + 1
            end if
        else
            do while (.not. parser%is_at_end())
                do
                    if (parser%current_token > size(parser%tokens)) exit
                    token = parser%tokens(parser%current_token)
                    select case (token%kind)
                    case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                        parser%current_token = parser%current_token + 1
                    case (TK_OPERATOR)
                        if (token%text == ";") then
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
                if (token%kind == TK_KEYWORD) then
                    if (to_lower(token%text) == "endforall") then
                        token = parser%consume()
                        exit
                    else if (to_lower(token%text) == "end") then
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                            next_token = parser%tokens(parser%current_token + 1)
                            if (next_token%kind == TK_KEYWORD .and. &
                                to_lower(next_token%text) == "forall") then
                                token = parser%consume()
                                token = parser%consume()
                                exit
                            end if
                        end if
                    end if
                end if

                stmt_end = find_statement_end(parser%tokens, parser%current_token)
                if (stmt_end < parser%current_token) then
                    stmt_end = parser%current_token
                end if

                token_count = stmt_end - parser%current_token + 1
                if (token_count <= 0) then
                    parser%current_token = parser%current_token + 1
                    cycle
                end if

                allocate(stmt_tokens(token_count + 1))
                stmt_tokens(1:token_count) = &
                    parser%tokens(parser%current_token:stmt_end)
                stmt_tokens(token_count + 1)%kind = TK_EOF
                stmt_tokens(token_count + 1)%text = ""
                stmt_tokens(token_count + 1)%line = &
                    parser%tokens(stmt_end)%line
                stmt_tokens(token_count + 1)%column = &
                    parser%tokens(stmt_end)%column + 1

                stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                    callbacks=callbacks)
                if (allocated(stmt_indices)) then
                    do k = 1, size(stmt_indices)
                        if (stmt_indices(k) > 0) then
                            body_indices = [body_indices, stmt_indices(k)]
                        end if
                    end do
                    deallocate(stmt_indices)
                end if
                deallocate(stmt_tokens)

                parser%current_token = stmt_end + 1
            end do
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
