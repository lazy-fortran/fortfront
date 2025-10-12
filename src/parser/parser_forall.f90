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
    implicit none
    private

    abstract interface
        function parse_without_parent_interface(parser, arena) result(node_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer :: node_index
        end function parse_without_parent_interface
    end interface

    interface
        subroutine ensure_forall_array_registration_bridge()
        end subroutine ensure_forall_array_registration_bridge
    end interface

    procedure(parse_without_parent_interface), pointer, save :: &
        parse_where_proc => null()
    procedure(parse_without_parent_interface), pointer, save :: &
        parse_associate_proc => null()
    logical, save :: array_callbacks_initialized = .false.

    public :: parse_forall, register_forall_body_parsers

contains

    subroutine register_forall_body_parsers(parse_where, parse_associate)
        procedure(parse_without_parent_interface) :: parse_where
        procedure(parse_without_parent_interface) :: parse_associate

        parse_where_proc => parse_where
        parse_associate_proc => parse_associate
        array_callbacks_initialized = .true.
    end subroutine register_forall_body_parsers

    subroutine ensure_array_callbacks_ready()
        if (.not. array_callbacks_initialized) then
            call ensure_forall_array_registration_bridge()
        end if
    end subroutine ensure_array_callbacks_ready

    integer function parse_where_dispatch(parser, arena) result(where_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        call ensure_array_callbacks_ready()
        where_index = 0
        if (associated(parse_where_proc)) then
            where_index = parse_where_proc(parser, arena)
        end if
    end function parse_where_dispatch

    integer function parse_associate_dispatch(parser, arena) &
        result(associate_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        call ensure_array_callbacks_ready()
        associate_index = 0
        if (associated(parse_associate_proc)) then
            associate_index = parse_associate_proc(parser, arena)
        end if
    end function parse_associate_dispatch

    function build_forall_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_forall => parse_forall
        call ensure_array_callbacks_ready()
        if (associated(parse_where_proc)) then
            callbacks%parse_where => parse_where_dispatch
        end if
        if (associated(parse_associate_proc)) then
            callbacks%parse_associate => parse_associate_dispatch
        end if
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
        character(len=:), allocatable :: index_names(:)
        integer, allocatable :: lower_bounds(:), upper_bounds(:), stride_bounds(:)
        integer :: triplet_count
        integer :: max_name_len
        integer :: line, column
        integer :: stmt_start, stmt_end, token_count, k
        logical :: is_inline
        type(statement_callbacks_t) :: callbacks

        ! Initialize
        forall_index = 0
        mask_index = 0
        triplet_count = 0
        allocate (triplets(10))  ! Initial allocation
        allocate (body_indices(0))

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
                    allocate (temp(size(triplets) * 2))
                    temp(1:size(triplets)) = triplets
                    call move_alloc(temp, triplets)
                end block
            end if

            triplets(triplet_count)%index_name = token%text
            token = parser%consume()

            ! Expect '='
            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= "=") then
                deallocate (triplets)
                deallocate (body_indices)
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
                deallocate (triplets)
                deallocate (body_indices)
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
                deallocate (triplets)
                deallocate (body_indices)
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
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (body_indices(0))

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
                    allocate (stmt_tokens(token_count + 1))
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
                        deallocate (stmt_indices)
                    end if
                    deallocate (stmt_tokens)
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

                allocate (stmt_tokens(token_count + 1))
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
                    deallocate (stmt_indices)
                end if
                deallocate (stmt_tokens)

                parser%current_token = stmt_end + 1
            end do
        end if

        ! Create FORALL node
        if (triplet_count > 0) then
            if (triplet_count == 1) then
                forall_index = push_forall(arena, triplets(1)%index_name, &
                                           triplets(1)%lower_expr_index, &
                                           triplets(1)%upper_expr_index, &
                                           triplets(1)%stride_expr_index, &
                                           mask_index, body_indices, line, column)
            else
                max_name_len = 0
                do k = 1, triplet_count
                    max_name_len = max(max_name_len, len_trim(triplets(k)%index_name))
                end do
                if (max_name_len <= 0) max_name_len = 1

                allocate (character(len=max_name_len) :: index_names(triplet_count))
                allocate (lower_bounds(triplet_count))
                allocate (upper_bounds(triplet_count))
                allocate (stride_bounds(triplet_count))

                do k = 1, triplet_count
                    index_names(k) = triplets(k)%index_name
                    lower_bounds(k) = triplets(k)%lower_expr_index
                    upper_bounds(k) = triplets(k)%upper_expr_index
                    stride_bounds(k) = triplets(k)%stride_expr_index
                end do

                if (allocated(body_indices)) then
                    forall_index = push_forall(arena, triplets(1)%index_name, &
                                               triplets(1)%lower_expr_index, &
                                               triplets(1)%upper_expr_index, &
                                               triplets(1)%stride_expr_index, &
                                               mask_index=mask_index, &
                                               body_indices=body_indices, line=line, column=column, &
                                               index_vars_all=index_names, &
                                               start_indices_all=lower_bounds, &
                                               end_indices_all=upper_bounds, &
                                               stride_indices_all=stride_bounds)
                else
                    forall_index = push_forall(arena, triplets(1)%index_name, &
                                               triplets(1)%lower_expr_index, &
                                               triplets(1)%upper_expr_index, &
                                               triplets(1)%stride_expr_index, &
                                               mask_index=mask_index, line=line, column=column, &
                                               index_vars_all=index_names, &
                                               start_indices_all=lower_bounds, &
                                               end_indices_all=upper_bounds, &
                                               stride_indices_all=stride_bounds)
                end if
            end if
        end if

        deallocate (triplets)
        if (allocated(body_indices)) deallocate (body_indices)
        if (allocated(index_names)) deallocate (index_names)
        if (allocated(lower_bounds)) deallocate (lower_bounds)
        if (allocated(upper_bounds)) deallocate (upper_bounds)
        if (allocated(stride_bounds)) deallocate (stride_bounds)

    end function parse_forall

end module parser_forall_module
