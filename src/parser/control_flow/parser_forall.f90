module parser_forall_module
    ! Parser module for FORALL constructs
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression_until
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_forall
    use ast_nodes_loops, only: forall_triplet_t
    use parser_statement_core_module, only: parse_basic_statement_core, &
        statement_callbacks_t, &
        null_statement_callbacks, &
        find_statement_end
    use parser_utilities, only: consume_token
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

        type(token_t) :: start_token
        type(forall_triplet_t), allocatable :: triplets(:)
        integer :: triplet_count
        integer :: mask_index
        integer, allocatable :: body_indices(:)
        integer :: line, column
        type(statement_callbacks_t) :: callbacks

        forall_index = 0
        mask_index = 0
        triplet_count = 0
        allocate (triplets(0))

        start_token = parser%peek()
        if (.not. is_forall_keyword(start_token)) return
        line = start_token%line
        column = start_token%column
        call consume_token(parser)

        if (.not. parse_forall_header(parser, arena, triplets, triplet_count, &
            mask_index)) then
            return
        end if

        callbacks = build_forall_callbacks()
        call collect_forall_body(parser, arena, callbacks, body_indices)
        forall_index = build_forall_node(arena, triplets, triplet_count, mask_index, &
            body_indices, line, column)
    end function parse_forall

    logical function is_forall_keyword(token) result(is_forall)
        type(token_t), intent(in) :: token
        is_forall = token%kind == TK_KEYWORD .and. token%text == "forall"
    end function is_forall_keyword

    logical function parse_forall_header(parser, arena, triplets, triplet_count, &
            mask_index) &
            result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(forall_triplet_t), allocatable, intent(inout) :: triplets(:)
        integer, intent(inout) :: triplet_count
        integer, intent(inout) :: mask_index
        type(token_t) :: token

        success = .false.
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") return
        call consume_token(parser)

        do
            if (.not. parse_forall_triplet(parser, arena, triplets, &
                triplet_count)) return

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                if (next_triplet_follows(parser)) then
                    call consume_token(parser)
                    cycle
                else
                    call consume_token(parser)
                    mask_index = parse_expression_until(parser, arena, [")"])
                    exit
                end if
            else
                exit
            end if
        end do

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") return
        call consume_token(parser)
        success = triplet_count > 0
    end function parse_forall_header

    logical function parse_forall_triplet(parser, arena, triplets, triplet_count) &
            result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(forall_triplet_t), allocatable, intent(inout) :: triplets(:)
        integer, intent(inout) :: triplet_count
        type(token_t) :: token

        success = .false.
        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) return

        triplet_count = triplet_count + 1
        call ensure_triplet_capacity(triplets, triplet_count)
        triplets(triplet_count)%index_name = token%text
        call consume_token(parser)

        if (.not. expect_operator(parser, "=")) return

        triplets(triplet_count)%lower_expr_index = &
            parse_bound_expression(parser, arena, &
            [":", ",", ")"])
        if (triplets(triplet_count)%lower_expr_index <= 0) return

        if (.not. expect_operator(parser, ":")) return

        triplets(triplet_count)%upper_expr_index = &
            parse_bound_expression(parser, arena, &
            [":", ",", ")"])
        if (triplets(triplet_count)%upper_expr_index <= 0) return

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ":") then
            call consume_token(parser)
            triplets(triplet_count)%stride_expr_index = &
                parse_bound_expression(parser, arena, &
                [",", ")"])
        else
            triplets(triplet_count)%stride_expr_index = 0
        end if

        success = .true.
    end function parse_forall_triplet

    integer function parse_bound_expression(parser, arena, terminators) &
            result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: terminators(:)
        expr_index = parse_expression_until(parser, arena, terminators)
    end function parse_bound_expression

    logical function next_triplet_follows(parser) result(is_triplet)
        type(parser_state_t), intent(inout) :: parser
        integer :: idx

        is_triplet = .false.
        idx = parser%current_token + 1
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case default
                exit
            end select
        end do

        if (idx > size(parser%tokens)) return
        if (parser%tokens(idx)%kind /= TK_IDENTIFIER) return
        if (idx + 1 > size(parser%tokens)) return

        is_triplet = parser%tokens(idx + 1)%kind == TK_OPERATOR .and. &
            parser%tokens(idx + 1)%text == "="
    end function next_triplet_follows

    subroutine ensure_triplet_capacity(triplets, required)
        type(forall_triplet_t), allocatable, intent(inout) :: triplets(:)
        integer, intent(in) :: required
        type(forall_triplet_t), allocatable :: temp(:)

        if (.not. allocated(triplets)) then
            allocate (triplets(max(4, required)))
        else if (required > size(triplets)) then
            allocate (temp(max(required, size(triplets) * 2)))
            temp(1:size(triplets)) = triplets
            call move_alloc(temp, triplets)
        end if
    end subroutine ensure_triplet_capacity

    logical function expect_operator(parser, symbol) result(ok)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: symbol
        type(token_t) :: token

        token = parser%peek()
        ok = token%kind == TK_OPERATOR .and. token%text == symbol
        if (ok) call consume_token(parser)
    end function expect_operator

    subroutine collect_forall_body(parser, arena, callbacks, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(out) :: body_indices(:)
        integer :: stmt_start
        logical :: is_inline

        call determine_body_start(parser, stmt_start, is_inline)
        allocate (body_indices(0))

        if (is_inline) then
            call collect_inline_forall_body(parser, arena, callbacks, stmt_start, &
                body_indices)
        else
            call collect_block_forall_body(parser, arena, callbacks, body_indices)
        end if
    end subroutine collect_forall_body

    subroutine determine_body_start(parser, stmt_start, is_inline)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: stmt_start
        logical, intent(out) :: is_inline
        integer :: idx

        is_inline = .true.
        stmt_start = parser%current_token
        idx = stmt_start

        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case (TK_NEWLINE)
                is_inline = .false.
                idx = idx + 1
                exit
            case default
                exit
            end select
        end do

        parser%current_token = idx
        stmt_start = parser%current_token
        if (parser%current_token > size(parser%tokens)) is_inline = .false.
    end subroutine determine_body_start

    subroutine collect_inline_forall_body(parser, arena, callbacks, stmt_start, &
            body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, intent(in) :: stmt_start
        integer, allocatable, intent(inout) :: body_indices(:)
        integer :: stmt_end

        if (stmt_start > size(parser%tokens)) return

        stmt_end = find_statement_end(parser%tokens, stmt_start)
        if (stmt_end < stmt_start) stmt_end = stmt_start
        call append_statement_range(parser, arena, callbacks, stmt_start, stmt_end, &
            body_indices)
        parser%current_token = stmt_end + 1
    end subroutine collect_inline_forall_body

    subroutine collect_block_forall_body(parser, arena, callbacks, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(inout) :: body_indices(:)
        integer :: stmt_start, stmt_end

        do while (.not. parser%is_at_end())
            call skip_forall_trivia(parser)
            if (parser%current_token > size(parser%tokens)) exit
            if (reached_end_forall(parser)) exit

            stmt_start = parser%current_token
            stmt_end = find_statement_end(parser%tokens, stmt_start)
            if (stmt_end < stmt_start) stmt_end = stmt_start

            call append_statement_range(parser, arena, callbacks, stmt_start, &
                stmt_end, body_indices)
            parser%current_token = stmt_end + 1
        end do
    end subroutine collect_block_forall_body

    subroutine skip_forall_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (parser%current_token <= size(parser%tokens))
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
    end subroutine skip_forall_trivia

    logical function reached_end_forall(parser) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token, next_token

        is_end = .false.
        if (parser%current_token > size(parser%tokens)) return

        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return

        if (to_lower(token%text) == "endforall") then
            call consume_token(parser)
            is_end = .true.
            return
        end if

        if (to_lower(token%text) == "end") then
            if (parser%current_token + 1 <= size(parser%tokens)) then
                next_token = parser%tokens(parser%current_token + 1)
                if (next_token%kind == TK_KEYWORD .and. to_lower(next_token%text) == &
                    "forall") then
                    call consume_token(parser)
                    call consume_token(parser)
                    is_end = .true.
                end if
            end if
        end if
    end function reached_end_forall

    subroutine append_statement_range(parser, arena, callbacks, start_idx, end_idx, &
            body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, intent(in) :: start_idx, end_idx
        integer, allocatable, intent(inout) :: body_indices(:)
        integer :: token_count, k
        type(token_t), allocatable, target :: stmt_tokens(:)
        integer, allocatable :: stmt_indices(:)

        token_count = end_idx - start_idx + 1
        if (token_count <= 0) return

        allocate (stmt_tokens(token_count + 1))
        stmt_tokens(1:token_count) = parser%tokens(start_idx:end_idx)
        stmt_tokens(token_count + 1)%kind = TK_EOF
        stmt_tokens(token_count + 1)%text = ""
        stmt_tokens(token_count + 1)%line = parser%tokens(end_idx)%line
        stmt_tokens(token_count + 1)%column = parser%tokens(end_idx)%column + 1

        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
            callbacks=callbacks)
        if (allocated(stmt_indices)) then
            do k = 1, size(stmt_indices)
                if (stmt_indices(k) > 0) body_indices = [body_indices, stmt_indices(k)]
            end do
            block
                integer, allocatable :: temp(:)
                call move_alloc(stmt_indices, temp)
            end block
        end if
        block
            type(token_t), allocatable, target :: temp(:)
            call move_alloc(stmt_tokens, temp)
        end block
    end subroutine append_statement_range

    integer function build_forall_node(arena, triplets, triplet_count, mask_index, &
            body_indices, line, column) &
            result(forall_index)
        type(ast_arena_t), intent(inout) :: arena
        type(forall_triplet_t), intent(in) :: triplets(:)
        integer, intent(in) :: triplet_count
        integer, intent(in) :: mask_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in) :: line, column
        integer :: k, max_name_len
        character(len=:), allocatable :: index_names(:)
        integer, allocatable :: lower_bounds(:), upper_bounds(:), stride_bounds(:)

        forall_index = 0
        if (triplet_count <= 0) return

        if (triplet_count == 1) then
            forall_index = push_forall(arena, triplets(1)%index_name, &
                triplets(1)%lower_expr_index, &
                triplets(1)%upper_expr_index, &
                triplets(1)%stride_expr_index, mask_index, &
                body_indices, line, column)
            return
        end if

        max_name_len = 1
        do k = 1, triplet_count
            max_name_len = max(max_name_len, len_trim(triplets(k)%index_name))
        end do

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

        forall_index = push_forall(arena, triplets(1)%index_name, &
            triplets(1)%lower_expr_index, &
            triplets(1)%upper_expr_index, &
            triplets(1)%stride_expr_index, &
            mask_index=mask_index, &
            body_indices=body_indices, line=line, &
            column=column, &
            index_vars_all=index_names, &
            start_indices_all=lower_bounds, &
            end_indices_all=upper_bounds, &
            stride_indices_all=stride_bounds)
    end function build_forall_node

end module parser_forall_module
