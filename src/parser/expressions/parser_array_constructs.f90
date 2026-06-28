module parser_array_constructs_module
    ! Parser module for WHERE and ASSOCIATE constructs
    use lexer_core, only: token_t, TK_EOF, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, TK_IDENTIFIER
    use string_utils_mod, only: to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression, &
        parse_expression_until
    use parser_statement_core_module, only: parse_basic_statement_core, &
        statement_callbacks_t, &
        null_statement_callbacks, &
        find_statement_end, &
        allocate_stmt_tokens_with_eof, &
        skip_whitespace_and_semicolons
    use parser_basic_statement_module, only: parse_statement_body
    use parser_if_constructs_module, only: parse_if, parse_if_condition
    use parser_select_constructs_module, only: parse_select_case
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_where, push_associate, push_block_construct
    implicit none
    private

    public :: parse_where_construct, parse_associate, parse_block_construct

contains

    ! Local implementation to avoid circular dependency
    function build_where_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_where => parse_where_construct
        callbacks%parse_associate => parse_associate
        callbacks%parse_block => parse_block_construct
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
        type(statement_callbacks_t) :: callbacks
        logical :: single_line
        character(len=9), parameter :: where_end_keywords(2) = [ &
            'elsewhere', 'end where']

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
            single_line = .true.
            block
                integer :: lookahead

                do lookahead = parser%current_token, size(parser%tokens)
                    if (parser%tokens(lookahead)%kind /= TK_WHITESPACE) then
                        token = parser%tokens(lookahead)
                        exit
                    end if
                end do

                if (lookahead > size(parser%tokens)) then
                    token%kind = TK_EOF
                    token%text = ""
                end if
            end block

            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) then
                single_line = .false.
            else if (token%kind == TK_OPERATOR .and. token%text == ";") then
                single_line = .false.
            else if (token%kind == TK_COMMENT) then
                single_line = .false.
            else if (token%kind == TK_KEYWORD) then
                if (token%text == "elsewhere" .or. token%text == "end") then
                    single_line = .false.
                end if
            end if

            if (single_line) then
                callbacks = build_where_callbacks()
                call parse_single_line_where_body(parser, arena, callbacks, &
                    where_body_indices)

                ! Create WHERE node with single statement
                where_index = push_where(arena, mask_expr_index, where_body_indices, &
                    line=line, column=column)

                return
            end if
        else
            ! Attempt lazy single-line WHERE using colon separator
            callbacks = build_where_callbacks()
            mask_expr_index = parse_expression_until(parser, arena, &
                [character(len=1) :: ":"])
            if (mask_expr_index <= 0) then
                where_index = 0
                return
            end if

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ":") then
                where_index = 0
                return
            end if
            token = parser%consume()

            call parse_single_line_where_body(parser, arena, callbacks, &
                where_body_indices)

            where_index = push_where(arena, mask_expr_index, where_body_indices, &
                line=line, column=column)
            return
        end if

        ! Multi-line WHERE - parse body statements using shared statement body parser
        callbacks = build_where_callbacks()
        where_body_indices = parse_statement_body(parser, arena, &
            where_end_keywords, callbacks)

        ! Parse all ELSEWHERE clauses (including masked ones)
        where_index = create_where_with_elsewhere_clauses(arena, parser, &
            mask_expr_index, &
            where_body_indices, &
            callbacks, &
            where_end_keywords, &
            line, column)
    end function parse_where_construct

    function create_where_with_elsewhere_clauses(arena, parser, &
            mask_expr_index, &
            where_body_indices, &
            callbacks, &
            where_end_keywords, &
            line, column) result(where_index)
        use ast_nodes_control, only: elsewhere_clause_t
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t), intent(inout) :: parser
        integer, intent(in) :: mask_expr_index
        integer, allocatable, intent(in) :: where_body_indices(:)
        type(statement_callbacks_t), intent(in) :: callbacks
        character(len=*), intent(in) :: where_end_keywords(:)
        integer, intent(in) :: line, column
        integer :: where_index

        type(token_t) :: token
        type(elsewhere_clause_t), allocatable :: elsewhere_clauses(:)
        type(elsewhere_clause_t), allocatable :: temp_clauses(:)
        integer :: num_clauses
        integer :: clause_mask_index
        integer, allocatable :: clause_body_indices(:)

        num_clauses = 0
        allocate (elsewhere_clauses(0))

        do while (.true.)
            token = parser%peek()
            if (parser%is_at_end()) exit
            if (token%kind /= TK_KEYWORD) exit
            if (token%text /= "elsewhere") exit

            token = parser%consume()

            clause_mask_index = 0
            token = parser%peek()
            if (.not. parser%is_at_end()) then
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    clause_mask_index = parse_if_condition(parser, arena)
                    if (clause_mask_index <= 0) then
                        where_index = 0
                        return
                    end if
                end if
            end if

            clause_body_indices = parse_statement_body(parser, arena, &
                where_end_keywords, &
                callbacks)

            num_clauses = num_clauses + 1
            allocate (temp_clauses(num_clauses))
            if (num_clauses > 1) then
                temp_clauses(1:num_clauses - 1) = elsewhere_clauses(1:num_clauses - 1)
            end if
            temp_clauses(num_clauses)%mask_index = clause_mask_index
            if (allocated(clause_body_indices)) then
                allocate (temp_clauses(num_clauses)%body_indices( &
                    size(clause_body_indices)))
                temp_clauses(num_clauses)%body_indices = clause_body_indices
            end if
            call move_alloc(temp_clauses, elsewhere_clauses)
        end do

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "end") then
            token = parser%consume()
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "where") then
                    token = parser%consume()
                end if
            end if
        end if

        if (num_clauses > 0) then
            if (allocated(where_body_indices)) then
                where_index = push_where(arena, mask_expr_index, &
                    where_body_indices=where_body_indices, &
                    elsewhere_clauses=elsewhere_clauses, &
                    line=line, column=column)
            else
                where_index = push_where(arena, mask_expr_index, &
                    elsewhere_clauses=elsewhere_clauses, &
                    line=line, column=column)
            end if
        else
            if (allocated(where_body_indices)) then
                where_index = push_where(arena, mask_expr_index, &
                    where_body_indices=where_body_indices, &
                    line=line, column=column)
            else
                where_index = push_where(arena, mask_expr_index, &
                    line=line, column=column)
            end if
        end if
    end function create_where_with_elsewhere_clauses

    subroutine parse_single_line_where_body(parser, arena, callbacks, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        integer, allocatable, intent(out) :: body_indices(:)

        integer :: stmt_start, stmt_end
        type(token_t), allocatable :: stmt_tokens(:)
        integer, allocatable :: stmt_indices(:)

        call skip_whitespace_and_semicolons(parser)
        if (parser%current_token > size(parser%tokens)) then
            allocate (body_indices(0))
            return
        end if

        stmt_start = parser%current_token
        stmt_end = find_statement_end(parser%tokens, stmt_start)
        if (stmt_end < stmt_start) then
            allocate (body_indices(0))
            call skip_whitespace_and_semicolons(parser)
            return
        end if

        call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
            stmt_start, stmt_end)

        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
            callbacks=callbacks)

        if (allocated(stmt_indices)) then
            call move_alloc(stmt_indices, body_indices)
        else
            allocate (body_indices(0))
        end if

        if (allocated(stmt_tokens)) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(stmt_tokens, temp)
            end block
        end if

        parser%current_token = stmt_end + 1
        call skip_whitespace_and_semicolons(parser)
    end subroutine parse_single_line_where_body

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
        allocate (associations(10)) ! Initial allocation

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
                        assoc_index = 0
                        return
                    end if
                    block
                        type(token_t) :: next_token
                        next_token = parser%tokens(parser%current_token + 1)
                        if (next_token%kind /= TK_OPERATOR .or. &
                            next_token%text /= ">") then
                            assoc_index = 0
                            return
                        end if
                    end block
                    token = parser%consume()
                    token = parser%consume()
                else
                    assoc_index = 0
                    return
                end if

                ! Parse expression
                expr_index = parse_expression( &
                    parser%tokens(parser%current_token:), arena)
                if (expr_index <= 0) then
                    if (allocated(body_indices)) then
                        block
                            integer, allocatable :: temp(:)
                            call move_alloc(body_indices, temp)
                        end block
                    end if
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
                                if (depth == 0) exit ! Found closing paren &
                                ! of association
                                depth = depth - 1
                            else if (current_token%text == "," .and. depth == 0) then
                                exit ! Found comma at same level
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
                        allocate (temp(size(associations) * 2))
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
                assoc_index = 0
                return
            end if
        end do

        ! Parse body statements until 'end associate'
        body_count = 0
        allocate (body_indices(100)) ! Initial allocation

        do while (.not. parser%is_at_end())
            call skip_whitespace_and_semicolons(parser)
            if (parser%current_token > size(parser%tokens)) exit

            token = parser%peek()

            ! Check for 'end associate'
            if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. to_lower(parser%tokens(parser%current_token + &
                        1)%text) == &
                        "associate") then
                        token = parser%consume() ! consume 'end'
                        token = parser%consume() ! consume 'associate'
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

                call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
                    parser%current_token, stmt_end)
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
                                    allocate (temp(size(body_indices) + 100))
                                    temp(1:size(body_indices)) = body_indices
                                    temp(size(body_indices) + 1:) = 0
                                    call move_alloc(temp, body_indices)
                                end block
                            end if
                            body_indices(body_count) = stmt_indices(k)
                        end if
                    end do
                end if

                parser%current_token = stmt_end + 1

                block
                    type(token_t), allocatable, target :: temp(:)
                    call move_alloc(stmt_tokens, temp)
                end block
            end block
        end do

        ! Create ASSOCIATE node
        if (assoc_count > 0) then
            block
                type(association_t), allocatable :: final_assocs(:)
                integer, allocatable :: final_body(:)

                allocate (final_assocs(assoc_count))
                final_assocs = associations(1:assoc_count)

                if (body_count > 0) then
                    allocate (final_body(body_count))
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

    end function parse_associate

    function parse_block_construct(parser, arena) result(block_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: block_index

        type(token_t) :: token
        integer :: line, column
        integer, allocatable :: body_indices(:)
        type(statement_callbacks_t) :: callbacks
        character(len=3), parameter :: block_end_keywords(1) = ['end']

        token = parser%peek()
        line = token%line
        column = token%column

        if (token%kind /= TK_KEYWORD .or. to_lower(token%text) /= "block") then
            block_index = 0
            return
        end if
        token = parser%consume()

        callbacks = build_associate_callbacks()
        body_indices = parse_statement_body(parser, arena, &
            block_end_keywords, callbacks)

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "end") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "block") then
                token = parser%consume()
            end if
        end if

        block_index = push_block_construct(arena, body_indices, &
            line=line, column=column)
    end function parse_block_construct

end module parser_array_constructs_module
