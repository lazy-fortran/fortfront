module parser_do_constructs_module
    ! Parser module for DO constructs (do loops, do while)
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
                          TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module
    use parser_expressions_module, only: parse_logical_or, parse_range
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_factory, only: push_do_loop, push_do_while
    use parser_if_constructs_module, only: parse_if, register_parse_do_loop
    use parser_select_constructs_module, only: parse_select_case
    use parser_array_constructs_module, only: parse_where_construct, parse_associate
    use parser_forall_module, only: parse_forall
    use parser_statement_core_module, only: parse_basic_statement_core, &
                                            statement_callbacks_t, null_statement_callbacks
    implicit none
    private

    logical, save :: if_hooks_initialized = .false.

    public :: parse_do_loop, parse_do_while, parse_do_while_from_do
    public :: ensure_if_do_registration

contains

    subroutine initialize_if_hooks()
        if (.not. if_hooks_initialized) then
            call register_parse_do_loop(parse_do_loop)
            if_hooks_initialized = .true.
        end if
    end subroutine initialize_if_hooks

    subroutine ensure_if_do_registration()
        call initialize_if_hooks()
    end subroutine ensure_if_do_registration

    logical function has_then_before_newline(tokens, start_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: idx

        has_then_before_newline = .false.
        do idx = start_pos + 1, size(tokens)
            select case (tokens(idx)%kind)
            case (TK_KEYWORD)
                if (tokens(idx)%text == "then") then
                    has_then_before_newline = .true.
                    return
                else if (tokens(idx)%text == "endif" .or. tokens(idx)%text == "end") then
                    return
                end if
            case (TK_NEWLINE, TK_EOF)
                return
            end select
        end do
    end function has_then_before_newline

    logical function is_else_if_context(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: idx

        is_else_if_context = .false.
        idx = pos - 1
        do while (idx >= 1)
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx - 1
                cycle
            case (TK_KEYWORD)
                if (tokens(idx)%text == "else") then
                    is_else_if_context = .true.
                end if
                return
            case default
                return
            end select
        end do
    end function is_else_if_context

    integer function find_matching_end_if(tokens, start_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: depth, idx

        find_matching_end_if = -1
        if (.not. has_then_before_newline(tokens, start_pos)) return

        depth = 1
        idx = start_pos + 1
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_KEYWORD)
                select case (tokens(idx)%text)
                case ("if")
                    if (.not. is_else_if_context(tokens, idx)) then
                        if (has_then_before_newline(tokens, idx)) depth = depth + 1
                    end if
                case ("endif")
                    depth = depth - 1
                    if (depth == 0) then
                        find_matching_end_if = idx
                        return
                    end if
                case ("end")
                    if (idx + 1 <= size(tokens)) then
                        if (tokens(idx + 1)%kind == TK_KEYWORD) then
                            if (tokens(idx + 1)%text == "if") then
                                depth = depth - 1
                                if (depth == 0) then
                                    find_matching_end_if = idx + 1
                                    return
                                end if
                            end if
                        end if
                    end if
                case ("elseif")
                    cycle
                case ("else")
                    cycle
                end select
            case (TK_EOF)
                exit
            end select
            idx = idx + 1
        end do
    end function find_matching_end_if

    function build_do_body_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_do_loop => parse_do_loop
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_where => parse_where_construct
        callbacks%parse_forall => parse_forall
        callbacks%parse_associate => parse_associate
    end function build_do_body_callbacks

    function parse_do_loop(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: loop_index

        type(token_t) :: do_token, var_token, eq_token, comma_token
        character(len=:), allocatable :: var_name
        integer :: start_index, end_index, step_index
        integer :: line, column

        call initialize_if_hooks()

        step_index = 0  ! Initialize to 0 (no step)
        loop_index = 0  ! Initialize to 0 (failure) in case of early return

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

        ! Parse start expression (now handles full expressions)
        start_index = parse_range(parser, arena)
        
        if (start_index <= 0) then
            ! Failed to parse start expression
            loop_index = 0
            return
        end if

        ! Expect ','
        comma_token = parser%consume()
        if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ",") then
            ! Error: expected ','
            loop_index = 0
            return
        end if

        ! Parse end expression
        end_index = parse_range(parser, arena)
        
        if (end_index <= 0) then
            ! Failed to parse end expression
            loop_index = 0
            return
        end if

        ! Check for optional step
        if (.not. parser%is_at_end()) then
            comma_token = parser%peek()
            if (comma_token%kind == TK_OPERATOR .and. comma_token%text == ",") then
                comma_token = parser%consume()  ! consume comma
                step_index = parse_range(parser, arena)
            end if
        end if

        ! Parse body statements until 'end do'
        block
            integer, allocatable :: body_indices(:)
            integer :: stmt_start, stmt_end, j
            type(token_t), allocatable, target :: stmt_tokens(:)
            type(statement_callbacks_t) :: callbacks

            allocate (body_indices(0))
            callbacks = build_do_body_callbacks()

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

                ! Parse statement until end of line or semicolon
                stmt_start = parser%current_token
                
                ! Skip leading semicolons to get to actual statement
                do while (stmt_start <= size(parser%tokens) .and. &
                         parser%tokens(stmt_start)%kind == TK_OPERATOR .and. &
                         parser%tokens(stmt_start)%text == ";")
                    stmt_start = stmt_start + 1
                end do

                if (stmt_start > size(parser%tokens)) then
                    parser%current_token = stmt_start
                    exit
                end if

                if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                    if (parser%tokens(stmt_start)%text == "enddo" .or. &
                        parser%tokens(stmt_start)%text == "end do") then
                        parser%current_token = stmt_start
                        exit
                    else if (parser%tokens(stmt_start)%text == "end") then
                        if (stmt_start + 1 <= size(parser%tokens)) then
                            if (parser%tokens(stmt_start + 1)%kind == TK_KEYWORD .and. &
                                parser%tokens(stmt_start + 1)%text == "do") then
                                parser%current_token = stmt_start
                                exit
                            end if
                        else
                            parser%current_token = stmt_start
                            exit
                        end if
                    end if
                end if

                stmt_end = stmt_start

                block
                    integer :: block_end
                    logical :: handled_block

                    handled_block = .false.
                    if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                        if (parser%tokens(stmt_start)%text == "if") then
                            block_end = find_matching_end_if(parser%tokens, stmt_start)
                            if (block_end > stmt_start) then
                                stmt_end = block_end
                                handled_block = .true.
                            end if
                        end if
                    end if

                    if (.not. handled_block) then
                        do j = stmt_start, size(parser%tokens)
                            if (parser%tokens(j)%kind == TK_EOF) then
                                stmt_end = j
                                exit
                            end if
                            if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) then
                                stmt_end = j - 1
                                exit
                            end if
                            if (j > stmt_start .and. parser%tokens(j)%kind == TK_OPERATOR .and. &
                                parser%tokens(j)%text == ";") then
                                stmt_end = j - 1
                                exit
                            end if
                            stmt_end = j
                        end do
                    end if
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
                        logical :: has_meaningful
                        has_meaningful = .false.
                        do k = 1, size(stmt_tokens)
                            select case (stmt_tokens(k)%kind)
                            case (TK_EOF, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE)
                                cycle
                            case default
                                if (len_trim(stmt_tokens(k)%text) > 0) then
                                    has_meaningful = .true.
                                    exit
                                end if
                            end select
                        end do
                        if (.not. has_meaningful) then
                            ! Advance past this blank line before cycling to avoid infinite loop
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
                            deallocate(stmt_tokens)
                            cycle
                        end if
                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                                loop_index, callbacks)

                        ! Add all parsed statements to body
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
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

        call initialize_if_hooks()

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        loop_index = parse_do_while_from_do(parser, arena, line, column)
    end function parse_do_while

    ! Helper function for parsing do while from do token
    function parse_do_while_from_do(parser, arena, line, column) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line, column
        integer :: loop_index

        type(token_t) :: while_token, lparen_token, rparen_token
        integer :: condition_index
        integer, allocatable :: body_indices(:)
        type(statement_callbacks_t) :: callbacks

        call initialize_if_hooks()

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

        callbacks = build_do_body_callbacks()

        loop_index = push_do_while(arena, condition_index, body_indices=[integer::], &
                                   line=line, column=column)

        ! Parse body statements until 'end' (same logic as if blocks)
        block
            integer, allocatable :: temp_body_indices(:)
            type(token_t) :: token
            integer :: stmt_start, stmt_end, j
            type(token_t), allocatable, target :: stmt_tokens(:)

            allocate (temp_body_indices(0))

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

            if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                if (parser%tokens(stmt_start)%text == "enddo" .or. &
                    parser%tokens(stmt_start)%text == "end do") then
                    parser%current_token = stmt_start
                    exit
                else if (parser%tokens(stmt_start)%text == "end") then
                    if (stmt_start + 1 <= size(parser%tokens)) then
                        if (parser%tokens(stmt_start + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(stmt_start + 1)%text == "do") then
                            parser%current_token = stmt_start
                            exit
                        end if
                    else
                        parser%current_token = stmt_start
                        exit
                    end if
                end if
            end if

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
                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                            loop_index, callbacks)

                        ! Add all parsed statements to body
                        do n = 1, size(stmt_indices)
                            if (stmt_indices(n) > 0) then
                                temp_body_indices = [temp_body_indices, stmt_indices(n)]
                            end if
                        end do
                    end block

                    deallocate (stmt_tokens)
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

        if (loop_index > 0) then
            if (allocated(arena%entries(loop_index)%node)) then
                select type(node => arena%entries(loop_index)%node)
                type is (do_while_node)
                    if (allocated(body_indices)) then
                        node%body_indices = body_indices
                    end if
                end select
            end if
        end if

        if (allocated(body_indices)) deallocate (body_indices)
    end function parse_do_while_from_do

end module parser_do_constructs_module
