module parser_do_constructs_module
    ! Parser module for DO constructs (do loops, do while)
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
                          TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module
    use parser_expressions_module, only: parse_logical_or
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_factory, only: push_do_loop, push_do_while
    use parser_if_constructs_module, only: parse_if, register_parse_do_loop
    use parser_select_constructs_module, only: parse_select_case
    use parser_array_constructs_module, only: parse_where_construct, parse_associate
    use parser_forall_module, only: parse_forall
    use parser_statement_core_module, only: parse_basic_statement_core, &
                                            statement_callbacks_t, &
                                            null_statement_callbacks, &
                                            find_statement_end
    implicit none
    private

    type :: do_loop_control_t
        character(len=:), allocatable :: var_name
        integer :: start_index = 0
        integer :: end_index = 0
        integer :: step_index = 0
    end type do_loop_control_t

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

    subroutine append_control(control_list, control)
        type(do_loop_control_t), allocatable, intent(inout) :: control_list(:)
        type(do_loop_control_t), intent(in) :: control

        if (.not. allocated(control_list)) then
            allocate (control_list(1))
            control_list(1) = control
        else
            control_list = [control_list, control]
        end if
    end subroutine append_control

    logical function next_tokens_form_control(parser, start_pos) result(is_control)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: start_pos
        integer :: idx

        idx = start_pos
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (idx > size(parser%tokens)) then
            is_control = .false.
            return
        end if

        if (parser%tokens(idx)%kind /= TK_IDENTIFIER) then
            is_control = .false.
            return
        end if

        idx = idx + 1
        do while (idx <= size(parser%tokens))
            select case (parser%tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (idx > size(parser%tokens)) then
            is_control = .false.
            return
        end if

        if (parser%tokens(idx)%kind == TK_OPERATOR .and. &
            parser%tokens(idx)%text == "=") then
            is_control = .true.
        else
            is_control = .false.
        end if
    end function next_tokens_form_control

    logical function parse_standard_control(parser, arena, control) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_control_t), intent(out) :: control
        type(token_t) :: tok
        logical :: allow_colon_step

        success = .false.
        control%step_index = 0

        tok = parser%consume()
        if (tok%kind /= TK_IDENTIFIER) return
        control%var_name = tok%text

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= "=") return

        control%start_index = parse_logical_or(parser, arena)
        if (control%start_index <= 0) return

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR) return
        allow_colon_step = tok%text == ":"
        if (.not. (tok%text == "," .or. tok%text == ":")) return

        control%end_index = parse_logical_or(parser, arena)
        if (control%end_index <= 0) return

        if (.not. parser%is_at_end()) then
            tok = parser%peek()
            if (tok%kind == TK_OPERATOR) then
                if (tok%text == ",") then
                    tok = parser%consume()
                    control%step_index = parse_logical_or(parser, arena)
                    if (control%step_index <= 0) return
                else if (allow_colon_step .and. tok%text == ":") then
                    tok = parser%consume()
                    control%step_index = parse_logical_or(parser, arena)
                    if (control%step_index <= 0) return
                end if
            end if
        end if

        if (control%step_index < 0) control%step_index = 0
        success = control%start_index > 0 .and. control%end_index > 0
    end function parse_standard_control

    logical function parse_concurrent_control(parser, arena, control) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_control_t), intent(out) :: control
        type(token_t) :: tok

        success = .false.
        control%step_index = 0

        tok = parser%consume()
        if (tok%kind /= TK_IDENTIFIER) return
        control%var_name = tok%text

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= "=") return

        control%start_index = parse_logical_or(parser, arena)
        if (control%start_index <= 0) return

        tok = parser%consume()
        if (tok%kind /= TK_OPERATOR .or. tok%text /= ":") return

        control%end_index = parse_logical_or(parser, arena)
        if (control%end_index <= 0) return

        if (.not. parser%is_at_end()) then
            tok = parser%peek()
            if (tok%kind == TK_OPERATOR .and. tok%text == ":") then
                tok = parser%consume()
                control%step_index = parse_logical_or(parser, arena)
                if (control%step_index <= 0) return
            end if
        end if

        if (control%step_index < 0) control%step_index = 0
        success = control%start_index > 0 .and. control%end_index > 0
    end function parse_concurrent_control

    logical function parse_concurrent_control_list(parser, arena, controls) &
        result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_control_t), allocatable, intent(out) :: controls(:)
        type(do_loop_control_t) :: control
        type(token_t) :: tok
        integer :: lookahead

        success = .false.
        if (allocated(controls)) then
            deallocate (controls)
        end if
        allocate (controls(0))

        if (.not. parse_concurrent_control(parser, arena, control)) return
        call append_control(controls, control)

        do
            if (parser%is_at_end()) exit
            tok = parser%peek()
            if (tok%kind /= TK_OPERATOR .or. tok%text /= ",") exit

            lookahead = parser%current_token + 1
            if (.not. next_tokens_form_control(parser, lookahead)) return

            tok = parser%consume()
            if (.not. parse_concurrent_control(parser, arena, control)) return
            call append_control(controls, control)
        end do

        success = size(controls) > 0
    end function parse_concurrent_control_list

    logical function has_then_before_newline(tokens, start_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: idx

        has_then_before_newline = .false.
        do idx = start_pos + 1, size(tokens)
            select case (tokens(idx)%kind)
            case (TK_KEYWORD)
                if (to_lower(tokens(idx)%text) == "then") then
                    has_then_before_newline = .true.
                    return
                else if (to_lower(tokens(idx)%text) == "endif" .or. &
                         to_lower(tokens(idx)%text) == "end") then
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
                case ("elseif", "else")
                    ! Advance but do not change depth; skip remaining checks
                    idx = idx + 1
                    cycle
                end select
            case (TK_EOF)
                exit
            end select
            idx = idx + 1
        end do
    end function find_matching_end_if

    integer function skip_trivia_tokens(tokens, start_idx) result(next_idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx
        integer :: idx

        idx = start_idx
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                exit
            end select
        end do
        next_idx = idx
    end function skip_trivia_tokens

    integer function find_matching_end_do(tokens, start_pos) result(end_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos

        integer :: idx, next_idx
        integer :: depth
        type(token_t) :: token

        end_index = -1
        if (start_pos < 1 .or. start_pos > size(tokens)) return

        depth = 1
        idx = start_pos + 1
        do while (idx <= size(tokens))
            token = tokens(idx)

            ! Check for labeled DO: identifier : do
            if (token%kind == TK_IDENTIFIER .and. idx + 2 <= size(tokens)) then
                if (tokens(idx + 1)%kind == TK_OPERATOR .and. &
                    tokens(idx + 1)%text == ":" .and. &
                    tokens(idx + 2)%kind == TK_KEYWORD .and. &
                    tokens(idx + 2)%text == "do") then
                    depth = depth + 1
                    idx = idx + 2
                    cycle
                end if
            end if

            if (token%kind == TK_KEYWORD) then
                select case (token%text)
                case ("do")
                    depth = depth + 1
                case ("enddo", "end do")
                    depth = depth - 1
                    if (depth == 0) then
                        end_index = idx
                        return
                    end if
                case ("end")
                    ! Skip whitespace/newlines/comments after "end"
                    next_idx = skip_trivia_tokens(tokens, idx + 1)
                    if (next_idx <= size(tokens)) then
                        if (tokens(next_idx)%kind == TK_KEYWORD .and. &
                            tokens(next_idx)%text == "do") then
                            depth = depth - 1
                            if (depth == 0) then
                                ! Check if there's a label after "end do"
                                if (next_idx + 1 <= size(tokens) .and. &
                                    tokens(next_idx + 1)%kind == TK_IDENTIFIER) then
                                    end_index = next_idx + 1
                                else
                                    end_index = next_idx
                                end if
                                return
                            end if
                            idx = next_idx
                        end if
                    end if
                end select
            end if
            idx = idx + 1
        end do
    end function find_matching_end_do

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

        type(token_t) :: do_token, var_token, comma_token, label_token
        character(len=:), allocatable :: var_name, loop_label
        type(do_loop_control_t), allocatable :: controls(:)
        type(do_loop_control_t) :: control
        integer :: start_index, end_index, step_index
        integer :: line, column
        integer :: control_count
        logical :: has_open_paren
        logical :: is_concurrent

        call initialize_if_hooks()

        step_index = 0  ! Initialize to 0 (no step)
        loop_index = 0  ! Initialize to 0 (failure) in case of early return
        is_concurrent = .false.

        ! Starting to parse do loop

        ! Check for optional loop label at start: label: do ...
        if (parser%current_token >= 1 .and. parser%current_token + 2 <= &
            size(parser%tokens)) then
            if (parser%tokens(parser%current_token)%kind == TK_IDENTIFIER .and. &
                parser%tokens(parser%current_token + 1)%kind == TK_OPERATOR .and. &
                parser%tokens(parser%current_token + 1)%text == ":" .and. &
                parser%tokens(parser%current_token + 2)%kind == TK_KEYWORD .and. &
                parser%tokens(parser%current_token + 2)%text == "do") then
                label_token = parser%tokens(parser%current_token)
                loop_label = label_token%text
                parser%current_token = parser%current_token + 2
            end if
        end if

        ! Consume 'do'
        do_token = parser%consume()
        line = do_token%line
        column = do_token%column

        ! Check if it's a do while loop
        var_token = parser%peek()
        if (var_token%kind == TK_KEYWORD .and. var_token%text == "while") then
            ! Parse as do while loop
            if (allocated(loop_label)) then
                loop_index = parse_do_while_from_do(parser, arena, line, column, &
                                                    loop_label)
            else
                loop_index = parse_do_while_from_do(parser, arena, line, column)
            end if
            return
        end if

        allocate (controls(0))
        has_open_paren = .false.

        if (var_token%kind == TK_KEYWORD .and. var_token%text == "concurrent") then
            is_concurrent = .true.
            var_token = parser%consume()

            if (.not. parser%is_at_end()) then
                comma_token = parser%peek()
                if (comma_token%kind == TK_OPERATOR .and. comma_token%text == "(") then
                    has_open_paren = .true.
                    comma_token = parser%consume()
                end if
            end if

            if (.not. parse_concurrent_control_list(parser, arena, controls)) then
                loop_index = 0
                return
            end if

            if (has_open_paren) then
                if (parser%is_at_end()) then
                    loop_index = 0
                    return
                end if
                comma_token = parser%consume()
                if (comma_token%kind /= TK_OPERATOR .or. comma_token%text /= ")") then
                    loop_index = 0
                    return
                end if
            end if
        else
            if (.not. parse_standard_control(parser, arena, control)) then
                loop_index = 0
                return
            end if
            call append_control(controls, control)
        end if

        control_count = size(controls)
        if (control_count <= 0) then
            loop_index = 0
            return
        end if

        control = controls(control_count)
        var_name = control%var_name
        start_index = control%start_index
        end_index = control%end_index
        step_index = control%step_index

        if (start_index <= 0 .or. end_index <= 0) then
            loop_index = 0
            return
        end if

        ! Parse body statements until 'end do'
        block
            integer, allocatable :: body_indices(:)
            integer :: stmt_start, stmt_end, j, token_count, next_index
            type(token_t), allocatable, target :: stmt_tokens(:)
            type(token_t) :: next_token
            type(statement_callbacks_t) :: callbacks

            allocate (body_indices(0))
            callbacks = build_do_body_callbacks()

            if (step_index > 0) then
                if (control_count == 1 .and. allocated(loop_label)) then
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              step_index=step_index, &
                                              body_indices=[integer ::], &
                                              loop_label=loop_label, &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column)
                else
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              step_index=step_index, &
                                              body_indices=[integer ::], &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column)
                end if
            else
                if (control_count == 1 .and. allocated(loop_label)) then
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              body_indices=[integer ::], &
                                              loop_label=loop_label, &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column)
                else
                    loop_index = push_do_loop(arena, var_name, start_index, &
                                              end_index, &
                                              body_indices=[integer ::], &
                                              is_concurrent=is_concurrent, &
                                              line=line, column=column)
                end if
            end if

            do while (parser%current_token <= size(parser%tokens))
                block
                    type(token_t) :: current_token
                    integer :: check_idx

                    current_token = parser%peek()
                    if (current_token%kind == TK_KEYWORD .and. &
                        current_token%text == "end") then
                        ! Skip whitespace/newlines/comments after "end"
                        check_idx = skip_trivia_tokens(parser%tokens, &
                                                       parser%current_token + 1)
                        if (check_idx <= size(parser%tokens)) then
                            if (parser%tokens(check_idx)%kind == TK_KEYWORD .and. &
                                parser%tokens(check_idx)%text == "do") then
                                ! Consume all tokens from "end" to "do"
                                current_token = parser%consume()  ! consume "end"
                                do while (parser%current_token < check_idx)
                                    current_token = parser%consume()  ! skip trivia
                                end do
                                current_token = parser%consume()  ! consume "do"
                                exit
                            end if
                        end if
                    end if
                end block

                stmt_start = parser%current_token

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
                        ! Skip whitespace/newlines/comments after "end"
                        next_index = skip_trivia_tokens(parser%tokens, stmt_start + 1)
                        if (next_index <= size(parser%tokens)) then
                            next_token = parser%tokens(next_index)
                            if (next_token%kind == TK_KEYWORD .and. &
                                next_token%text == "do") then
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
                    integer :: block_end, lookahead, do_pos
                    logical :: handled_block

                    handled_block = .false.

                    ! Check for label: do pattern
                    do_pos = stmt_start
                    if (parser%tokens(stmt_start)%kind == TK_IDENTIFIER .and. &
                        stmt_start + 2 <= size(parser%tokens)) then
                        if (parser%tokens(stmt_start + 1)%kind == TK_OPERATOR .and. &
                            parser%tokens(stmt_start + 1)%text == ":" .and. &
                            parser%tokens(stmt_start + 2)%kind == TK_KEYWORD .and. &
                            parser%tokens(stmt_start + 2)%text == "do") then
                            do_pos = stmt_start + 2
                        end if
                    end if

                    if (parser%tokens(do_pos)%kind == TK_KEYWORD) then
                        select case (parser%tokens(do_pos)%text)
                        case ("if")
                            block_end = find_matching_end_if(parser%tokens, do_pos)
                            if (block_end > do_pos) then
                                stmt_end = block_end
                                handled_block = .true.
                            end if
                        case ("do")
                            block_end = find_matching_end_do(parser%tokens, do_pos)
                            if (block_end > do_pos) then
                                stmt_end = block_end
                                handled_block = .true.
                            end if
                        case ("select")
                            lookahead = stmt_start + 1
                            do while (lookahead <= size(parser%tokens))
                                select case (parser%tokens(lookahead)%kind)
                                case (TK_WHITESPACE, TK_COMMENT)
                                    lookahead = lookahead + 1
                                    cycle
                                end select
                                exit
                            end do
                            if (lookahead <= size(parser%tokens)) then
                                if (parser%tokens(lookahead)%kind == TK_KEYWORD) then
                                    if (parser%tokens(lookahead)%text == "case") then
                                        block_end = find_statement_end( &
                                                    parser%tokens, stmt_start)
                                        if (block_end > stmt_start) then
                                            stmt_end = block_end
                                            handled_block = .true.
                                        end if
                                    end if
                                end if
                            end if
                        end select
                    end if

                    if (.not. handled_block) then
                        do j = stmt_start, size(parser%tokens)
                            select case (parser%tokens(j)%kind)
                            case (TK_EOF)
                                stmt_end = j
                                exit
                            case (TK_NEWLINE)
                                if (j == stmt_start) then
                                    stmt_end = j
                                else
                                    stmt_end = j - 1
                                end if
                                exit
                            case (TK_OPERATOR)
                                if (j > stmt_start) then
                                    if (parser%tokens(j)%text == ";") then
                                        stmt_end = j - 1
                                        exit
                                    end if
                                end if
                            end select
                            stmt_end = j
                        end do
                    end if
                end block

                if (stmt_end >= stmt_start) then
                    token_count = stmt_end - stmt_start + 1
                    allocate (stmt_tokens(token_count + 1))
                    stmt_tokens(1:token_count) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(token_count + 1)%kind = TK_EOF
                    stmt_tokens(token_count + 1)%text = ""
                    stmt_tokens(token_count + 1)%line = parser%tokens(stmt_end)%line
                    stmt_tokens(token_count + 1)%column = &
                        parser%tokens(stmt_end)%column + 1

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
                            if (stmt_end + 1 <= size(parser%tokens)) then
                                next_index = stmt_end + 1
                                next_token = parser%tokens(next_index)
                                if (next_token%kind == TK_OPERATOR .and. &
                                    next_token%text == ";") then
                                    parser%current_token = stmt_end + 2
                                else
                                    parser%current_token = stmt_end + 1
                                end if
                            else
                                parser%current_token = stmt_end + 1
                            end if
                            block
                                type(token_t), allocatable, target :: temp(:)
                                call move_alloc(stmt_tokens, temp)
                            end block
                            cycle
                        end if

                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                                  loop_index, &
                                                                  callbacks)

                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                            end if
                        end do
                    end block

                    block
                        type(token_t), allocatable, target :: temp(:)
                        call move_alloc(stmt_tokens, temp)
                    end block
                end if

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

            if (allocated(arena%entries(loop_index)%node)) then
                select type (node => arena%entries(loop_index)%node)
                type is (do_loop_node)
                    if (allocated(body_indices)) then
                        node%body_indices = body_indices
                    end if
                end select
            end if

            ! Consume the "end do" tokens after loop body is parsed
            block
                type(token_t) :: end_token
                integer :: skip_idx

                if (parser%current_token <= size(parser%tokens)) then
                    end_token = parser%peek()
                    if (end_token%kind == TK_KEYWORD) then
                        if (end_token%text == "enddo" .or. end_token%text == "end do") then
                            end_token = parser%consume()
                        else if (end_token%text == "end") then
                            end_token = parser%consume()  ! consume "end"
                            ! Skip whitespace/newlines/comments to find "do"
                            do while (parser%current_token <= size(parser%tokens))
                                end_token = parser%peek()
                                if (end_token%kind == TK_WHITESPACE .or. &
                                    end_token%kind == TK_NEWLINE .or. &
                                    end_token%kind == TK_COMMENT) then
                                    end_token = parser%consume()
                                else
                                    exit
                                end if
                            end do
                            ! Consume "do"
                            if (parser%current_token <= size(parser%tokens)) then
                                end_token = parser%peek()
                                if (end_token%kind == TK_KEYWORD .and. &
                                    end_token%text == "do") then
                                    end_token = parser%consume()
                                end if
                            end if
                        end if
                    end if
                end if
            end block
        end block

        if (control_count > 1) then
            block
                integer :: idx, outer_index, outer_step
                integer :: outer_start, outer_end
                character(len=:), allocatable :: outer_name

                do idx = control_count - 1, 1, -1
                    outer_name = controls(idx)%var_name
                    outer_start = controls(idx)%start_index
                    outer_end = controls(idx)%end_index
                    outer_step = controls(idx)%step_index

                    if (outer_start <= 0 .or. outer_end <= 0) then
                        loop_index = 0
                        return
                    end if

                    if (outer_step > 0) then
                        if (idx == 1 .and. allocated(loop_label)) then
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       step_index=outer_step, &
                                                       body_indices=[loop_index], &
                                                       loop_label=loop_label, &
                                                       line=line, column=column)
                        else
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       step_index=outer_step, &
                                                       body_indices=[loop_index], &
                                                       line=line, column=column)
                        end if
                    else
                        if (idx == 1 .and. allocated(loop_label)) then
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       body_indices=[loop_index], &
                                                       loop_label=loop_label, &
                                                       line=line, column=column)
                        else
                            outer_index = push_do_loop(arena, outer_name, &
                                                       outer_start, &
                                                       outer_end, &
                                                       body_indices=[loop_index], &
                                                       line=line, column=column)
                        end if
                    end if

                    if (outer_index <= 0) then
                        loop_index = 0
                        return
                    end if

                    loop_index = outer_index
                end do
            end block
        end if

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
    function parse_do_while_from_do(parser, arena, line, column, loop_label) &
        result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: line, column
        character(len=*), intent(in), optional :: loop_label
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

        if (present(loop_label)) then
            loop_index = push_do_while(arena, condition_index, &
                                       body_indices=[integer ::], &
                                       loop_label=loop_label, line=line, column=column)
        else
            loop_index = push_do_while(arena, condition_index, &
                                       body_indices=[integer ::], &
                                       line=line, column=column)
        end if

        ! Parse body statements until 'end' (same logic as if blocks)
        block
            integer, allocatable :: temp_body_indices(:)
            type(token_t) :: token
            integer :: stmt_start, stmt_end, j, token_count, next_index
            type(token_t), allocatable, target :: stmt_tokens(:)

            allocate (temp_body_indices(0))

            do while (.not. parser%is_at_end())
                token = parser%peek()

                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Skip whitespace/newlines/comments after "end"
                    next_index = skip_trivia_tokens(parser%tokens, &
                                                    parser%current_token + 1)
                    if (next_index <= size(parser%tokens)) then
                        if (parser%tokens(next_index)%kind == TK_KEYWORD .and. &
                            parser%tokens(next_index)%text == "do") then
                            exit
                        end if
                    end if
                end if

                stmt_start = parser%current_token
                stmt_end = stmt_start

                if (parser%tokens(stmt_start)%kind == TK_KEYWORD) then
                    if (parser%tokens(stmt_start)%text == "enddo" .or. &
                        parser%tokens(stmt_start)%text == "end do") then
                        parser%current_token = stmt_start
                        exit
                    else if (parser%tokens(stmt_start)%text == "end") then
                        ! Skip whitespace/newlines/comments after "end"
                        next_index = skip_trivia_tokens(parser%tokens, stmt_start + 1)
                        if (next_index <= size(parser%tokens)) then
                            if (parser%tokens(next_index)%kind == TK_KEYWORD .and. &
                                parser%tokens(next_index)%text == "do") then
                                parser%current_token = stmt_start
                                exit
                            end if
                        else
                            parser%current_token = stmt_start
                            exit
                        end if
                    end if
                end if

                do j = stmt_start, size(parser%tokens)
                    select case (parser%tokens(j)%kind)
                    case (TK_EOF)
                        stmt_end = j
                        exit
                    case (TK_NEWLINE)
                        if (j == stmt_start) then
                            stmt_end = j
                        else
                            stmt_end = j - 1
                        end if
                        exit
                    end select
                    stmt_end = j
                end do

                if (stmt_end >= stmt_start) then
                    token_count = stmt_end - stmt_start + 1
                    allocate (stmt_tokens(token_count + 1))
                    stmt_tokens(1:token_count) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(token_count + 1)%kind = TK_EOF
                    stmt_tokens(token_count + 1)%text = ""
                    stmt_tokens(token_count + 1)%line = parser%tokens(stmt_end)%line
                    stmt_tokens(token_count + 1)%column = &
                        parser%tokens(stmt_end)%column + 1

                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: n

                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                                  loop_index, &
                                                                  callbacks)

                        do n = 1, size(stmt_indices)
                            if (stmt_indices(n) > 0) then
                                temp_body_indices = [temp_body_indices, &
                                                     stmt_indices(n)]
                            end if
                        end do
                    end block

                    block
                        type(token_t), allocatable, target :: temp(:)
                        call move_alloc(stmt_tokens, temp)
                    end block
                end if

                parser%current_token = stmt_end + 1
            end do

            body_indices = temp_body_indices
        end block

        ! Consume 'end do' tokens
        block
            type(token_t) :: token
            integer :: check_idx
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                while_token = parser%consume()  ! consume 'end'
                ! Skip whitespace/newlines/comments to find "do"
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_WHITESPACE .or. token%kind == TK_NEWLINE .or. &
                        token%kind == TK_COMMENT) then
                        while_token = parser%consume()  ! skip trivia
                    else
                        exit
                    end if
                end do
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "do") then
                    while_token = parser%consume()  ! consume 'do'
                end if
            end if
        end block

        if (loop_index > 0) then
            if (allocated(arena%entries(loop_index)%node)) then
                select type (node => arena%entries(loop_index)%node)
                type is (do_while_node)
                    if (allocated(body_indices)) then
                        node%body_indices = body_indices
                    end if
                end select
            end if
        end if

        if (allocated(body_indices)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(body_indices, temp)
            end block
        end if
    end function parse_do_while_from_do

end module parser_do_constructs_module
