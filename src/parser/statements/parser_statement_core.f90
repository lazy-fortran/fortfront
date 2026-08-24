module parser_statement_core_module
    use fortfront_constants, only: MAX_DIAGNOSTIC_MESSAGE_LEN
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
        TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state, &
        reject_unconsumed_tokens
    use parser_expressions_module, only: parse_expression
    use parser_io_statements_module, only: parse_print_statement, &
        & parse_write_statement, parse_read_statement, &
        & parse_open_statement, parse_close_statement, &
        & parse_format_statement, parse_inquire_statement, &
        & parse_backspace_statement, parse_rewind_statement, &
        & parse_endfile_statement
    use parser_control_statements_module, only: &
        parse_cycle_statement, parse_exit_statement, parse_return_statement, &
        parse_stop_statement, parse_goto_statement, parse_error_stop_statement, &
        parse_pause_statement, parse_nullify_statement, parse_continue_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
        parse_deallocate_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration, &
        parse_derived_type_def, parser_is_at_type_definition, parse_save_statement
    use parser_call_module, only: parse_call_statement
    use parser_import_resolution_module, only: parse_use_statement
    use parser_utils, only: analyze_declaration_structure
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_assignment, push_pointer_assignment, push_identifier
    use parser_statement_callbacks_module, only: statement_callbacks_t, &
        null_statement_callbacks, &
        call_fallback_do_parser, call_fallback_if_parser, &
        call_fallback_block_parser, &
        call_fallback_select_case_parser, call_fallback_select_type_parser
    use parser_statement_data_module, only: parse_data_statement, &
        parse_namelist_statement
    use parser_dimension_statements_module, only: parse_dimension_statement
    use parser_parameter_statements_module, only: parse_parameter_statement
    use parser_allocatable_statements_module, only: parse_allocatable_statement
    use parser_value_statements_module, only: parse_value_statement
    use parser_statement_detection_module, only: find_statement_end, &
        extend_if_statement_end, extend_do_statement_end, &
        extend_block_statement_end
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    use parser_statement_utilities_module, only: parse_if_from_definition, &
        parse_associate_from_definition
    use parser_trailing_comment_module, only: capture_trailing_comment_from_tokens
    implicit none
    private

    public :: statement_callbacks_t, null_statement_callbacks
    public :: parse_basic_statement_core, find_statement_end, extend_if_statement_end, extend_do_statement_end
    public :: extend_block_statement_end
    public :: parse_data_statement
    public :: allocate_stmt_tokens_with_eof
    public :: skip_whitespace_and_semicolons
    private :: parse_namelist_statement

    integer, parameter :: STATEMENT_NO_NODE = -1

contains
    recursive function parse_basic_statement_core(tokens, arena, parent_index, &
            callbacks, consumed_count, parent_parser) &
            result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, intent(out), optional :: consumed_count
        type(parser_state_t), intent(in), optional :: parent_parser
        integer, allocatable :: stmt_indices(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        type(statement_callbacks_t) :: local_callbacks
        integer :: stmt_index
        logical :: handled

        if (present(callbacks)) then
            local_callbacks = callbacks
        else
            local_callbacks = null_statement_callbacks()
        end if

        parser = create_parser_state(tokens)
        if (present(parent_parser)) then
            if (associated(parent_parser%diagnostic_sink)) then
                parser%diagnostic_sink => parent_parser%diagnostic_sink
            end if
        end if
        first_token = parser%peek()
        handled = try_handle_declaration(parser, arena, first_token, stmt_indices)
        if (handled) then
            if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0 .and. &
                .not. parser%has_errors()) then
                call reject_unconsumed_tokens(parser)
            end if
            if (present(consumed_count)) consumed_count = parser%current_token - 1
            return
        end if

        stmt_index = 0
        block
            character(len=:), allocatable :: lowered_text
            lowered_text = to_lower(first_token%text)
            if (trim(lowered_text) == 'data') then
                ! Check if this looks like a DATA statement before parsing
                if (looks_like_data_statement_local(parser)) then
                    stmt_index = parse_data_statement(parser, arena, parent_index)
                    if (stmt_index > 0) then
                        if (.not. allocated(stmt_indices)) allocate (stmt_indices(1))
                        stmt_indices(1) = stmt_index
                        if (present(consumed_count)) consumed_count = &
                            parser%current_token - 1
                        if (.not. parser%has_errors()) then
                            call reject_unconsumed_tokens(parser)
                        end if
                        return
                    end if
                end if
            end if
        end block
        select case (first_token%kind)
        case (TK_KEYWORD)
            if (keyword_should_parse_as_identifier(first_token, parser)) then
                stmt_index = parse_identifier_statement(parser, arena, &
                    parent_index, tokens, &
                    local_callbacks)
            else
                stmt_index = parse_keyword_statement(first_token, parser, arena, &
                    parent_index, local_callbacks)
            end if
        case (TK_IDENTIFIER)
            stmt_index = parse_identifier_statement(parser, arena, parent_index, &
                tokens, local_callbacks)
        end select

        if (stmt_index == STATEMENT_NO_NODE) then
            if (.not. allocated(stmt_indices)) allocate (stmt_indices(0))
            if (present(consumed_count)) consumed_count = parser%current_token - 1
            return
        end if

        if (stmt_index == 0) then
            if (is_terminator_statement(first_token, tokens)) then
                allocate (stmt_indices(1))
                stmt_indices(1) = 0
            else
                call report_unparsed_statement(parser, tokens)
                allocate (stmt_indices(1))
                stmt_indices(1) = 0
            end if
            if (present(consumed_count)) consumed_count = parser%current_token - 1
            return
        end if

        if (.not. allocated(stmt_indices)) allocate (stmt_indices(1))
        stmt_indices(1) = stmt_index

        call capture_trailing_comment_from_tokens(tokens, arena, stmt_index)

        if (present(consumed_count)) consumed_count = parser%current_token - 1
    end function parse_basic_statement_core

    logical function try_handle_declaration(parser, arena, first_token, stmt_indices) &
            result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, allocatable, intent(out) :: stmt_indices(:)
        logical :: has_initializer, has_comma

        handled = .false.
        if (first_token%kind /= TK_KEYWORD) return
        if (.not. is_declaration_keyword(first_token%text)) return

        ! A local `type :: name` starts a derived-type definition, not a
        ! variable declaration.  Keep it out of the generic declaration fast
        ! path so its components are retained in a derived_type_node.
        if (to_lower(trim(first_token%text)) == "type" .and. &
                parser_is_at_type_definition(parser)) then
            allocate (stmt_indices(1))
            stmt_indices(1) = parse_derived_type_def(parser, arena)
            handled = .true.
            return
        end if

        has_initializer = .false.
        has_comma = .false.
        call analyze_declaration_structure(parser, has_initializer, has_comma)

        if (.not. has_comma) then
            allocate (stmt_indices(1))
            stmt_indices(1) = parse_declaration(parser, arena)
        else
            stmt_indices = parse_multi_declaration(parser, arena)
        end if
        handled = .true.
    end function try_handle_declaration

    logical function is_declaration_keyword(text) result(matches)
        character(len=*), intent(in) :: text

        matches = (text == "real" .or. text == "integer" .or. text == "logical" .or. &
            text == "character" .or. text == "type" .or. text == "class" .or. &
            text == "complex" .or. text == "double" .or. text == "procedure")
    end function is_declaration_keyword

    recursive integer function parse_keyword_statement(first_token, parser, arena, &
            parent_index, &
            callbacks) result(stmt_index)
        type(token_t), intent(in) :: first_token
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks

        stmt_index = 0
        block
            character(len=:), allocatable :: lowered
            type(token_t) :: ignored_token
            lowered = to_lower(first_token%text)

            stmt_index = handle_control_keyword(lowered, parser, arena, &
                parent_index, callbacks)
            if (stmt_index /= 0) return

            ! FLUSH has no AST node yet, but it is a valid executable
            ! statement. Consume the complete statement so it does not become
            ! an unparsed-statement diagnostic while preserving the rest of
            ! the procedure body.
            if (lowered == "flush") then
                do while (.not. parser%is_at_end())
                    ignored_token = parser%consume()
                end do
                stmt_index = STATEMENT_NO_NODE
                return
            end if

            stmt_index = handle_io_keyword(lowered, parser, arena)
            if (stmt_index /= 0) return

            stmt_index = handle_memory_keyword(lowered, parser, arena)
            if (stmt_index /= 0) return

            stmt_index = handle_data_keyword(lowered, parser, arena, parent_index)
            if (stmt_index /= 0) return

            stmt_index = handle_flow_keyword(lowered, parser, arena, parent_index, &
                callbacks)
            if (stmt_index /= 0) return

            ! A BLOCK construct may open with its own USE statements, and this
            ! is the dispatcher a block body goes through. Without this branch
            ! the whole construct failed to parse, which took the enclosing
            ! file with it.
            if (lowered == "use") then
                stmt_index = parse_use_statement(parser, arena)
            end if

            if (lowered == "dimension") then
                stmt_index = parse_dimension_statement(parser, arena)
            end if

            if (lowered == "save") then
                stmt_index = parse_save_statement(parser, arena)
            end if

            if (lowered == "parameter") then
                if (parse_parameter_statement(parser, arena)) then
                    stmt_index = STATEMENT_NO_NODE
                else
                    stmt_index = 0
                end if
            end if

            if (lowered == "allocatable") then
                if (parse_allocatable_statement(parser, arena)) then
                    stmt_index = STATEMENT_NO_NODE
                else
                    stmt_index = 0
                end if
            end if

            if (lowered == "value") then
                stmt_index = parse_value_statement(parser, arena)
            end if
        end block
    end function parse_keyword_statement

    integer function handle_control_keyword(keyword, parser, arena, parent_index, &
            callbacks) result(stmt_index)
        character(len=*), intent(in) :: keyword
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks
        type(token_t) :: prev_token
        integer :: prev_idx
        logical :: skip_if
        character(len=16) :: prev_keyword

        stmt_index = 0
        select case (keyword)
        case ("if")
            ! Check if this "if" is part of a compound keyword like "end if" or "else if"
            skip_if = .false.
            if (parser%current_token >= 2) then
                ! Look back for previous non-whitespace token
                do prev_idx = parser%current_token - 2, max(1, &
                        parser%current_token - 5), -1
                    if (prev_idx >= 1 .and. prev_idx <= size(parser%tokens)) then
                        prev_token = parser%tokens(prev_idx)
                        ! Skip whitespace/newlines/comments
                        if (prev_token%kind == TK_WHITESPACE .or. &
                            prev_token%kind == TK_NEWLINE .or. &
                            prev_token%kind == TK_COMMENT) cycle
                        ! Check if previous keyword is "end" or "else"
                        if (prev_token%kind == TK_KEYWORD) then
                            prev_keyword = to_lower(trim(prev_token%text))
                            if (prev_keyword == "end" .or. prev_keyword == "else") then
                                skip_if = .true.
                            end if
                        end if
                        exit ! Found a significant token, stop searching
                    end if
                end do
            end if

            ! Only call parse_if if this is a standalone "if", not part of compound keyword
            if (.not. skip_if) then
                if (associated(callbacks%parse_if)) then
                    if (present(parent_index)) then
                        stmt_index = callbacks%parse_if(parser, arena, parent_index)
                    else
                        stmt_index = callbacks%parse_if(parser, arena)
                    end if
                else
                    ! The registered full parser first: it handles an if that
                    ! contains another, where the definition-level fallback
                    ! below handles only a single one. Reached when the caller
                    ! populated no callback - `select case` sets only its own
                    ! entry, so a nested if in a case arm lands here.
                    stmt_index = call_fallback_if_parser(parser, arena, &
                        parent_index)
                    if (stmt_index /= 0) return
                    ! Fallback to simple IF parser if callback not set
                    stmt_index = parse_if_from_definition(parser, arena)
                end if
            end if
        case ("do")
            if (associated(callbacks%parse_do_loop)) then
                stmt_index = callbacks%parse_do_loop(parser, arena)
            else
                ! The same indirection `if` uses for its fallback. `select
                ! case` populates only its own callback entry, so a loop
                ! inside a case arm had no parser and came out as an
                ! unrecognised statement.
                stmt_index = call_fallback_do_parser(parser, arena)
            end if
        case ("block")
            ! A BLOCK construct with its own declarations shows up inside
            ! loops and case arms, whose callers populate only their own
            ! callback entry. Without a fallback it was reported as an
            ! unrecognized statement and failed the enclosing file.
            if (associated(callbacks%parse_block)) then
                stmt_index = callbacks%parse_block(parser, arena)
            else
                stmt_index = call_fallback_block_parser(parser, arena)
            end if
        case ("select", "selectcase")
            stmt_index = handle_select_keyword(parser, arena, callbacks)
        case ("where")
            if (associated(callbacks%parse_where)) then
                stmt_index = callbacks%parse_where(parser, arena)
            end if
        case ("forall")
            if (associated(callbacks%parse_forall)) then
                stmt_index = callbacks%parse_forall(parser, arena)
            end if
        case ("associate")
            if (associated(callbacks%parse_associate)) then
                stmt_index = callbacks%parse_associate(parser, arena)
            else
                ! Fallback to simple ASSOCIATE parser if callback not set
                stmt_index = parse_associate_from_definition(parser, arena)
            end if
        end select
    end function handle_control_keyword

    integer function handle_select_keyword(parser, arena, callbacks) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(statement_callbacks_t), intent(in) :: callbacks
        logical :: has_next_token

        stmt_index = 0
        has_next_token = parser%current_token + 1 <= size(parser%tokens)
        if (has_next_token) then
            if (parser%tokens(parser%current_token + 1)%text == "type") then
                if (associated(callbacks%parse_select_type)) then
                    stmt_index = callbacks%parse_select_type(parser, arena)
                else
                    stmt_index = call_fallback_select_type_parser(parser, arena)
                end if
                return
            end if
        end if

        if (associated(callbacks%parse_select_case)) then
            stmt_index = callbacks%parse_select_case(parser, arena)
        else
            stmt_index = call_fallback_select_case_parser(parser, arena)
        end if
    end function handle_select_keyword

    integer function handle_io_keyword(keyword, parser, arena) result(stmt_index)
        character(len=*), intent(in) :: keyword
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        stmt_index = 0
        select case (keyword)
        case ("print")
            stmt_index = parse_print_statement(parser, arena)
        case ("write")
            stmt_index = parse_write_statement(parser, arena)
        case ("read")
            stmt_index = parse_read_statement(parser, arena)
        case ("open")
            stmt_index = parse_open_statement(parser, arena)
        case ("close")
            stmt_index = parse_close_statement(parser, arena)
        case ("format")
            stmt_index = parse_format_statement(parser, arena)
        case ("inquire")
            stmt_index = parse_inquire_statement(parser, arena)
        case ("backspace")
            stmt_index = parse_backspace_statement(parser, arena)
        case ("rewind")
            stmt_index = parse_rewind_statement(parser, arena)
        case ("endfile")
            stmt_index = parse_endfile_statement(parser, arena)
        end select
    end function handle_io_keyword

    integer function handle_memory_keyword(keyword, parser, arena) result(stmt_index)
        character(len=*), intent(in) :: keyword
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        stmt_index = 0
        select case (keyword)
        case ("allocate")
            stmt_index = parse_allocate_statement(parser, arena)
        case ("deallocate")
            stmt_index = parse_deallocate_statement(parser, arena)
        end select
    end function handle_memory_keyword

    integer function handle_data_keyword(keyword, parser, arena, parent_index) &
            result(stmt_index)
        character(len=*), intent(in) :: keyword
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index

        stmt_index = 0
        select case (keyword)
        case ("data")
            ! Check if this is actually a DATA statement or just a variable named "data"
            if (looks_like_data_statement_local(parser)) then
                stmt_index = parse_data_statement(parser, arena, parent_index)
            end if
        case ("namelist")
            stmt_index = parse_namelist_statement(parser, arena, parent_index)
        end select
    end function handle_data_keyword

    logical function looks_like_data_statement_local(parser) result(is_data_stmt)
        type(parser_state_t), intent(in) :: parser
        type(token_t) :: token
        integer :: idx

        is_data_stmt = .false.
        ! Start looking AFTER the current token (which is "data")
        idx = parser%current_token + 1

        ! Skip whitespace and look for the pattern that indicates DATA statement
        do while (idx <= size(parser%tokens))
            token = parser%tokens(idx)
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_OPERATOR)
                select case (trim(token%text))
                case ("/")
                    ! DATA statements have values in /.../ delimiters
                    is_data_stmt = .true.
                    return
                case ("=", "(")
                    ! Assignment or array subscript - not a DATA statement
                    return
                end select
                return
            case default
                ! DATA statement format: DATA var1, var2 / value1, value2 /
                ! Continue scanning to find the / delimiter
                idx = idx + 1
            end select
        end do
    end function looks_like_data_statement_local

    integer function handle_flow_keyword(keyword, parser, arena, parent_index, &
            callbacks) result(stmt_index)
        character(len=*), intent(in) :: keyword
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks

        stmt_index = 0
        select case (keyword)
        case ("cycle")
            stmt_index = parse_cycle_statement(parser, arena)
        case ("exit")
            stmt_index = parse_exit_statement(parser, arena)
        case ("return")
            if (present(parent_index)) then
                stmt_index = parse_return_statement(parser, arena, parent_index)
            else
                stmt_index = parse_return_statement(parser, arena)
            end if
        case ("call")
            stmt_index = parse_call_statement(parser, arena)
        case ("stop")
            stmt_index = parse_stop_statement(parser, arena)
        case ("pause")
            stmt_index = parse_pause_statement(parser, arena)
        case ("nullify")
            stmt_index = parse_nullify_statement(parser, arena)
        case ("continue")
            stmt_index = parse_continue_statement(parser, arena)
        case ("go", "goto")
            stmt_index = parse_goto_statement(parser, arena)
        case ("error")
            stmt_index = parse_error_stop_statement(parser, arena)
        end select
    end function handle_flow_keyword

    integer function parse_identifier_statement(parser, arena, parent_index, tokens, &
            callbacks) &
            result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t), intent(in) :: tokens(:)
        type(statement_callbacks_t), intent(in) :: callbacks
        type(token_t) :: id_token, op_token, keyword_token
        character(len=:), allocatable :: lowered_identifier

        stmt_index = 0
        id_token = parser%consume()
        lowered_identifier = trim(to_lower(id_token%text))
        if (lowered_identifier == "goto") then
            ! Rewind to allow the goto parser to consume the identifier
            parser%current_token = max(1, parser%current_token - 1)
            stmt_index = parse_goto_statement(parser, arena)
            return
        end if

        op_token = parser%peek()

        if (op_token%kind == TK_OPERATOR) then
            select case (op_token%text)
            case (":")
                op_token = parser%consume()
                keyword_token = parser%peek()
                if (keyword_token%kind == TK_KEYWORD) then
                    ! Rewind to before the label so the keyword parser handles it
                    parser%current_token = parser%current_token - 2
                    stmt_index = parse_keyword_statement(keyword_token, &
                        parser, arena, &
                        parent_index, callbacks)
                    return
                end if
            case ("(", "%")
                ! '(' covers array-element or call assignments; '%' covers
                ! derived-type component assignments (e.g. p%x = 7).  Both
                ! route through parse_complex_assignment which parses the
                ! full LHS expression up to '=' / '=>'.
                stmt_index = parse_complex_assignment(parser, arena, &
                    parent_index, tokens, &
                    id_token)
            case ("=", "=>")
                stmt_index = parse_simple_assignment(parser, arena, &
                    parent_index, tokens, &
                    id_token)
            end select
        end if
    end function parse_identifier_statement

    integer function parse_simple_assignment(parser, arena, parent_index, tokens, &
            id_token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t), intent(in) :: tokens(:)
        type(token_t), intent(in) :: id_token
        type(token_t) :: op_token
        type(token_t), allocatable, target :: expr_tokens(:)
        integer :: remaining_count, target_index, value_index
        character(len=:), allocatable :: operator_text

        stmt_index = 0
        op_token = parser%consume()
        operator_text = trim(op_token%text)
        remaining_count = size(tokens) - parser%current_token + 1
        if (remaining_count <= 0) return

        allocate (expr_tokens(remaining_count))
        expr_tokens = tokens(parser%current_token:)
        value_index = parse_expression(expr_tokens, arena, parser)
        if (value_index <= 0) return

        if (present(parent_index)) then
            target_index = push_identifier(arena, id_token%text, id_token%line, &
                id_token%column, parent_index)
        else
            target_index = push_identifier(arena, id_token%text, id_token%line, &
                id_token%column)
        end if

        stmt_index = create_assignment_node(arena, parent_index, target_index, &
            value_index, id_token, operator_text)
    end function parse_simple_assignment

    integer function find_assignment_operator(parser, paren_depth) result(left_end)
        type(parser_state_t), intent(in) :: parser
        integer, intent(inout) :: paren_depth
        integer :: pos
        type(token_t) :: current_token

        left_end = parser%current_token - 1
        do pos = parser%current_token, size(parser%tokens)
            current_token = parser%tokens(pos)
            if (current_token%kind == TK_EOF) exit
            if (current_token%kind == TK_OPERATOR) then
                select case (trim(current_token%text))
                case ("(")
                    paren_depth = paren_depth + 1
                case (")")
                    if (paren_depth > 0) paren_depth = paren_depth - 1
                case ("=", "=>")
                    if (paren_depth == 0) then
                        left_end = pos - 1
                        exit
                    end if
                end select
            else if (current_token%kind == TK_NEWLINE) then
                exit
            end if
            left_end = pos
        end do
    end function find_assignment_operator

    subroutine build_lhs_tokens(lhs_tokens, id_token, parser, left_end)
        type(token_t), allocatable, intent(out) :: lhs_tokens(:)
        type(token_t), intent(in) :: id_token
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: left_end
        integer :: lhs_len

        lhs_len = 1 + max(0, left_end - parser%current_token + 1) + 1
        allocate (lhs_tokens(lhs_len))
        lhs_tokens(1) = id_token
        if (lhs_len >= 3 .and. left_end >= parser%current_token) then
            lhs_tokens(2:lhs_len - 1) = parser%tokens(parser%current_token:left_end)
        end if
        lhs_tokens(lhs_len)%kind = TK_EOF
        lhs_tokens(lhs_len)%text = ""
        lhs_tokens(lhs_len)%line = id_token%line
        lhs_tokens(lhs_len)%column = id_token%column
    end subroutine build_lhs_tokens

    integer function create_assignment_node(arena, parent_index, target_index, &
            value_index, id_token, assignment_op) &
            result(stmt_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, intent(in) :: target_index
        integer, intent(in) :: value_index
        type(token_t), intent(in) :: id_token
        character(len=*), intent(in) :: assignment_op

        if (assignment_op == "=>") then
            if (present(parent_index)) then
                stmt_index = push_pointer_assignment(arena, target_index, &
                    value_index, id_token%line, &
                    id_token%column, parent_index)
            else
                stmt_index = push_pointer_assignment(arena, target_index, &
                    value_index, id_token%line, &
                    id_token%column)
            end if
        else
            if (present(parent_index)) then
                stmt_index = push_assignment(arena, target_index, value_index, &
                    id_token%line, id_token%column, &
                    parent_index, assignment_op)
            else
                stmt_index = push_assignment(arena, target_index, value_index, &
                    id_token%line, id_token%column, &
                    operator_text=assignment_op)
            end if
        end if
    end function create_assignment_node

    integer function parse_complex_assignment(parser, arena, parent_index, tokens, &
            id_token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t), intent(in) :: tokens(:)
        type(token_t), intent(in) :: id_token
        integer :: left_end, paren_depth
        integer :: target_index, value_index, remaining_count
        type(token_t), allocatable, target :: lhs_tokens(:)
        type(token_t), allocatable, target :: rhs_tokens(:)
        type(token_t) :: current_token
        character(len=:), allocatable :: assignment_op

        stmt_index = 0
        paren_depth = 0
        left_end = find_assignment_operator(parser, paren_depth)
        if (left_end < parser%current_token - 1) return

        call build_lhs_tokens(lhs_tokens, id_token, parser, left_end)
        target_index = parse_expression(lhs_tokens, arena, parser)
        if (target_index <= 0) return

        parser%current_token = left_end + 1
        assignment_op = "="
        if (parser%current_token <= size(parser%tokens)) then
            current_token = parser%peek()
            if (current_token%kind == TK_OPERATOR) then
                select case (current_token%text)
                case ("=", "=>")
                    assignment_op = trim(current_token%text)
                    current_token = parser%consume()
                end select
            end if
        end if

        remaining_count = size(tokens) - parser%current_token + 1
        if (remaining_count <= 0) return

        allocate (rhs_tokens(remaining_count))
        rhs_tokens = tokens(parser%current_token:)
        value_index = parse_expression(rhs_tokens, arena, parser)
        if (value_index <= 0) return

        stmt_index = create_assignment_node(arena, parent_index, target_index, &
            value_index, id_token, assignment_op)
    end function parse_complex_assignment

    logical function is_terminator_statement(first_token, tokens) result(is_terminator)
        type(token_t), intent(in) :: first_token
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable :: lowered

        is_terminator = .false.
        if (first_token%kind /= TK_KEYWORD) return

        lowered = to_lower(first_token%text)
        select case (lowered)
        case ("end")
            if (size(tokens) >= 2) then
                select case (to_lower(tokens(2)%text))
                case ("if", "do", "select", "where", "forall", "associate", "case")
                    is_terminator = .true.
                end select
            else
                is_terminator = .true.
            end if
        case ("else", "elseif", "contains", "case", "endselect", "enddo", &
                "endif", "endwhere", "endforall", "elsewhere")
            is_terminator = .true.
        end select
    end function is_terminator_statement

    subroutine report_unparsed_statement(parser, tokens)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: tokens(:)
        character(len=MAX_DIAGNOSTIC_MESSAGE_LEN) :: message
        character(len=64) :: token_text
        integer :: i, msg_len

        message = "Unrecognized statement:"
        msg_len = len_trim(message)

        do i = 1, min(3, size(tokens))
            if (tokens(i)%kind == TK_EOF) exit
            if (len_trim(tokens(i)%text) == 0) cycle

            token_text = trim(tokens(i)%text)
            if (msg_len + len_trim(token_text) + 1 < len(message)) then
                message = message(1:msg_len)//" "//token_text
                msg_len = len_trim(message)
            end if
        end do

        if (msg_len == len_trim("Unrecognized statement:")) then
            message = "Unrecognized statement"
        end if

        parser%current_token = max(1, parser%current_token)
        call parser%error(trim(message))
    end subroutine report_unparsed_statement

    subroutine allocate_stmt_tokens_with_eof(stmt_tokens, source_tokens, &
            current_token, stmt_end)
        type(token_t), allocatable, intent(out) :: stmt_tokens(:)
        type(token_t), intent(in) :: source_tokens(:)
        integer, intent(in) :: current_token, stmt_end
        integer :: remaining_count

        remaining_count = stmt_end - current_token + 1
        allocate (stmt_tokens(remaining_count + 1))
        stmt_tokens(1:remaining_count) = source_tokens(current_token:stmt_end)
        stmt_tokens(remaining_count + 1)%kind = TK_EOF
        stmt_tokens(remaining_count + 1)%text = ""
    end subroutine allocate_stmt_tokens_with_eof

    subroutine skip_whitespace_and_semicolons(parser)
        type(parser_state_t), intent(inout) :: parser

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
    end subroutine skip_whitespace_and_semicolons
end module parser_statement_core_module
