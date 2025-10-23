module parser_statement_core_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
                          TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression, parse_expression_until
    use parser_io_statements_module, only: parse_print_statement, &
                                           parse_write_statement, parse_read_statement
    use parser_control_statements_module, only: &
        parse_cycle_statement, parse_exit_statement, parse_return_statement, &
        parse_stop_statement, parse_goto_statement, parse_error_stop_statement, &
        parse_pause_statement, parse_nullify_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_call_module, only: parse_call_statement
    use parser_utils, only: analyze_declaration_structure
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_assignment, push_identifier
    use parser_statement_callbacks_module, only: statement_callbacks_t, &
                                                 null_statement_callbacks
    use parser_statement_data_module, only: parse_data_statement, &
                                            parse_namelist_statement
    use parser_dimension_statements_module, only: parse_dimension_statement
    use parser_statement_detection_module, only: is_block_if, find_statement_end, &
                                                  extend_if_statement_end
    implicit none
    private

    public :: statement_callbacks_t, null_statement_callbacks
    public :: parse_basic_statement_core, find_statement_end, extend_if_statement_end
    public :: parse_data_statement
    public :: allocate_stmt_tokens_with_eof
    public :: skip_whitespace_and_semicolons
    private :: parse_namelist_statement

    integer, parameter :: STATEMENT_NO_NODE = -1

contains

    function parse_basic_statement_core(tokens, arena, parent_index, callbacks, &
                                        consumed_count) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, intent(out), optional :: consumed_count
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
        first_token = parser%peek()
        handled = try_handle_declaration(parser, arena, first_token, stmt_indices)
        if (handled) then
            if (present(consumed_count)) then
                consumed_count = parser%current_token - 1
            end if
            return
        end if

        stmt_index = 0
        block
            character(len=:), allocatable :: lowered_text
            lowered_text = to_lower(first_token%text)
            if (trim(lowered_text) == 'data') then
                stmt_index = parse_data_statement(parser, arena, parent_index)
                if (stmt_index > 0) then
                    if (.not. allocated(stmt_indices)) allocate (stmt_indices(1))
                    stmt_indices(1) = stmt_index
                    if (present(consumed_count)) consumed_count = &
                        parser%current_token - 1
                    return
                end if
            end if
        end block
        select case (first_token%kind)
        case (TK_KEYWORD)
            stmt_index = parse_keyword_statement(first_token, parser, arena, &
                                                 parent_index, local_callbacks)
        case (TK_IDENTIFIER)
            stmt_index = parse_identifier_statement(parser, arena, parent_index, &
                                                    tokens, local_callbacks)
        end select

        if (stmt_index == STATEMENT_NO_NODE) then
            if (.not. allocated(stmt_indices)) allocate (stmt_indices(0))
            if (present(consumed_count)) then
                consumed_count = parser%current_token - 1
            end if
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
            if (present(consumed_count)) then
                consumed_count = parser%current_token - 1
            end if
            return
        end if

        if (.not. allocated(stmt_indices)) then
            allocate (stmt_indices(1))
        end if
        stmt_indices(1) = stmt_index

        if (present(consumed_count)) then
            consumed_count = parser%current_token - 1
        end if
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
                   text == "complex" .or. text == "double")
    end function is_declaration_keyword

    integer function parse_keyword_statement(first_token, parser, arena, &
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
            lowered = to_lower(first_token%text)
            select case (lowered)
            case ("if")
                if (associated(callbacks%parse_if)) then
                    if (present(parent_index)) then
                        stmt_index = callbacks%parse_if(parser, arena, parent_index)
                    else
                        stmt_index = callbacks%parse_if(parser, arena)
                    end if
                end if
            case ("do")
                if (associated(callbacks%parse_do_loop)) then
                    stmt_index = callbacks%parse_do_loop(parser, arena)
                end if
            case ("select", "selectcase")
                ! Look ahead to distinguish SELECT CASE from SELECT TYPE
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%text == "type") then
                        if (associated(callbacks%parse_select_type)) then
                            stmt_index = callbacks%parse_select_type(parser, arena)
                        end if
                    else
                        if (associated(callbacks%parse_select_case)) then
                            stmt_index = callbacks%parse_select_case(parser, arena)
                        end if
                    end if
                else
                    if (associated(callbacks%parse_select_case)) then
                        stmt_index = callbacks%parse_select_case(parser, arena)
                    end if
                end if
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
                end if
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                stmt_index = parse_read_statement(parser, arena)
            case ("data")
                stmt_index = parse_data_statement(parser, arena, parent_index)
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
            case ("go", "goto")
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case ("namelist")
                stmt_index = parse_namelist_statement(parser, arena, parent_index)
            case ("dimension")
                if (parse_dimension_statement(parser, arena)) then
                    stmt_index = STATEMENT_NO_NODE
                else
                    stmt_index = 0
                end if
            end select
        end block
    end function parse_keyword_statement

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

        ! Check for labeled construct: label: keyword
        if (op_token%kind == TK_OPERATOR .and. op_token%text == ":") then
            op_token = parser%consume()
            keyword_token = parser%peek()
            if (keyword_token%kind == TK_KEYWORD) then
                ! Rewind to before the label so the keyword parser handles it
                parser%current_token = parser%current_token - 2
                stmt_index = parse_keyword_statement(keyword_token, parser, arena, &
                                                     parent_index, callbacks)
                return
            end if
        else if (op_token%kind == TK_OPERATOR .and. op_token%text == "(") then
            stmt_index = parse_complex_assignment(parser, arena, &
                                                  parent_index, tokens, &
                                                  id_token)
        else if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
            stmt_index = parse_simple_assignment(parser, arena, parent_index, tokens, &
                                                 id_token)
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

        stmt_index = 0
        op_token = parser%consume()
        remaining_count = size(tokens) - parser%current_token + 1
        if (remaining_count <= 0) return

        allocate (expr_tokens(remaining_count))
        expr_tokens = tokens(parser%current_token:)
        value_index = parse_expression(expr_tokens, arena)
        if (value_index <= 0) return

        if (present(parent_index)) then
            target_index = push_identifier(arena, id_token%text, id_token%line, &
                                           id_token%column, parent_index)
        else
            target_index = push_identifier(arena, id_token%text, id_token%line, &
                                           id_token%column)
        end if

        stmt_index = push_assignment(arena, target_index, value_index, id_token%line, &
                                     id_token%column, parent_index)
    end function parse_simple_assignment

    integer function parse_complex_assignment(parser, arena, parent_index, tokens, &
                                              id_token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t), intent(in) :: tokens(:)
        type(token_t), intent(in) :: id_token
        integer :: left_end, pos, paren_depth
        integer :: target_index, value_index, lhs_len, remaining_count
        type(token_t), allocatable, target :: lhs_tokens(:)
        type(token_t), allocatable, target :: rhs_tokens(:)
        type(token_t) :: current_token

        stmt_index = 0
        left_end = parser%current_token - 1
        paren_depth = 0

        do pos = parser%current_token, size(parser%tokens)
            current_token = parser%tokens(pos)
            if (current_token%kind == TK_EOF) exit
            if (current_token%kind == TK_OPERATOR) then
                select case (trim(current_token%text))
                case ("(")
                    paren_depth = paren_depth + 1
                case (")")
                    if (paren_depth > 0) paren_depth = paren_depth - 1
                case ("=")
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

        if (left_end < parser%current_token - 1) return

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

        target_index = parse_expression(lhs_tokens, arena)
        if (target_index <= 0) return

        parser%current_token = left_end + 1
        if (parser%current_token <= size(parser%tokens)) then
            current_token = parser%peek()
            if (current_token%kind == TK_OPERATOR .and. current_token%text == "=") then
                current_token = parser%consume()
            end if
        end if

        remaining_count = size(tokens) - parser%current_token + 1
        if (remaining_count <= 0) return

        allocate (rhs_tokens(remaining_count))
        rhs_tokens = tokens(parser%current_token:)
        value_index = parse_expression(rhs_tokens, arena)
        if (value_index <= 0) return

        stmt_index = push_assignment(arena, target_index, value_index, id_token%line, &
                                     id_token%column, parent_index)
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
        character(len=256) :: message
        character(len=64) :: token_text
        integer :: i, msg_len

        message = "Unrecognized statement:"
        msg_len = len_trim(message)

        do i = 1, min(3, size(tokens))
            if (tokens(i)%kind == TK_EOF) exit
            if (len_trim(tokens(i)%text) == 0) cycle

            token_text = trim(tokens(i)%text)
            if (msg_len + len_trim(token_text) + 1 < len(message)) then
                message = message(1:msg_len) // " " // token_text
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
