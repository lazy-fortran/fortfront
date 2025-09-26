module parser_statement_core_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
                          TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    use ast_types, only: LITERAL_STRING
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression
    use parser_io_statements_module, only: parse_print_statement, &
                                           parse_write_statement, parse_read_statement
    use parser_control_statements_module, only: &
        parse_cycle_statement, parse_exit_statement, parse_return_statement, &
        parse_stop_statement, parse_goto_statement, parse_error_stop_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_call_module, only: parse_call_statement
    use parser_utils, only: analyze_declaration_structure
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_assignment, push_identifier, push_literal
    implicit none
    private

    public :: statement_callbacks_t, parse_basic_statement_core, &
              null_statement_callbacks, find_statement_end

    abstract interface
        function parse_with_parent_interface(parser, arena, parent_index) &
                result(node_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in), optional :: parent_index
            integer :: node_index
        end function parse_with_parent_interface

        function parse_without_parent_interface(parser, arena) result(node_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer :: node_index
        end function parse_without_parent_interface
    end interface

    type :: statement_callbacks_t
        procedure(parse_with_parent_interface), pointer, nopass :: parse_if => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_do_loop => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_select_case => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_where => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_forall => null()
        procedure(parse_without_parent_interface), pointer, nopass :: &
            parse_associate => null()
    end type statement_callbacks_t

contains

    pure function null_statement_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks
    end function null_statement_callbacks

    logical function is_block_if(tokens, start_index) result(is_block)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer :: i

        is_block = .false.
        do i = start_index + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_KEYWORD)
                if (tokens(i)%text == "then") then
                    is_block = .true.
                    return
                else
                    return
                end if
            case (TK_OPERATOR)
                if (tokens(i)%text == ";") return
            case (TK_NEWLINE, TK_EOF)
                return
            case (TK_COMMENT, TK_WHITESPACE)
                cycle
            case default
                cycle
            end select
        end do
    end function is_block_if

    pure logical function at_top_level(if_depth, select_depth, do_depth, &
            where_depth, assoc_depth, forall_depth) result(is_top_level)
        integer, intent(in) :: if_depth, select_depth, do_depth
        integer, intent(in) :: where_depth, assoc_depth, forall_depth

        is_top_level = (if_depth == 0 .and. select_depth == 0 .and. &
            do_depth == 0 .and. where_depth == 0 .and. &
            assoc_depth == 0 .and. forall_depth == 0)
    end function at_top_level

    integer function find_statement_end(tokens, start_index) result(end_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index

        integer :: idx
        integer :: if_depth, select_depth, do_depth, where_depth, assoc_depth
        integer :: forall_depth
        logical :: first_processed, block_if
        character(len=16) :: first_keyword
        type(token_t) :: token, next_token

        end_index = start_index
        if (start_index > size(tokens)) return

        if_depth = 0
        select_depth = 0
        do_depth = 0
        where_depth = 0
        assoc_depth = 0
        forall_depth = 0
        first_processed = .false.
        block_if = .false.
        first_keyword = ""

        idx = start_index
        do while (idx <= size(tokens))
            token = tokens(idx)

            select case (token%kind)
            case (TK_EOF)
                end_index = idx - 1
                exit
            case (TK_NEWLINE)
                        if (at_top_level( &
                            if_depth, select_depth, do_depth, where_depth, &
                            assoc_depth, forall_depth)) then
                    end_index = idx - 1
                    exit
                end if
            case (TK_OPERATOR)
                if (token%text == ";") then
                        if (at_top_level( &
                            if_depth, select_depth, do_depth, where_depth, &
                            assoc_depth, forall_depth)) then
                        end_index = idx - 1
                        exit
                    end if
                end if
            case (TK_COMMENT, TK_WHITESPACE)
                ! Ignore spacing tokens
            case (TK_KEYWORD)
                select case (token%text)
                case ("if")
                    if (.not. first_processed) then
                        first_processed = .true.
                        block_if = is_block_if(tokens, idx)
                        if (block_if) if_depth = if_depth + 1
                        first_keyword = "if"
                    else
                        if (is_block_if(tokens, idx)) then
                            if_depth = if_depth + 1
                        end if
                    end if
                case ("select")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "select"
                    end if
                    select_depth = select_depth + 1
                case ("do")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "do"
                    end if
                    do_depth = do_depth + 1
                case ("where")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "where"
                    end if
                    where_depth = where_depth + 1
                case ("forall")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "forall"
                    end if
                    forall_depth = forall_depth + 1
                case ("associate")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "associate"
                    end if
                    assoc_depth = assoc_depth + 1
               case ("else")
                   if (block_if) then
                       if (idx + 1 <= size(tokens)) then
                           if (tokens(idx + 1)%kind == TK_KEYWORD .and. &
                               tokens(idx + 1)%text == "if") then
                               if (if_depth == 1) then
                                   end_index = idx - 1
                                   exit
                               end if
                           else
                               if (if_depth == 1) then
                                   end_index = idx - 1
                                   exit
                               end if
                           end if
                       else
                           if (if_depth == 1) then
                               end_index = idx - 1
                               exit
                           end if
                       end if
                    else if (at_top_level( &
                             if_depth, select_depth, do_depth, where_depth, &
                             assoc_depth, forall_depth)) then
                        end_index = idx - 1
                        exit
                    end if
                case ("elseif", "else if")
                    if (block_if .and. if_depth == 1) then
                        end_index = idx - 1
                        exit
                    else if (at_top_level( &
                             if_depth, select_depth, do_depth, where_depth, &
                             assoc_depth, forall_depth)) then
                        end_index = idx - 1
                        exit
                    end if
                case ("endif")
                    if (if_depth > 0) then
                        if_depth = if_depth - 1
                        if (if_depth == 0 .and. block_if) then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endselect")
                    if (select_depth > 0) then
                        select_depth = select_depth - 1
                        if (select_depth == 0 .and. first_keyword == "select") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("enddo")
                    if (do_depth > 0) then
                        do_depth = do_depth - 1
                        if (do_depth == 0 .and. first_keyword == "do") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endwhere")
                    if (where_depth > 0) then
                        where_depth = where_depth - 1
                        if (where_depth == 0 .and. first_keyword == "where") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endforall")
                    if (forall_depth > 0) then
                        forall_depth = forall_depth - 1
                        if (forall_depth == 0 .and. first_keyword == "forall") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endassociate")
                    if (assoc_depth > 0) then
                        assoc_depth = assoc_depth - 1
                        if (assoc_depth == 0 .and. first_keyword == "associate") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("end")
                    if (idx + 1 <= size(tokens)) then
                        if (tokens(idx + 1)%kind == TK_KEYWORD) then
                            next_token = tokens(idx + 1)
                            select case (next_token%text)
                            case ("if")
                                if (if_depth > 0) then
                                    if_depth = if_depth - 1
                                    if (if_depth == 0 .and. block_if) then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("select")
                                if (select_depth > 0) then
                                    select_depth = select_depth - 1
                                    if (select_depth == 0 .and. &
                                        first_keyword == "select") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("do")
                                if (do_depth > 0) then
                                    do_depth = do_depth - 1
                                    if (do_depth == 0 .and. first_keyword == "do") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("associate")
                                if (assoc_depth > 0) then
                                    assoc_depth = assoc_depth - 1
                                    if (assoc_depth == 0 .and. &
                                        first_keyword == "associate") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("where")
                                if (where_depth > 0) then
                                    where_depth = where_depth - 1
                                    if (where_depth == 0 .and. &
                                        first_keyword == "where") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("forall")
                                if (forall_depth > 0) then
                                    forall_depth = forall_depth - 1
                                    if (forall_depth == 0 .and. &
                                        first_keyword == "forall") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case default
                                if (at_top_level( &
                                    if_depth, select_depth, do_depth, where_depth, &
                                    assoc_depth, forall_depth)) then
                                    end_index = idx - 1
                                    exit
                                end if
                            end select
                        else
                            if (at_top_level( &
                                if_depth, select_depth, do_depth, where_depth, &
                                assoc_depth, forall_depth)) then
                                end_index = idx - 1
                                exit
                            end if
                        end if
                    else
                        if (at_top_level( &
                            if_depth, select_depth, do_depth, where_depth, &
                            assoc_depth, forall_depth)) then
                            end_index = idx - 1
                            exit
                        end if
                    end if
                case default
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = to_lower(token%text)
                    end if
                end select
            case default
                if (.not. first_processed) then
                    first_processed = .true.
                end if
            end select

            end_index = idx
            idx = idx + 1
        end do
    end function find_statement_end

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
        select case (first_token%kind)
        case (TK_KEYWORD)
            stmt_index = parse_keyword_statement(first_token, parser, arena, &
                                                 parent_index, local_callbacks)
        case (TK_IDENTIFIER)
            stmt_index = parse_identifier_statement(parser, arena, parent_index, tokens)
        end select

        if (stmt_index == 0) then
            stmt_index = build_placeholder_or_zero(tokens, first_token, arena)
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
                   text == "character")
    end function is_declaration_keyword

    integer function parse_keyword_statement(first_token, parser, arena, parent_index, &
                                             callbacks) result(stmt_index)
        type(token_t), intent(in) :: first_token
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks

        stmt_index = 0
        select case (first_token%text)
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
        case ("select")
            if (associated(callbacks%parse_select_case)) then
                stmt_index = callbacks%parse_select_case(parser, arena)
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
        case ("go")
            stmt_index = parse_goto_statement(parser, arena)
        case ("error")
            stmt_index = parse_error_stop_statement(parser, arena)
        end select
    end function parse_keyword_statement

    integer function parse_identifier_statement(parser, arena, parent_index, tokens) &
            result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t), intent(in) :: tokens(:)
        type(token_t) :: id_token, op_token

        stmt_index = 0
        id_token = parser%consume()
        op_token = parser%peek()

        if (op_token%kind == TK_OPERATOR .and. op_token%text == "(") then
            stmt_index = parse_complex_assignment(parser, arena, parent_index, tokens, &
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
        if (value_index <= 0) then
            deallocate (expr_tokens)
            return
        end if

        if (present(parent_index)) then
            target_index = push_identifier(arena, id_token%text, id_token%line, &
                                           id_token%column, parent_index)
        else
            target_index = push_identifier(arena, id_token%text, id_token%line, &
                                           id_token%column)
        end if

        stmt_index = push_assignment(arena, target_index, value_index, id_token%line, &
                                     id_token%column, parent_index)
        deallocate (expr_tokens)
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
        if (target_index <= 0) then
            deallocate (lhs_tokens)
            return
        end if
        deallocate (lhs_tokens)

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
        deallocate (rhs_tokens)
        if (value_index <= 0) return

        stmt_index = push_assignment(arena, target_index, value_index, id_token%line, &
                                     id_token%column, parent_index)
    end function parse_complex_assignment

    integer function build_placeholder_or_zero(tokens, first_token, arena) &
            result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(token_t), intent(in) :: first_token
        type(ast_arena_t), intent(inout) :: arena
        logical :: is_terminator
        character(len=:), allocatable :: lowered
        character(len=256) :: debug_msg
        character(len=64) :: token_text
        integer :: debug_len, i

        stmt_index = 0
        is_terminator = .false.
        if (first_token%kind == TK_KEYWORD) then
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
        end if

        if (is_terminator) return

        debug_msg = "! Unparsed: "
        debug_len = len_trim(debug_msg)
        do i = 1, min(3, size(tokens))
            if (tokens(i)%kind == TK_EOF) exit
            if (len_trim(tokens(i)%text) > 0) then
                token_text = trim(tokens(i)%text)
                if (debug_len + len_trim(token_text) + 1 < 250) then
                    debug_msg = debug_msg(1:debug_len)//" "//trim(token_text)
                    debug_len = len_trim(debug_msg)
                end if
            end if
        end do

        stmt_index = push_literal(arena, trim(debug_msg), LITERAL_STRING, &
                                  first_token%line, first_token%column)
    end function build_placeholder_or_zero

end module parser_statement_core_module
