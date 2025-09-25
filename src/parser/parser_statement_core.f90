module parser_statement_core_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD, &
                          TK_NEWLINE, to_lower
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
              null_statement_callbacks

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

    function parse_basic_statement_core(tokens, arena, parent_index, callbacks) &
            result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in), optional :: callbacks
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
        if (handled) return

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
