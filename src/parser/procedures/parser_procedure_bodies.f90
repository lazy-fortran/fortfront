module parser_procedure_bodies_module
    ! Procedure body parsing for subroutines and functions in module contexts
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_subroutine_def, push_function_def, &
                           push_parameter_declaration, push_subroutine_call, &
                           push_assignment, push_identifier, push_literal, &
                           push_binary_op
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_call_module, only: parse_call_statement
    use parser_statement_data_module, only: parse_data_statement
    use ast_types, only: LITERAL_STRING, LITERAL_INTEGER
    use parser_legacy_statements_module, only: parse_legacy_statement
    use parser_prefix_buffer_module, only: append_prefix_token
    use parser_procedure_shared_module, only: consume_optional_return_type, &
                                              keyword_can_be_function_name
    use parser_do_constructs_module, only: parse_do_loop
    use parser_select_constructs_module, only: parse_select_case, parse_select_type, &
                                               parse_select_rank
    use parser_utilities, only: skip_to_end_of_line
    use parser_utils, only: analyze_declaration_structure
    use parser_io_statements_module, only: parse_print_statement
    use parser_control_statements_module, only: parse_entry_statement
    implicit none
    private

    public :: parse_subroutine_in_module, parse_function_in_module

contains

    pure logical function token_is_identifier_like(token) result(is_ident)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: token_lower

        is_ident = .false.

        if (token%kind == TK_IDENTIFIER) then
            is_ident = .true.
            return
        end if

        if (token%kind /= TK_KEYWORD) then
            return
        end if

        token_lower = to_lower(trim(token%text))

        select case (token_lower)
        case ("in", "out", "inout")
            is_ident = .true.
        case default
            is_ident = .false.
        end select
    end function token_is_identifier_like

    function try_parse_keyword_assignment(parser, arena, allow_keyword_tokens) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in), optional :: allow_keyword_tokens
        integer :: stmt_index
        type(token_t) :: token, assign_token
        integer :: lhs_index, rhs_index
        integer :: saved_token
        logical :: accept_keyword_tokens

        stmt_index = 0
        saved_token = parser%current_token
        accept_keyword_tokens = .false.
        if (present(allow_keyword_tokens)) then
            accept_keyword_tokens = allow_keyword_tokens
        end if

        token = parser%peek()
        if (accept_keyword_tokens) then
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
                return
            end if
        else
            if (.not. token_is_identifier_like(token)) then
                return
            end if
        end if

        lhs_index = push_identifier(arena, token%text, token%line, token%column)
        token = parser%consume()

        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "=")) then
            parser%current_token = saved_token
            return
        end if

        assign_token = token
        token = parser%consume()

        rhs_index = parse_simple_rhs_expression(parser, arena)
        if (rhs_index <= 0) then
            parser%current_token = saved_token
            return
        end if

        stmt_index = push_assignment(arena, lhs_index, rhs_index, &
                                     assign_token%line, assign_token%column)
    end function try_parse_keyword_assignment

    subroutine parse_prefix_keywords(parser, prefix_keywords, has_recursive_keyword)
        type(parser_state_t), intent(inout) :: parser
        character(len=16), allocatable, intent(out) :: prefix_keywords(:)
        logical, intent(out) :: has_recursive_keyword
        type(token_t) :: token

        has_recursive_keyword = .false.

        do
            token = parser%peek()
            if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
                select case (trim(to_lower(token%text)))
                case ("recursive")
                    has_recursive_keyword = .true.
                    call append_prefix_token(prefix_keywords, "recursive")
                    token = parser%consume()
                case ("pure")
                    call append_prefix_token(prefix_keywords, "pure")
                    token = parser%consume()
                case ("elemental")
                    call append_prefix_token(prefix_keywords, "elemental")
                    token = parser%consume()
                case default
                    exit
                end select
            else
                exit
            end if
        end do
    end subroutine parse_prefix_keywords

    ! Safe subroutine parsing for module contexts (avoids circular dependencies)
    recursive function parse_subroutine_in_module(parser, arena) result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: sub_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer :: line, column, stmt_index_local
        integer, allocatable :: param_indices(:), body_indices(:)
        character(len=16), allocatable :: prefix_keywords(:)
        logical :: has_recursive_keyword

        call parse_prefix_keywords(parser, prefix_keywords, has_recursive_keyword)

        ! Consume subroutine keyword
        token = parser%peek()
        if (.not. (token%kind == TK_KEYWORD .and. &
                   to_lower(token%text) == "subroutine")) then
            sub_index = 0
            return
        end if
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
        else
            subroutine_name = "unnamed_subroutine"
        end if

        ! Parse parameters (simplified - no type info)
        call parse_simple_parameter_list(parser, arena, param_indices)

        ! Parse subroutine body until end subroutine (simplified)
        allocate (body_indices(0))
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of subroutine
            if (token%kind == TK_KEYWORD .and. &
                to_lower(token%text) == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        to_lower(parser%tokens(parser%current_token + 1)%text) == &
                        "subroutine") then
                        if (parser%current_token + 2 <= size(parser%tokens) .and. &
                            parser%tokens(parser%current_token + 2)%kind == &
                            TK_IDENTIFIER) then
                            if (to_lower(parser%tokens(parser%current_token + 2)%text) &
                                == to_lower(subroutine_name)) then
                                token = parser%consume()  ! consume end
                                token = parser%consume()  ! consume subroutine
                                token = parser%consume()  ! consume subroutine name
                                exit
                            else
                                ! Nested procedure end; keep tokens for nested parser
                            end if
                        else
                            token = parser%consume()  ! consume end
                            token = parser%consume()  ! consume subroutine
                            exit
                        end if
                    end if
                end if
            end if

            call append_body_item(parser, arena, token, body_indices)
        end do

        ! Create subroutine node
        if (allocated(prefix_keywords)) then
            if (size(prefix_keywords) == 0) then
                block
                    character(len=16), allocatable :: temp(:)
                    call move_alloc(prefix_keywords, temp)
                end block
            end if
        end if

        if (allocated(prefix_keywords)) then
            sub_index = push_subroutine_def(arena, subroutine_name, &
                                            param_indices, body_indices, &
                                            line, column, &
                                            is_recursive=has_recursive_keyword, &
                                            prefix_keywords=prefix_keywords)
        else
            sub_index = push_subroutine_def(arena, subroutine_name, param_indices, &
                                            body_indices, line, column, &
                                            is_recursive=has_recursive_keyword)
        end if
    end function parse_subroutine_in_module

    ! Safe function parsing for module contexts (avoids circular dependencies)
    recursive function parse_function_in_module(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index
        type(token_t) :: token
        character(len=:), allocatable :: function_name, return_type_str
        character(len=:), allocatable :: result_variable_name
        integer :: line, column, stmt_index_local
        integer, allocatable :: param_indices(:), body_indices(:)
        character(len=16), allocatable :: prefix_keywords(:)
        logical :: has_recursive_keyword

        ! Initialize
        return_type_str = ""

        ! Optional prefix keywords before "function"
        call parse_prefix_keywords(parser, prefix_keywords, has_recursive_keyword)

        ! Check if we have a return type before "function"
        call consume_optional_return_type(parser, return_type_str)

        ! Consume function keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            func_index = 0
            return
        end if

        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            function_name = token%text
            token = parser%consume()
        else if (token%kind == TK_KEYWORD .and. &
                 keyword_can_be_function_name(parser, token)) then
            function_name = token%text
            token = parser%consume()
        else
            function_name = "unnamed_function"
        end if

        ! Parse parameters (simplified - no type info)
        call parse_simple_parameter_list(parser, arena, param_indices)

        ! Parse result clause if present
        result_variable_name = ""
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                    result_variable_name = token%text
                    token = parser%consume()
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        end if

        ! Parse function body until "end function" (simplified)
        allocate (body_indices(0))
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of function
            if (token%kind == TK_KEYWORD .and. &
                to_lower(token%text) == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        to_lower(parser%tokens(parser%current_token + 1)%text) == &
                        "function") then
                        if (parser%current_token + 2 <= size(parser%tokens) .and. &
                            parser%tokens(parser%current_token + 2)%kind == &
                            TK_IDENTIFIER) then
                            if (to_lower(parser%tokens(parser%current_token + 2)%text) &
                                == to_lower(function_name)) then
                                token = parser%consume()  ! consume end
                                token = parser%consume()  ! consume function
                                token = parser%consume()  ! consume function name
                                exit
                            else
                                ! Nested procedure end; keep tokens for nested parser
                            end if
                        else
                            token = parser%consume()  ! consume end
                            token = parser%consume()  ! consume function
                            exit
                        end if
                    end if
                end if
            end if

            if (token%kind == TK_KEYWORD) then
                stmt_index_local = try_parse_keyword_assignment(parser, arena, &
                                                                .true.)
                if (stmt_index_local > 0) then
                    body_indices = [body_indices, stmt_index_local]
                    cycle
                end if
            else if (token_is_identifier_like(token)) then
                stmt_index_local = try_parse_keyword_assignment(parser, arena)
                if (stmt_index_local > 0) then
                    body_indices = [body_indices, stmt_index_local]
                    cycle
                end if
            end if

            call append_body_item(parser, arena, token, body_indices)
        end do

        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, &
                                       return_type_str, body_indices, &
                                       line, column, &
                                       result_variable=result_variable_name, &
                                       is_recursive=has_recursive_keyword, &
                                       prefix_keywords=prefix_keywords)
    end function parse_function_in_module

    ! Basic statement parsing for subroutine/function bodies (avoiding circular deps)
    function parse_basic_statement_in_subroutine(parser, arena, body_indices) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout), optional :: body_indices(:)
        integer :: stmt_index
        type(token_t) :: token

        ! Check first token to determine statement type
        token = parser%peek()
        stmt_index = 0

        select case (token%kind)
        case (TK_KEYWORD)
            stmt_index = try_parse_keyword_assignment(parser, arena, .true.)
            if (stmt_index > 0) then
                return
            end if
            select case (trim(to_lower(token%text)))
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("data")
                stmt_index = parse_data_statement(parser, arena)
            case ("integer", "real", "logical", "character", "complex", &
                  "double", "procedure", "class", "type")
                ! Check if this is a multi-variable declaration
                block
                    logical :: has_initializer, has_comma
                    integer, allocatable :: decl_indices(:)
                    call analyze_declaration_structure(parser, has_initializer, &
                                                       has_comma)
                    if (has_comma) then
                        decl_indices = parse_multi_declaration(parser, arena)
                        if (allocated(decl_indices) .and. size(decl_indices) > 0) then
                            stmt_index = decl_indices(1)
                            if (size(decl_indices) > 1 .and. present(body_indices)) then
                                call append_multi_declarations(body_indices, &
                                                               decl_indices)
                            end if
                        else
                            stmt_index = 0
                        end if
                    else
                        stmt_index = parse_declaration(parser, arena)
                    end if
                end block
            case ("call")
                stmt_index = parse_call_statement(parser, arena)
            case ("contains")
                ! Handle contains section with nested procedures
                token = parser%consume()  ! consume 'contains'
                stmt_index = 0  ! contains itself is not a statement, just a marker
                ! The nested procedures will be parsed in subsequent iterations
            case ("subroutine")
                ! Handle nested subroutine definitions
                stmt_index = parse_subroutine_in_module(parser, arena)
            case ("function")
                ! Handle nested function definitions
                stmt_index = parse_function_in_module(parser, arena)
            case ("do")
                ! Handle DO loops
                stmt_index = parse_do_loop(parser, arena)
            case ("equivalence", "common")
                ! Handle legacy statements
                stmt_index = parse_legacy_statement(trim(to_lower(token%text)), &
                                                    parser, arena)
            case ("entry")
                ! Handle ENTRY statements
                stmt_index = parse_entry_statement(parser, arena)
            case default
                token = parser%consume()
                stmt_index = 0
            end select
        case (TK_IDENTIFIER)
            ! Likely assignment statement
            stmt_index = parse_simple_assignment_statement(parser, arena)
        case default
            ! Unknown token - consume it
            token = parser%consume()
            stmt_index = 0
        end select
    end function parse_basic_statement_in_subroutine

    subroutine append_body_item(parser, arena, token, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer, allocatable, intent(inout) :: body_indices(:)
        integer :: nested_index, stmt_index
        type(token_t) :: consumed_token
        type(token_t) :: lookahead

        if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "subroutine") then
            nested_index = parse_subroutine_in_module(parser, arena)
            if (nested_index > 0) then
                body_indices = [body_indices, nested_index]
            end if
        else if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "function") then
            nested_index = parse_function_in_module(parser, arena)
            if (nested_index > 0) then
                body_indices = [body_indices, nested_index]
            end if
        else if (token%kind == TK_KEYWORD .and. token%text == "do") then
            ! Handle DO loops directly
            stmt_index = parse_do_loop(parser, arena)
            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
            end if
        else if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "select") then
            ! Handle SELECT constructs (case/type/rank)
            ! Peek ahead to determine which select construct
            if (parser%current_token + 1 <= size(parser%tokens)) then
                lookahead = parser%tokens(parser%current_token + 1)
                if (lookahead%kind == TK_KEYWORD) then
                    if (to_lower(lookahead%text) == "case") then
                        stmt_index = parse_select_case(parser, arena)
                    else if (to_lower(lookahead%text) == "type") then
                        stmt_index = parse_select_type(parser, arena)
                    else if (to_lower(lookahead%text) == "rank") then
                        stmt_index = parse_select_rank(parser, arena)
                    else
                        stmt_index = 0
                    end if
                else
                    stmt_index = 0
                end if
            else
                stmt_index = 0
            end if
            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
            else
                ! If parsing failed, consume the select token to avoid infinite loop
                consumed_token = parser%consume()
            end if
        else if (token%kind /= TK_NEWLINE) then
            if (token%kind == TK_KEYWORD) then
                stmt_index = try_parse_keyword_assignment(parser, arena, .true.)
            else if (token_is_identifier_like(token)) then
                stmt_index = try_parse_keyword_assignment(parser, arena)
            else
                stmt_index = 0
            end if

            if (stmt_index == 0) then
                stmt_index = parse_basic_statement_in_subroutine(parser, arena, &
                                                                 body_indices)
            end if
            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]
            end if
        else
            consumed_token = parser%consume()
        end if
    end subroutine append_body_item

    subroutine append_multi_declarations(body_indices, decl_indices)
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, allocatable, intent(in) :: decl_indices(:)

        if (.not. allocated(decl_indices)) return
        if (size(decl_indices) <= 1) return

        body_indices = [body_indices, decl_indices(2:)]
    end subroutine append_multi_declarations

    ! Simple assignment statement parser for subroutine bodies
    function parse_simple_assignment_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        integer :: lhs_index, rhs_index

        stmt_index = 0

        ! Parse left-hand side (identifier)
        token = parser%peek()
        if (token_is_identifier_like(token)) then
            lhs_index = push_identifier(arena, token%text, token%line, token%column)
            token = parser%consume()

            ! Expect assignment operator
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='

                ! Parse right-hand side (expression)
                rhs_index = parse_simple_rhs_expression(parser, arena)

                if (rhs_index <= 0) then
                    ! Failed to parse expression
                    return
                end if

                ! Create assignment node
                stmt_index = push_assignment(arena, lhs_index, rhs_index, &
                                             token%line, token%column)
            else
                ! No assignment operator
                stmt_index = 0
            end if
        else
            ! No identifier
            stmt_index = 0
        end if
    end function parse_simple_assignment_statement

    ! Parse simple right-hand side expression (handles binary operations)
    subroutine parse_simple_parameter_list(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)
        type(token_t) :: token
        integer, allocatable :: local_indices(:)

        allocate (local_indices(0))

        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "(")) then
            call move_alloc(local_indices, param_indices)
            return
        end if

        token = parser%consume()

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            if (token%kind == TK_IDENTIFIER) then
                block
                    integer :: param_index
                    block
                        integer, allocatable :: empty_dims(:)
                        allocate (empty_dims(0))
                        param_index = push_parameter_declaration( &
                                      arena, name=token%text, type_name="", &
                                      kind_value=0, intent_value=0, &
                                      is_optional=.false., is_target=.false., &
                                      is_unsigned=.false., &
                                      dimension_indices=empty_dims, &
                                      line=token%line, column=token%column)
                    end block
                    local_indices = [local_indices, param_index]
                end block
                token = parser%consume()
            else
                token = parser%consume()
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            end if
        end do

        call move_alloc(local_indices, param_indices)
    end subroutine parse_simple_parameter_list

    function parse_simple_rhs_expression(parser, arena) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index
        type(token_t) :: token
        integer :: left_index, right_index
        character(len=:), allocatable :: op_text

        expr_index = 0

        ! Parse first operand
        token = parser%peek()
        if (token%kind == TK_STRING) then
            left_index = push_literal(arena, token%text, LITERAL_STRING, &
                                      token%line, token%column)
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            left_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                      token%line, token%column)
            token = parser%consume()
        else if (token_is_identifier_like(token)) then
            left_index = push_identifier(arena, token%text, &
                                         token%line, token%column)
            token = parser%consume()
        else
            return  ! Invalid expression
        end if

        ! Check for binary operator
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. &
            (token%text == "+" .or. token%text == "-" .or. &
             token%text == "*" .or. token%text == "/" .or. &
             token%text == "**")) then
            op_text = token%text
            token = parser%consume()  ! consume operator

            ! Parse second operand
            token = parser%peek()
            if (token%kind == TK_STRING) then
                right_index = push_literal(arena, token%text, LITERAL_STRING, &
                                           token%line, token%column)
                token = parser%consume()
            else if (token%kind == TK_NUMBER) then
                right_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                           token%line, token%column)
                token = parser%consume()
            else if (token_is_identifier_like(token)) then
                right_index = push_identifier(arena, token%text, &
                                              token%line, token%column)
                token = parser%consume()
            else
                expr_index = left_index  ! Return just the left operand
                return
            end if

            ! Create binary operation node
            expr_index = push_binary_op(arena, left_index, right_index, op_text, &
                                        token%line, token%column)
        else
            ! No operator, just return the single operand
            expr_index = left_index
        end if
    end function parse_simple_rhs_expression

end module parser_procedure_bodies_module
