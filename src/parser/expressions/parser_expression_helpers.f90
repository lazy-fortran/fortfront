module parser_expression_helpers_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, &
        TK_WHITESPACE, TK_NEWLINE, TK_COMMENT, TK_EOF, to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_types, only: LITERAL_REAL, LITERAL_INTEGER, LITERAL_STRING, LITERAL_LOGICAL
    use ast_factory, only: push_literal, push_component_access, &
        push_call_or_subscript, &
        push_call_or_subscript_with_slice_detection
    use parser_state_module, only: parser_state_t
    use parser_utilities, only: peek_next_nontrivial_token
    implicit none
    private

    public :: parse_number_literal, parse_string_literal
    public :: parse_boolean_literal, parse_component_access_postfix

contains

    ! Parse number literal tokens
    function parse_number_literal(current_token, arena) result(expr_index)
        type(token_t), intent(in) :: current_token
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        if (index(current_token%text, '.') > 0) then
            ! Contains decimal point - classify as real
            expr_index = push_literal(arena, current_token%text, LITERAL_REAL, &
                current_token%line, current_token%column)
        else
            ! No decimal point - classify as integer
            expr_index = push_literal(arena, current_token%text, &
                LITERAL_INTEGER, current_token%line, &
                current_token%column)
        end if
    end function parse_number_literal

    ! Parse string literal tokens
    function parse_string_literal(current_token, arena) result(expr_index)
        type(token_t), intent(in) :: current_token
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = push_literal(arena, current_token%text, LITERAL_STRING, &
            current_token%line, current_token%column)
    end function parse_string_literal

    ! Parse boolean literal tokens
    function parse_boolean_literal(current_token, arena) result(expr_index)
        type(token_t), intent(in) :: current_token
        type(ast_arena_t), intent(inout) :: arena
        integer :: expr_index

        expr_index = push_literal(arena, current_token%text, LITERAL_LOGICAL, &
            current_token%line, current_token%column)
    end function parse_boolean_literal

    ! Parse component access postfix operator (% or LFortran .)
    function parse_component_access_postfix(parser, arena, base_expr, op_token) &
            result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        type(token_t), intent(in) :: op_token
        integer :: expr_index
        type(token_t) :: component_token

        component_token = parser%peek()
        if (component_token%kind == TK_IDENTIFIER) then
            component_token = parser%consume()
            expr_index = push_component_access(arena, base_expr, &
                component_token%text, &
                op_token%line, op_token%column)
        else if (component_token%kind == TK_KEYWORD) then
            ! Keywords are valid component names in Fortran
            component_token = parser%consume()
            expr_index = push_component_access(arena, base_expr, &
                component_token%text, &
                op_token%line, op_token%column)
        else
            call parser%error("Expected identifier after component access operator")
            expr_index = base_expr
        end if
    end function parse_component_access_postfix

    logical function promote_keyword_component(parser, token) result(promoted)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(inout) :: token
        type(token_t) :: next_token
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: next_lower
        integer :: index

        promoted = .false.
        if (token%kind /= TK_KEYWORD) then
            return
        end if

        lowered = to_lower(trim(token%text))
        if (lowered /= "end") then
            return
        end if

        next_token = peek_next_nontrivial_token(parser)
        if (next_token%kind == TK_KEYWORD) then
            next_lower = to_lower(trim(next_token%text))
            select case (next_lower)
            case ("type", "module", "subroutine", "function", "program", &
                    "interface", "procedure", "select", "if", "do", "forall", &
                    "where", "associate", "block", "team", "critical", &
                    "blockdata")
                return
            end select
        end if

        if (.not. associated(parser%tokens)) then
            return
        end if

        index = parser%current_token - 1
        if (index < 1 .or. index > size(parser%tokens)) then
            return
        end if

        parser%tokens(index)%kind = TK_IDENTIFIER
        token%kind = TK_IDENTIFIER
        promoted = .true.
    end function promote_keyword_component

end module parser_expression_helpers_module
