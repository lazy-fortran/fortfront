module parser_expression_helpers_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_OPERATOR, TK_IDENTIFIER
    use ast_core
    use ast_nodes_core, only: component_access_node, identifier_node
    use ast_factory, only: push_literal, push_identifier, push_component_access, &
                           push_call_or_subscript, push_call_or_subscript_with_slice_detection
    use parser_state_module, only: parser_state_t
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
                LITERAL_INTEGER, current_token%line, current_token%column)
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

    ! Parse component access postfix operator (%)
    function parse_component_access_postfix(parser, arena, base_expr, op_token) result(expr_index)
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
        else
            ! Error: expected identifier after %
            expr_index = push_literal(arena, &
                "!ERROR: Expected identifier after %", &
                LITERAL_STRING, op_token%line, op_token%column)
        end if
    end function parse_component_access_postfix

end module parser_expression_helpers_module