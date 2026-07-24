module fortfront_ast
    use ast_arena_modern, only: ast_arena_t, create_ast_arena, &
        ast_arena_stats_t
    use ast_arena_source_text, only: set_source_text, has_source_text, &
        get_source_text, get_source_line, &
        get_source_range, get_source_range_by_pos
    use ast_base, only: ast_node, LITERAL_INTEGER, LITERAL_REAL, &
        LITERAL_STRING, LITERAL_LOGICAL, LITERAL_ARRAY, &
        LITERAL_COMPLEX
    use ast_nodes_core, only: program_node, assignment_node, binary_op_node, &
        identifier_node, literal_node, &
        array_literal_node, call_or_subscript_node, &
        pointer_assignment_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
        subroutine_call_node, is_procedure_node, &
        create_function_def, create_subroutine_def, &
        get_procedure_name, get_procedure_params, &
        get_procedure_body, &
        procedure_has_return_type, &
        get_procedure_return_type
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
        module_node, derived_type_node, &
        intent_type_to_string, INTENT_NONE, INTENT_IN, &
        INTENT_OUT, INTENT_INOUT
    use ast_nodes_control, only: if_node, select_case_node, case_block_node, &
        case_range_node, case_default_node, &
        where_node, cycle_node, exit_node, goto_node, &
        error_stop_node, stop_node, return_node, &
        continue_node
    use ast_nodes_loops, only: do_loop_node, do_while_node, forall_node
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
        read_statement_node, format_descriptor_node
    use ast_nodes_misc, only: use_statement_node, include_statement_node, &
        allocate_statement_node, deallocate_statement_node, &
        comment_node, directive_node, contains_node, &
        implicit_statement_node, interface_block_node, &
        complex_literal_node
    use ast_introspection, only: visit_node_at, get_node_type_id, &
        has_semantic_info, get_node_source_location, &
        get_node_type_kind, get_node_type_details, &
        get_node_type_id_from_arena, &
        get_node_source_location_from_arena
    use ast_traversal, only: traverse_ast_visitor => traverse_ast, &
        traverse_preorder, traverse_postorder, &
        is_program_node, is_assignment_node, &
        is_binary_op_node, is_function_def_node, &
        is_subroutine_def_node, is_identifier_node, &
        is_literal_node, is_declaration_node, &
        is_if_node, is_do_loop_node, is_do_while_node, &
        is_call_or_subscript_node, &
        is_subroutine_call_node, is_print_statement_node, &
        is_use_statement_node, is_select_case_node, &
        is_derived_type_node, is_module_node, &
        is_interface_block_node
    use ast_visitor, only: ast_visitor_t, debug_visitor_t
    use fortfront_utils, only: node_exists, get_node_type_at, &
        get_node_location, get_node_line, &
        get_node_column, get_parent, get_next_sibling, &
        get_previous_sibling, get_block_statements, &
        is_last_in_block, is_block_node, get_node_type, &
        find_nodes_by_type, ast_to_json, get_arena_stats, &
        analyze_program, get_type_for_node, &
        get_diagnostics, semantic_info_to_json, &
        get_max_depth, get_node_as_program, &
        get_node_as_assignment, get_node_as_function_def, &
        get_children, traverse_ast, traverse_node, &
        get_node_range
    use fortfront_node_constants
    use cst_nodes, only: trivia_t, CST_COMMENT, CST_WHITESPACE, CST_NEWLINE
    use cst_trivia_query, only: get_source_trivia_at, &
        get_trivia_for_ast_node, &
        get_trivia_for_ast_node_tokens
    implicit none
    public
end module fortfront_ast
