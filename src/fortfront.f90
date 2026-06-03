module fortfront
    ! fortfront Public API - Facade module exposing all functionality for fluff
    ! This module provides a unified interface to all fortfront phases:
    ! - Lexical Analysis
    ! - AST Construction and Arena Management
    ! - Semantic Analysis with Type Inference
    ! - Code Generation
    !
    ! IMPORTANT: AST Node Access Policy
    ! =================================
    ! AST nodes MUST NOT be copied due to complex allocatable components
    ! that can cause memory corruption and segmentation faults.
    !
    ! USE ONLY the visitor pattern for safe node access:
    !   - visit_node_at() for visiting nodes by index
    !   - AST traversal functions with custom visitors
    !
    ! DO NOT attempt to:
    !   - Copy nodes with allocate(source=...)
    !   - Create functions that return node copies
    !   - Perform shallow copies of nodes
    !
    ! For read-only access to node properties, use:
    !   - get_node_type_id_from_arena()
    !   - get_node_source_location_from_arena()
    !   - get_node_type_kind()
    !   - get_node_type_details()

    ! Re-export core pipeline functionality
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens, parse_tokens_safe, &
                          parse_result_with_index_t
    use semantic_api, only: analyze_semantics
    use codegen_api, only: emit_fortran
    use transformation_api, only: transform_lazy_fortran_string, &
                                  transform_lazy_fortran_string_with_format, &
                                  compilation_options_t, format_options_t
    use frontend_tooling_api, only: tooling_parse_options_t, &
                                    tooling_load_ast_from_string, &
                                    tooling_load_ast_from_file
    use frontend_compiler_api, only: compiler_frontend_options_t, &
                                     compiler_frontend_result_t, &
                                     compile_frontend_from_string, &
                                     compile_frontend_from_file
    use frontend_compiler_queries, only: is_subroutine_call_statement, &
                                         get_subroutine_call_name, &
                                         get_subroutine_call_arg_indices, &
                                         is_binary_op, get_binary_op_info, &
                                         is_literal, get_literal_info, &
                                         is_identifier, get_identifier_name, &
                                         get_declaration_initializer, &
                                         get_derived_type_components, &
                                         get_array_literal_elements, &
                                         get_import_list, get_interface_block_body, &
                                         has_bind_c_attribute, get_bind_c_name, &
                                         get_select_case_info, get_case_block_info, &
                                         get_case_default_body, get_case_range_info, &
                                         get_select_type_info, get_type_guard_info, &
                                         get_dummy_allocatable_attribute, &
                                         get_program_body_info, &
                                         get_module_body_info, &
                                         get_function_body_info, &
                                         get_subroutine_body_info

    ! Include external interfaces to ensure they're compiled into the library
    use fortfront_c_interface, only: fortfront_initialize_c

    ! Re-export AST arena and core types
    use ast_arena_modern, only: ast_arena_t, create_ast_arena, ast_arena_stats_t
    use ast_arena_source_text, only: set_source_text, has_source_text, &
                                     get_source_text, get_source_line, &
                                     get_source_range, get_source_range_by_pos
    use ast_base, only: ast_node
    use ast_nodes_core, only: program_node, assignment_node, binary_op_node, &
                              identifier_node, literal_node, array_literal_node, &
                              call_or_subscript_node, pointer_assignment_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node, is_procedure_node, &
                                   create_function_def, create_subroutine_def, &
                                   get_procedure_name, get_procedure_params, &
                                   get_procedure_body, procedure_has_return_type, &
                                   get_procedure_return_type
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              module_node, derived_type_node
    use ast_nodes_control, only: if_node, select_case_node, case_block_node, &
                                 case_range_node, case_default_node, where_node, &
                                 cycle_node, exit_node, goto_node, error_stop_node, &
                                 stop_node, return_node, continue_node
    use ast_nodes_loops, only: do_loop_node, do_while_node, forall_node
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
                            read_statement_node, format_descriptor_node
    use ast_nodes_misc, only: use_statement_node, include_statement_node, &
                              allocate_statement_node, deallocate_statement_node, &
                              comment_node, directive_node, contains_node, &
                              implicit_statement_node, interface_block_node, &
                              complex_literal_node
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX

    ! Re-export AST node data utilities
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, INTENT_IN, &
                              INTENT_OUT, INTENT_INOUT

    ! Re-export CST core functionality (Issue #393)
    use cst_core, only: create_cst_node, create_trivia, get_node_kind_name, &
                        is_trivia_kind, validate_cst_node, validate_trivia, &
                        add_child_to_cst_node, set_cst_node_text, &
                        add_leading_trivia, add_trailing_trivia
    use cst_nodes, only: cst_node_t, trivia_t, CST_PROGRAM, CST_SUBROUTINE, &
                         CST_FUNCTION, CST_DECLARATION, CST_ASSIGNMENT, CST_CALL, &
                         CST_IDENTIFIER, CST_LITERAL, CST_OPERATOR, CST_COMMENT, &
                         CST_WHITESPACE, CST_NEWLINE
    use cst_arena, only: cst_arena_t, cst_handle_t, create_cst_arena
    use cst_trivia_query, only: get_cst_node_for_ast, get_leading_trivia, &
                                get_trailing_trivia, get_source_trivia_at, &
                                get_trivia_for_ast_node, &
                                get_trivia_for_ast_node_tokens

    ! Re-export semantic analyzer functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_INFER, OPERATING_MODE_STRICT

    ! Re-export lexer token type
    use lexer_core, only: token_t, trivia_token_t, tokenize_core, &
                          tokenize_core_with_trivia

    ! Re-export type system
    use type_system_unified, only: mono_type_t, poly_type_t, TINT, TREAL, TCHAR, &
                                   TLOGICAL, &
                                   TFUN, TARRAY, TVAR, &
                                   type_args_allocated, type_args_size, &
                                   type_args_element

    ! Re-export scope management
    use scope_manager, only: scope_stack_t, SCOPE_GLOBAL, SCOPE_MODULE, &
                             SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK, &
                             SCOPE_INTERFACE

    ! Re-export AST introspection APIs for issue #12
    use ast_introspection, only: visit_node_at, get_node_type_id, has_semantic_info, &
                                 get_node_source_location, &
                                 get_node_type_kind, get_node_type_details, &
                                 get_node_type_id_from_arena, &
                                 get_node_source_location_from_arena

    ! Re-export AST traversal and visitor functionality
    use ast_traversal, only: traverse_ast_visitor => traverse_ast, &
                             traverse_preorder, traverse_postorder, &
                             is_program_node, is_assignment_node, is_binary_op_node, &
                             is_function_def_node, is_subroutine_def_node, &
                             is_identifier_node, is_literal_node, &
                             is_declaration_node, &
                             is_if_node, is_do_loop_node, is_do_while_node, &
                             is_call_or_subscript_node, is_subroutine_call_node, &
                             is_print_statement_node, is_use_statement_node, &
                             is_select_case_node, is_derived_type_node, &
                             is_module_node, is_interface_block_node

    ! Re-export visitor pattern
    use ast_visitor, only: ast_visitor_t, debug_visitor_t

    ! Re-export call graph analysis functionality
    use call_graph_module, only: call_graph_t, create_call_graph, &
                                 procedure_info_t, call_edge_t, build_call_graph, &
                                 get_all_procedures, get_callers, get_callees, &
                                 get_call_count, &
                                 cg_is_procedure_used => is_procedure_used

    ! Variable usage tracking for issue #16
    use variable_usage_tracker_module, only: variable_usage_info_t, &
                                             expression_visitor_t, &
                                             create_variable_usage_info, &
                                             get_variables_in_expression, &
                                             get_identifiers_in_subtree, &
                                             visit_expression_nodes, &
                                             is_variable_used_in_expression, &
                                             count_variable_usage

    ! Re-export intrinsic function registry (using renamed imports to avoid conflicts)
    use intrinsic_registry, only: registry_is_intrinsic => is_intrinsic_function, &
                                  registry_get_signature => get_intrinsic_signature, &
                                  get_intrinsic_info, &
                                  initialize_intrinsic_registry, &
                                  intrinsic_signature_t

    ! NEW: Extensible Semantic Pipeline (issue #202)
    use semantic_analyzer_base, only: semantic_analyzer_t

 use frontend_compiler_queries, only: get_declaration_initializer, &
                                          get_derived_type_components, &
                                          get_array_literal_elements, &
                                          get_import_list, &
                                          get_interface_block_body, &
                                          has_bind_c_attribute, get_bind_c_name, &
                                          get_select_case_info, get_case_block_info, &
                                          get_case_default_body, get_case_range_info, &
                                          get_select_type_info, get_type_guard_info, &
                                          get_dummy_allocatable_attribute, &
                                          get_program_body_info, &
                                          get_module_body_info, &
                                          get_function_body_info, &
                                          get_subroutine_body_info

  ! Re-export utility functions from fortfront_utils
    use fortfront_utils, only: node_exists, get_node_type_at, get_node_location, &
                                get_node_line, get_node_column, &
                                get_parent, get_next_sibling, get_previous_sibling, &
                                get_block_statements, is_last_in_block, is_block_node, &
                                get_node_type, find_nodes_by_type, ast_to_json, &
                                get_arena_stats, analyze_program, get_type_for_node, &
                                get_diagnostics, semantic_info_to_json, get_max_depth, &
                                get_node_as_program, get_node_as_assignment, &
                                get_node_as_function_def, &
                                get_children, traverse_ast, traverse_node, &
                                get_node_range
    use fortfront_node_constants

    ! Re-export types from fortfront_types
    use fortfront_types, only: symbol_info_t, symbol_reference_t, scope_info_t, &
                               source_location_t, source_range_t, &
                               type_info_t, diagnostic_t, function_signature_t

    ! Call graph and control-flow utilities (lean re-export)
    use call_graph_module, only: build_call_graph_from_arena => build_call_graph, &
                                 call_graph_t, call_edge_t, &
                                 get_procedure_callers => get_callers, &
                                 get_procedure_callees => get_callees, &
                                 is_procedure_used, &
                                 get_all_procedures_in_graph => get_all_procedures

    ! Symbol table query API (issue #2613)
    use symbol_table_api, only: get_symbols_in_scope, get_all_symbols, &
                                is_symbol_defined, lookup_symbol, &
                                get_scope_info, get_current_scope_depth

    implicit none
    public
contains

    function get_call_edges(graph) result(edges)
        type(call_graph_t), intent(in) :: graph
        type(call_edge_t), allocatable :: edges(:)

        if (allocated(graph%calls) .and. graph%call_count > 0) then
            allocate (edges(graph%call_count))
            edges = graph%calls(1:graph%call_count)
        else
            allocate (edges(0))
        end if
    end function get_call_edges

end module fortfront
