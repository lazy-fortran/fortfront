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
    use frontend, only: lex_source, parse_tokens, analyze_semantics, &
                       emit_fortran, &
                       transform_lazy_fortran_string, &
                       transform_lazy_fortran_string_with_format, &
                       compilation_options_t, format_options_t, &
                       parse_tokens_safe, parse_result_with_index_t
    
    ! Include external interfaces to ensure they're compiled into the library
    use fortfront_c_interface, only: fortfront_initialize_c
    use fortfront_external_interface, only: fortfront_transform_source
    
    ! Re-export AST arena and core types
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                        binary_op_node, function_def_node, identifier_node, &
                        literal_node, array_literal_node, call_or_subscript_node, &
                        subroutine_def_node, subroutine_call_node, declaration_node, &
                        parameter_declaration_node, if_node, do_loop_node, &
                        do_while_node, select_case_node, case_block_node, &
                        module_node, use_statement_node, include_statement_node, &
                        print_statement_node, write_statement_node, &
                        read_statement_node, format_descriptor_node, &
                        allocate_statement_node, deallocate_statement_node, &
                        stop_node, return_node, cycle_node, exit_node, &
                        goto_node, error_stop_node, &
                        where_node, interface_block_node, derived_type_node, &
                        pointer_assignment_node, forall_node, case_range_node, &
                        case_default_node, complex_literal_node, &
                        comment_node, contains_node, implicit_statement_node, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX, &
                        create_ast_arena, ast_arena_stats_t, &
                        create_function_def, create_subroutine_def, &
                        is_procedure_node, get_procedure_name, get_procedure_params, &
                        get_procedure_body, procedure_has_return_type, get_procedure_return_type
    
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
    
    ! Re-export semantic analyzer functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use expression_temporary_tracker_module, only: temp_info_t
    
    ! Re-export lexer token type
    use lexer_core, only: token_t, tokenize_core
    
    ! Re-export type system
    use type_system_unified, only: mono_type_t, poly_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                             TFUN, TARRAY, TVAR, &
                             type_args_allocated, type_args_size, type_args_element
    
    ! Re-export scope management
    use scope_manager, only: scope_stack_t, SCOPE_GLOBAL, SCOPE_MODULE, &
                           SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK, &
                           SCOPE_INTERFACE
    
    ! Re-export AST introspection APIs for issue #12
    use ast_introspection, only: visit_node_at, get_node_type_id, has_semantic_info, &
                                get_node_source_location, &
                                get_node_type_kind, get_node_type_details, &
                                get_node_type_id_from_arena, get_node_source_location_from_arena
    
    ! Re-export AST traversal and visitor functionality
    use ast_traversal, only: traverse_ast_visitor => traverse_ast, &
                            traverse_preorder, traverse_postorder, &
                            is_program_node, is_assignment_node, is_binary_op_node, &
                            is_function_def_node, is_subroutine_def_node, &
                            is_identifier_node, is_literal_node, is_declaration_node, &
                            is_if_node, is_do_loop_node, is_do_while_node, &
                            is_call_or_subscript_node, is_subroutine_call_node, &
                            is_print_statement_node, is_use_statement_node, &
                            is_select_case_node, is_derived_type_node, &
                            is_module_node, is_interface_block_node
    
    ! Re-export visitor pattern
    use ast_visitor, only: ast_visitor_t, debug_visitor_t
    
    ! Re-export call graph analysis functionality
    use call_graph_module, only: call_graph_t, create_call_graph, &
                               procedure_info_t, call_edge_t, &
                               get_all_procedures, find_unused_procedures, &
                               get_callers, get_callees, find_recursive_cycles, &
                               cg_is_procedure_used => is_procedure_used
    use call_graph_builder_module, only: build_call_graph
    
    ! Re-export control flow graph functionality
    use control_flow_graph_module, only: control_flow_graph_t, basic_block_t, cfg_edge_t, &
                                       create_control_flow_graph, add_basic_block, add_cfg_edge, &
                                       find_reachable_blocks, find_unreachable_code, &
                                       get_entry_block, get_exit_blocks, get_all_blocks, &
                                       get_block_predecessors, get_block_successors, &
                                       is_block_reachable, get_unreachable_statements, &
                                       print_cfg, cfg_to_dot, &
                                       EDGE_UNCONDITIONAL, EDGE_TRUE_BRANCH, EDGE_FALSE_BRANCH, &
                                       EDGE_LOOP_BACK, EDGE_BREAK, EDGE_CONTINUE, &
                                       EDGE_RETURN, EDGE_EXCEPTION
    use cfg_builder_module, only: build_control_flow_graph
    
    ! Re-export control flow analyzer plugin for performance analysis (issue #194)
    use control_flow_analyzer_plugin, only: control_flow_analyzer_t
    
    ! Variable usage tracking for issue #16
    use variable_usage_tracker_module, only: variable_usage_info_t, expression_visitor_t, &
        create_variable_usage_info, get_variables_in_expression, &
        get_identifiers_in_subtree, visit_expression_nodes, &
        is_variable_used_in_expression, count_variable_usage
    
    ! Performance optimization for issue #15
    use ast_performance_module, only: ast_cache_entry_t, memory_stats_t, &
        cache_ast, load_cached_ast, clear_ast_cache, &
        is_cache_valid, get_cache_stats, &
        release_ast_memory, compact_arena, get_memory_stats, &
        update_ast_range, supports_incremental_update, &
        lock_arena, unlock_arena, is_arena_locked, &
        deep_copy_arena, deep_copy_semantic_context, compute_arena_hash
    
    ! Re-export intrinsic function registry (using renamed imports to avoid conflicts)
    use intrinsic_registry, only: registry_is_intrinsic => is_intrinsic_function, &
                                 registry_get_signature => get_intrinsic_signature, &
                                 get_intrinsic_info, &
                                 initialize_intrinsic_registry, &
                                 intrinsic_signature_t
    
    ! NEW: Extensible Semantic Pipeline (issue #202)
    ! use semantic_pipeline, only: semantic_pipeline_t, analyzer_ptr, create_pipeline
    use semantic_analyzer_base, only: semantic_analyzer_t
    ! use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
    !                              call_graph_analyzer_t, control_flow_analyzer_t, &
    !                              usage_tracker_analyzer_t, source_reconstruction_analyzer_t, &
    !                              interface_analyzer_t
    ! use semantic_pipeline_integration, only: analyze_semantics_with_pipeline, &
    !                                          create_default_semantic_pipeline
                                             
    ! Re-export utility functions from fortfront_utils
    use fortfront_utils, only: node_exists, get_node_type_at, get_node_location, &
                              get_parent, get_next_sibling, get_previous_sibling, &
                              get_block_statements, is_last_in_block, is_block_node, &
                              get_node_type, find_nodes_by_type, ast_to_json, &
                              get_arena_stats, analyze_program, get_type_for_node, &
                              get_diagnostics, semantic_info_to_json, get_max_depth, &
                              get_node_as_program, get_node_as_assignment, get_node_as_function_def, &
                              get_children, traverse_ast, traverse_node, get_node_range
    
    ! Re-export types from fortfront_types  
    use fortfront_types, only: symbol_info_t, symbol_reference_t, scope_info_t, &
                              expression_temp_info_t, source_location_t, source_range_t, &
                              type_info_t, diagnostic_t, function_signature_t, &
                              DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO, DIAGNOSTIC_HINT, &
                              NODE_PROGRAM, NODE_FUNCTION_DEF, NODE_ASSIGNMENT, NODE_BINARY_OP, &
                              NODE_IDENTIFIER, NODE_LITERAL, NODE_ARRAY_LITERAL, NODE_CALL_OR_SUBSCRIPT, &
                              NODE_SUBROUTINE_DEF, NODE_SUBROUTINE_CALL, NODE_DECLARATION, &
                              NODE_PARAMETER_DECLARATION, NODE_IF, NODE_DO_LOOP, NODE_DO_WHILE, &
                              NODE_SELECT_CASE, NODE_CASE_BLOCK, NODE_MODULE, NODE_USE_STATEMENT, &
                              NODE_PRINT_STATEMENT, NODE_WRITE_STATEMENT, NODE_READ_STATEMENT, &
                              NODE_ALLOCATE_STATEMENT, NODE_DEALLOCATE_STATEMENT, NODE_STOP, &
                              NODE_RETURN, NODE_GOTO, NODE_ERROR_STOP, NODE_CYCLE, NODE_EXIT, &
                              NODE_WHERE, NODE_INTERFACE_BLOCK, NODE_DERIVED_TYPE, &
                              NODE_POINTER_ASSIGNMENT, NODE_FORALL, NODE_CASE_RANGE, &
                              NODE_CASE_DEFAULT, NODE_COMPLEX_LITERAL, NODE_INCLUDE_STATEMENT, &
                              NODE_CONTAINS, NODE_FORMAT_DESCRIPTOR, NODE_COMMENT, &
                              NODE_IMPLICIT_STATEMENT, NODE_UNKNOWN
    
    ! Re-export advanced procedures from fortfront_advanced  
    ! NOTE: Most advanced functions temporarily disabled due to API incompatibility
    use fortfront_advanced, only: build_call_graph_from_arena, get_unused_procedures, &
                                 get_procedure_callers, get_procedure_callees, &
                                 is_procedure_used, get_all_procedures_in_graph, &
                                 get_call_edges, get_recursive_cycles, &
                                 build_cfg_from_arena, get_unreachable_code_from_cfg, &
                                 get_cfg_entry_block, get_cfg_exit_blocks, &
                                 get_cfg_all_blocks, get_cfg_block_predecessors, &
                                 get_cfg_block_successors, is_cfg_block_reachable, &
                                 get_cfg_unreachable_statements, print_control_flow_graph, &
                                 export_cfg_to_dot
    
    implicit none
    public

end module fortfront