module fortfront
    ! fortfront Public API - Facade module exposing all functionality for fluff
    ! This module provides a unified interface to all fortfront phases:
    ! - Lexical Analysis
    ! - AST Construction and Arena Management
    ! - Semantic Analysis with Type Inference
    ! - Code Generation
    !
    ! AST Node Access
    ! ===============
    ! The arena's assignment operator (=) performs a verified deep copy
    ! of all node types including nested allocatable components.
    !
    ! For read-only access to node properties, use:
    !   - visit_node_at() for visiting nodes by index
    !   - AST traversal functions with custom visitors
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
    use frontend_core, only: core_is_fixed_form_file => is_fixed_form_file, &
        core_normalize_fixed_form_source_text => normalize_fixed_form_source_text
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
        get_alternate_return_label, &
        get_construct_name, &
        get_return_selector, &
        is_alternate_return_dummy, &
        get_program_body_info, &
        get_module_body_info, &
        get_function_body_info, &
        get_subroutine_body_info, &
        get_used_modules, get_defined_module, &
        used_module_t, defined_module_t, &
        program_unit_query_t, declaration_query_t, &
        derived_type_query_t, type_binding_query_t, component_access_query_t, &
        array_bounds_query_t, range_expression_query_t, &
        use_statement_query_t, interface_query_t, visibility_query_t, &
        namelist_query_t, data_statement_query_t, common_block_query_t, &
        enum_query_t, statement_function_query_t, block_data_query_t, &
        query_program_units, query_program_unit, query_declarations, &
        query_declaration, query_derived_type, query_type_binding, &
        query_use_statement, query_use_statements, query_interface, &
        query_visibility, query_namelist, query_data_statement, &
        query_common_block, query_enum, query_statement_function, &
        query_component_access, query_array_bounds, query_range_expression, &
        query_block_data, STORAGE_LOCAL, STORAGE_OWNED, STORAGE_BORROWED, &
        STORAGE_POINTER, STORAGE_MODULE, STORAGE_SAVE, STORAGE_COMMON, &
        OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE, &
        OWNERSHIP_EVENT_POINTER_ASSIGN, OWNERSHIP_EVENT_MOVE_ALLOC, &
        OWNERSHIP_EVENT_NULLIFY, OWNERSHIP_EVENT_ASSIGNMENT, &
        OWNERSHIP_ASSIGNMENT_NONE, OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE, &
        OWNERSHIP_ASSIGNMENT_DEEP_DERIVED, &
        OWNERSHIP_REALLOCATION_NONE, OWNERSHIP_REALLOCATION_POTENTIAL, &
        ACCESS_READ, ACCESS_WRITE, ACCESS_READ_WRITE, storage_query_t, &
        ownership_event_query_t, component_path_query_t, &
        associate_selector_query_t, query_associate_selector, &
        query_associate_selectors, &
        polymorphic_allocation_query_t, &
        POLYMORPHIC_SOURCE_UNKNOWN, POLYMORPHIC_SOURCE_CONCRETE, &
        POLYMORPHIC_SOURCE_POLYMORPHIC, &
        binding_resolution_query_t, global_reference_query_t, query_storage, &
        binding_hierarchy_entry_t, binding_hierarchy_query_t, &
        query_ownership_events, query_component_path, &
        query_polymorphic_allocation, &
        query_type_binding_resolution, query_active_global_references, &
        query_type_binding_hierarchy, &
        type_bound_call_query_t, query_type_bound_call, &
        procedure_target_query_t, query_procedure_target, &
        procedure_callback_target_query_t, procedure_callback_flow_query_t, &
        query_procedure_callback_flow, query_procedure_pointer_callback_flow, &
        procedure_call_target_query_t, query_procedure_call_target, &
        procedure_dummy_query_t, procedure_signature_query_t, &
        call_argument_query_t, call_arguments_query_t, query_call_arguments, &
        procedure_actual_argument_query_t, query_procedure_actual_argument, &
        generic_argument_query_t, generic_candidate_query_t, generic_call_query_t, &
        query_generic_call
    use frontend_compiler_control_queries, only: control_statement_query_t, &
        select_rank_arm_query_t, select_type_arm_query_t, &
        query_control_statement, CONTROL_SELECT_RANK, CONTROL_SELECT_TYPE, &
        CONTROL_TYPE_GUARD, &
        SELECT_TYPE_ARM_TYPE_IS, SELECT_TYPE_ARM_CLASS_IS, &
        SELECT_TYPE_ARM_CLASS_DEFAULT, &
        SELECT_RANK_DISPATCH_EXPLICIT, SELECT_RANK_DISPATCH_ASSUMED_SIZE, &
        SELECT_RANK_DISPATCH_DEFAULT
    use frontend_compiler_select_type_queries, only: &
        select_type_branch_query_t, query_select_type_branch, &
        SELECT_TYPE_MATCH_UNKNOWN, SELECT_TYPE_MATCH_EXACT, &
        SELECT_TYPE_MATCH_EXTENSION, SELECT_TYPE_MATCH_DEFAULT, &
        select_type_component_query_t, query_select_type_component_path, &
        select_type_component_binding_query_t, &
        query_select_type_component_binding, &
        select_type_dispatch_query_t, query_select_type_dispatch, &
        select_type_generic_candidate_query_t, &
        select_type_generic_dispatch_query_t, &
        query_select_type_generic_dispatch, &
        select_type_component_generic_dispatch_query_t, &
        query_select_type_component_generic_dispatch
    use frontend_compiler_resolution, only: declaration_binding_t, &
        get_scope_bindings, resolve_name_in_scope, &
        resolve_name_at_node, resolve_identifier_binding, &
        BINDING_NONE, BINDING_DECLARATION, BINDING_NAMED_CONSTANT, &
        BINDING_DUMMY_ARGUMENT, BINDING_DERIVED_TYPE, BINDING_FUNCTION, &
        BINDING_SUBROUTINE, BINDING_FUNCTION_RESULT, &
        BINDING_STATEMENT_FUNCTION, BINDING_GENERIC_INTERFACE, &
        BINDING_ASSOCIATE_NAME, ASSOCIATION_NONE, ASSOCIATION_DIRECT, &
        ASSOCIATION_HOST, ASSOCIATION_USE
    use frontend_compiler_type_queries, only: resolved_type_query_t, &
        query_resolved_type

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

    ! Re-export lexical trivia queries (Issue #393)
    use cst_nodes, only: trivia_t, CST_COMMENT, CST_WHITESPACE, CST_NEWLINE
    use cst_trivia_query, only: get_source_trivia_at, &
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

    ! Re-export utility functions from fortfront_utils
    use fortfront_utils, only: node_exists, get_node_type_at, get_node_location, &
        get_node_line, get_node_column, &
        get_parent, get_next_sibling, get_previous_sibling, &
        get_block_statements, is_last_in_block, is_block_node, &
        get_node_type, find_nodes_by_type, ast_to_json, &
        get_arena_stats, analyze_program, get_type_for_node, &
        get_diagnostics, semantic_info_to_json, get_max_depth, &
        get_node_as_program, get_node_as_assignment, &
        get_node_as_function_def, get_node_as_subroutine_def, &
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

    pure logical function is_fixed_form_file(path) result(is_fixed)
        !! Whether `path` names one of the fixed-form file extensions accepted
        !! by the file frontend.
        character(len=*), intent(in) :: path

        is_fixed = core_is_fixed_form_file(path)
    end function is_fixed_form_file

    subroutine normalize_fixed_form_source_text(source)
        !! Normalize text already identified as fixed form by its file path.
        character(len=:), allocatable, intent(inout) :: source

        call core_normalize_fixed_form_source_text(source)
    end subroutine normalize_fixed_form_source_text

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
