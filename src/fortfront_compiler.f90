module fortfront_compiler
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
        get_derived_type_parameters, &
        get_declaration_type_parameters, &
        type_parameter_t, PARAM_UNKNOWN, PARAM_KIND, PARAM_LEN, &
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
        derived_type_query_t, type_binding_query_t, &
        use_statement_query_t, interface_query_t, visibility_query_t, &
        namelist_query_t, data_statement_query_t, common_block_query_t, &
        enum_query_t, statement_function_query_t, block_data_query_t, &
        query_program_units, query_program_unit, query_declarations, &
        query_declaration, query_derived_type, query_type_binding, &
        query_use_statement, query_use_statements, query_interface, &
        query_visibility, query_namelist, query_data_statement, &
        query_common_block, query_enum, query_statement_function, &
        query_block_data, &
        array_slice_query_t, array_bounds_query_t, &
        range_expression_query_t, component_access_query_t, &
        array_literal_query_t, pointer_assignment_query_t, nullify_query_t, &
        query_array_slice, query_array_bounds, query_range_expression, &
        query_component_access, query_array_literal, &
        query_pointer_assignment, query_nullify
    use frontend_compiler_diagnostics, only: compiler_diagnostic_t, &
        get_compiler_diagnostics, &
        DIAGNOSTIC_PHASE_PARSER, DIAGNOSTIC_PHASE_SEMANTIC, &
        DIAGNOSTIC_CODE_PARSER, DIAGNOSTIC_CODE_SEMANTIC
    use frontend_compiler_io_queries
    use frontend_compiler_control_queries
    use frontend_compiler_branch_queries
    use frontend_compiler_node_queries, only: is_declaration_node, &
        is_derived_type_node, &
        get_declaration_var_name, get_declaration_type_name, &
        get_declaration_has_initializer, &
        get_declaration_initializer_index, &
        get_derived_type_name, &
        get_node_stmt_label, get_goto_label, goto_is_computed, &
        get_goto_label_list, get_goto_selector_index
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
    use fortfront_semantic, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD, &
        OPERATING_MODE_INFER, OPERATING_MODE_STRICT
    use fortfront_ast, only: ast_arena_t
    use fortfront_types, only: DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, &
        DIAGNOSTIC_INFO
    use fortfront_utils, only: node_exists, get_node_type_at, get_type_for_node
    use fortfront_lexer, only: token_t
    use type_system_unified, only: mono_type_t, &
        TINT, TREAL, TCHAR, TLOGICAL, TARRAY, TCOMPLEX, TDOUBLE, TDERIVED
    implicit none
    public
end module fortfront_compiler
