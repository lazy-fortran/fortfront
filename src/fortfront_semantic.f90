module fortfront_semantic
    use semantic_api, only: semantic_context_t, create_semantic_context, &
        analyze_program, analyze_semantics, &
        has_semantic_errors, mono_type_t, poly_type_t, &
        type_env_t, type_var_t, substitution_t, &
        create_mono_type, create_poly_type, &
        create_type_var, TVAR, TINT, TREAL, TCHAR, &
        TLOGICAL, TFUN, TARRAY, TCOMPLEX, TDOUBLE, &
        TDERIVED, scope_stack_t, create_scope_stack, &
        error_collection_t, result_t
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_INFER, &
        OPERATING_MODE_STRICT
    use symbol_table_api, only: get_symbols_in_scope, get_all_symbols, &
        is_symbol_defined, lookup_symbol, &
        get_scope_info, get_current_scope_depth
    use fortfront_types, only: symbol_info_t, symbol_reference_t, &
        scope_info_t, source_location_t, &
        source_range_t, type_info_t, diagnostic_t, &
        function_signature_t
    implicit none
    public
end module fortfront_semantic
