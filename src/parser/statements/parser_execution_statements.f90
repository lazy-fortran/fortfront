module parser_execution_statements_module
    ! Parser module for execution statement types (call, program)
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, &
        TK_STRING, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, to_lower
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, &
        TK_STRING, TK_NEWLINE, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    use parser_label_validation_module, only: validate_label_context
    use parser_declarations, only: parse_declaration, parse_multi_declaration, &
        parse_derived_type_def, parser_is_at_type_definition
    use parser_definition_statements_module, only: parse_function_definition, &
        parse_subroutine_definition, &
        parse_interface_block
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    use parser_procedure_shared_module, only: consume_optional_kind_spec
    use parser_assignment_module, only: parse_assignment_statement
    use parser_utils, only: analyze_declaration_structure
    use parser_io_statements_module, only: parse_print_statement, &
        parse_write_statement, &
        parse_read_statement, &
        parse_format_statement, &
        parse_open_statement, &
        parse_close_statement, &
        parse_inquire_statement, &
        parse_backspace_statement, &
        parse_rewind_statement, &
        parse_endfile_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
        parse_deallocate_statement
    use parser_control_statements_module, only: parse_stop_statement, &
        parse_goto_statement, &
        parse_error_stop_statement, &
        parse_return_statement, &
        parse_entry_statement, &
        parse_continue_statement, &
        parse_cycle_statement, &
        parse_exit_statement, &
        parse_nullify_statement, &
        parse_pause_statement
    use parser_control_flow_router_module, only: route_control_flow, &
        is_control_flow_keyword
    use parser_do_constructs_module, only: parse_do_loop
    use parser_statement_data_module, only: parse_data_statement, &
        parse_namelist_statement, &
        get_data_additional_indices
    use parser_call_module, only: parse_call_statement
    use parser_import_resolution_module, only: parse_use_statement, &
        parse_include_statement
    use parser_intrinsic_statements_module, only: parse_intrinsic_statement
    use parser_external_statements_module, only: parse_external_statement
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    use parser_type_specifications_module, only: parse_implicit_statement, &
        take_implicit_additional_indices
    use parser_dimension_statements_module, only: parse_dimension_statement
    use parser_value_statements_module, only: parse_value_statement
    use parser_keyword_disambiguation_module, only: looks_like_format_statement, &
        looks_like_implicit_statement
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: contains_node
    use ast_factory, only: push_program, &
        push_declaration, push_implicit_statement, push_goto
    use parser_statement_utilities_module, only: parse_comment_or_directive
    use parser_legacy_statements_module, only: parse_legacy_statement
    use parser_common_statement_module, only: parse_common_statement
    use parser_enum_statement_module, only: parse_enum_construct
    use parser_trailing_comment_module, only: capture_trailing_comment
    use parser_submodule_placement_module, only: reject_misplaced_submodule
    implicit none
    private

    public :: parse_call_statement, parse_program_statement

    ! Module variable to store additional indices from multi-declaration parsing
    integer, allocatable :: additional_execution_indices(:)

contains

    include 'parser_execution_statements_module.inc'

end module parser_execution_statements_module
