module parser_dispatcher_module
    ! Statement dispatcher that delegates to appropriate parsing modules
    ! This implements the SRP by separating the switch logic from the implementations
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, to_lower
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module
    use parser_declarations, only: parse_declaration, parse_multi_declaration, &
        parse_derived_type_def, parser_is_at_type_definition
    use parser_utils, only: analyze_declaration_structure
    use parser_import_resolution_module, only: parse_use_statement, &
        parse_include_statement
    use parser_module_structures_module, only: parse_module
    use parser_submodule_structures_module, only: parse_submodule
    use parser_intrinsic_statements_module, only: parse_intrinsic_statement
    use parser_external_statements_module, only: parse_external_statement
    use parser_block_data_module, only: parse_block_data
    use parser_template_blocks_module, only: parse_template_block, parse_trait_block, &
        parse_requirement_block, &
        parse_implements_block
    use parser_instantiate_statement_module, only: parse_instantiate_statement
    use parser_io_statements_module, only: parse_print_statement, &
        parse_write_statement, &
        parse_read_statement, &
        parse_open_statement, &
        parse_close_statement, &
        parse_format_statement, &
        parse_inquire_statement, &
        parse_backspace_statement, &
        parse_rewind_statement, &
        parse_endfile_statement
    use parser_definition_statements_module, only: parse_function_definition, &
        parse_subroutine_definition, &
        parse_interface_block
    use parser_procedure_definitions_module, only: init_interface_procedure_parser
    use mixed_construct_detector, only: function_follows_type_spec
    use parser_control_statements_module, only: &
        parse_stop_statement, parse_return_statement, parse_entry_statement, &
        parse_goto_statement, parse_error_stop_statement, parse_cycle_statement, &
        parse_exit_statement, parse_end_statement, parse_nullify_statement, &
        parse_pause_statement, parse_continue_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
        parse_deallocate_statement
    use parser_execution_statements_module, only: parse_call_statement, &
        parse_program_statement
    use parser_type_specifications_module, only: parse_implicit_statement, &
        take_implicit_additional_indices
    use parser_statement_data_module, only: parse_data_statement, &
        get_data_additional_indices, &
        parse_namelist_statement
    use parser_legacy_statements_module, only: parse_legacy_statement
    use parser_dimension_statements_module, only: parse_dimension_statement
    use parser_enum_statement_module, only: parse_enum_construct
    use parser_control_flow_router_module, only: route_control_flow
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: comment_node, blank_line_node, directive_node
    use uid_generator, only: generate_uid
    use ast_factory
    use parser_assignment_module, only: parse_assignment_statement
    use parser_expressions_module, only: parse_expression
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    implicit none
    private

    public :: parse_statement_dispatcher, get_additional_indices, &
        clear_additional_indices, get_last_parser_errors, clear_parser_errors

    ! Module variable to store additional indices from multi-declaration parsing
    integer, allocatable :: additional_indices(:)

    ! Module variable to store parser errors from last dispatcher call
    character(len=:), allocatable :: last_parser_errors

contains

    include 'parser_dispatcher_part1.inc'
    include 'parser_dispatcher_part2.inc'

end module parser_dispatcher_module
