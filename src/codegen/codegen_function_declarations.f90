module codegen_function_declarations
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node, array_literal_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_io, only: print_statement_node, read_statement_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_misc, only: interface_block_node, implicit_statement_node, &
                              use_statement_node, comment_node, directive_node, &
                              blank_line_node, end_statement_node
    use ast_nodes_transfer, only: entry_node
    use codegen_declarations_core, only: fix_character_len_placeholder
    use codegen_declarations_inference, only: build_parameter_map, &
                                              derive_character_return_type, &
                                              has_character_len_result_decl, &
                                              is_deferred_character_return, &
                                              is_allocatable_array_return, &
                                              is_deferred_shape_array
    use codegen_procedure_shared, only: build_parameter_clause, gather_prefix, &
                                        copy_indices, apply_default_intents, &
                                        apply_function_default_intents, &
                                        maybe_add_procedure_implicit_none, &
                                        filter_implicit_statements, &
                                        append_parameter_declaration, &
                                        is_parameter_name, ensure_local_var_capacity, &
                                        is_local_var_collected
    use codegen_type_utils, only: get_type_standardization
    use codegen_grouped_body, only: generate_grouped_body
    use codegen_grouped_body_params, only: generate_grouped_body_with_params
    use codegen_import_reorder, only: reorder_import_lines
    use codegen_parameter_info, only: parameter_info_t
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_entry_utils, only: collect_entry_param_names, &
                                   is_entry_parameter_decl
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t, TARRAY, TFUN, type_args_allocated, &
                                   type_args_size, type_args_element
    implicit none
    private
    public :: generate_code_function_def

contains

    include 'codegen_function_declarations_part1.inc'
    include 'codegen_function_declarations_part2.inc'

end module codegen_function_declarations
