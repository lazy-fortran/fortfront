module codegen_program_variables
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
                              identifier_node, program_node, binary_op_node, &
                              array_literal_node, component_access_node, &
                              range_subscript_node
    use ast_nodes_misc, only: contains_node, use_statement_node, &
                              allocate_statement_node, interface_block_node, &
                              module_procedure_node, implicit_statement_node, &
                              comment_node, directive_node, blank_line_node, &
                              namelist_statement_node
    use ast_nodes_data, only: declaration_node, module_node, derived_type_node
    use ast_nodes_legacy, only: enum_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: entry_node
    use ast_nodes_control, only: associate_node, if_node, do_loop_node, &
                                 do_while_node, select_case_node, case_block_node, &
                                 case_default_node, where_node, forall_node, &
                                 block_construct_node
    use ast_nodes_conditional, only: select_type_node, type_guard_block_node, &
                                     select_rank_node, rank_block_node
    use codegen_program_decl_utils, only: exists_in_list, &
                                          build_function_return_type_table, &
                                          initialize_program_decl_state, &
                                          program_decl_state_t, &
                                          program_decl_max_vars, &
                                          record_declared_name, &
                                          record_namelist_group, &
                                          record_use_associated_name, &
                                          record_use_module_name, &
                                          seed_namelist_groups_from_text
    use codegen_type_utils, only: get_type_standardization
    use codegen_type_inference_utils, only: canonicalize_type, &
                                            deduce_type_from_arguments, &
                                            infer_function_return_type_from_rhs
    use intrinsic_registry, only: is_intrinsic_function
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t
    use variable_usage_dispatcher_module, only: collect_identifiers_recursive
    use variable_usage_core_module, only: variable_usage_info_t, &
                                          create_variable_usage_info
    implicit none
    private
    public :: collect_program_variable_decls

contains

    include 'codegen_program_variables_collect.inc'
    ! Variable collection excludes SELECT TYPE guard type-names (issue #2821)
    include 'codegen_program_variables_analysis.inc'
    include 'codegen_program_variables_emit.inc'

end module codegen_program_variables
