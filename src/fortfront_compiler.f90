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
    use fortfront_semantic, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD, &
                                  OPERATING_MODE_INFER, OPERATING_MODE_STRICT
    use fortfront_ast, only: ast_arena_t
    use fortfront_lexer, only: token_t
    implicit none
    public
end module fortfront_compiler
