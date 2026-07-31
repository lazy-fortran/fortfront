module frontend_transformation_structure
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_misc, only: contains_node, use_statement_node
    use ast_nodes_data, only: declaration_node, module_node, &
        mixed_construct_container_node, &
        multi_unit_container_node
    use procedure_classification, only: should_hoist_procedure, &
        procedure_has_entry_statement
    use frontend_transformation_common, only: transform_context_t, format_options_t
    use compiler_arena, only: compiler_arena_t
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_basic_utils, only: add_line_continuations
    use codegen_indent, only: set_indent_config, get_indent_config, &
        set_line_length_config, get_line_length_config
    use codegen_type_utils, only: set_type_standardization, &
        get_type_standardization
    use source_bom, only: decode_source_bom
    use standardizer, only: set_standardizer_type_standardization, &
        get_standardizer_type_standardization
    implicit none
    private

    public :: collect_procedures_and_target
    public :: filter_hoistable_procedures
    public :: remove_procedures_from_body
    public :: ensure_contains_exists
    public :: insert_procedures_after_contains
    public :: clean_external_declarations
    public :: merge_additional_main_programs
    public :: append_program_body_to_target
    public :: remove_target_procedures_from_body
    public :: normalize_multi_unit_container
    public :: collect_procedure_indices
    public :: create_module_with_procedures
    public :: wrap_ast_in_module_only
    public :: wrap_ast_in_module_and_program
    public :: run_code_generation_phase
    public :: is_whitespace_only
    public :: has_leading_comment
    public :: extract_leading_comment_block
    public :: contains_binary_data
    public :: decode_bom_if_needed
    public :: save_current_configuration
    public :: restore_configuration
    public :: apply_format_options

contains

    include 'frontend_transformation_structure.inc'

end module frontend_transformation_structure
