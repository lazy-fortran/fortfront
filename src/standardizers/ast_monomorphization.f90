module ast_monomorphization
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_subtree_clone, only: clone_ast_subtree
    use ast_nodes_core, only: program_node, call_or_subscript_node, &
        assignment_node, identifier_node, literal_node, &
        create_program
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
        get_procedure_name, get_procedure_params, &
        get_procedure_body, get_procedure_return_type, &
        create_function_def, create_subroutine_def
    use ast_nodes_data, only: module_node, parameter_declaration_node, &
        declaration_node, mixed_construct_container_node, &
        multi_unit_container_node, &
        create_multi_unit_container, &
        create_module
    use ast_factory_declarations, only: push_declaration
    use ast_nodes_misc, only: interface_block_node, module_procedure_node, &
        use_statement_node, create_interface_block, &
        create_module_procedure, create_use_statement
    use ast_base, only: string_t
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t
    use codegen_name_mangling, only: mangle_procedure_name
    use fortfront_constants, only: MAX_DIMENSION_BUFFER_LEN
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t, create_mono_type, TINT, TREAL, &
        TLOGICAL, TCHAR, TCOMPLEX, TDOUBLE, TARRAY, &
        TVAR
    use uid_generator, only: generate_uid
    use string_utils_mod, only: to_lower
    use standardizer_subroutine_intent, only: infer_subroutine_parameter_intents
    use standardizer_parameter, only: param_metadata_t, init_param_metadata
    use procedure_classification, only: procedure_has_explicit_types
    implicit none
    private

    public :: transform_monomorphization

    logical, save :: debug_initialized = .false.
    logical, save :: debug_enabled = .false.

contains

    include 'ast_monomorphization_part1.inc'
    include 'ast_monomorphization_part2.inc'
    include 'ast_monomorphization_part3.inc'

end module ast_monomorphization
