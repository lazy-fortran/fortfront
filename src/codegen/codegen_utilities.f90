module codegen_utilities
    use codegen_arena_utils, only: check_result_is_simple_scalar, &
                                   find_node_index_in_arena, same_node
    use codegen_character_normalization, only: normalize_character_type, &
                                               normalize_character_type_param
    use codegen_declaration_grouping, only: can_group_declarations, &
                                            can_group_declarations_with_params, &
                                            can_group_parameters, &
                                            build_param_name_with_dims, &
                                            generate_grouped_declaration
    use codegen_grouped_body, only: generate_grouped_body, &
                                    generate_grouped_body_context
    use codegen_grouped_body_params, only: generate_grouped_body_with_params
    use codegen_import_reorder, only: reorder_import_lines
    use codegen_parameter_info, only: parameter_info_t, find_parameter_info, &
                                      is_function_parameter, &
                                      is_parameter_name
    use type_string_utils, only: is_character_type_string
    implicit none
    private

    public :: check_result_is_simple_scalar
    public :: find_node_index_in_arena
    public :: same_node
    public :: can_group_declarations
    public :: can_group_declarations_with_params
    public :: can_group_parameters
    public :: build_param_name_with_dims
    public :: generate_grouped_declaration
    public :: generate_grouped_body
    public :: generate_grouped_body_with_params
    public :: generate_grouped_body_context
    public :: find_parameter_info
    public :: is_function_parameter
    public :: is_parameter_name
    public :: parameter_info_t
    public :: is_character_type_string
    public :: normalize_character_type
    public :: normalize_character_type_param
    public :: reorder_import_lines

end module codegen_utilities
