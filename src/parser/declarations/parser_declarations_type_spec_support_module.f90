module parser_declarations_type_spec_support_module
    use parser_type_spec_tokens_mod, only: append_token, append_int, tokens_to_text
    use parser_type_spec_tokens_mod, only: trim_token_sequence, strip_outer_parentheses
    use parser_type_spec_result_mod, only: type_specifier_t
    use parser_type_spec_result_mod, only: clear_derived_type_storage
    use parser_type_spec_result_mod, only: initialize_type_specifier
    use parser_type_spec_result_mod, only: split_derived_type_name_and_params
    use parser_type_spec_result_mod, only: set_derived_type_name_info
    use parser_type_spec_result_mod, only: process_derived_type_parameters
    use parser_type_spec_result_mod, only: analyze_derived_type_tokens
    use parser_type_spec_result_mod, only: parse_single_parameter
    use parser_type_spec_attributes_mod, only: is_type_attribute_token
    use parser_type_spec_attributes_mod, only: skip_type_definition_attributes
    use parser_type_spec_attributes_mod, only: parser_is_at_type_definition
    implicit none
    private

    public :: type_specifier_t
    public :: append_token
    public :: append_int
    public :: tokens_to_text
    public :: trim_token_sequence
    public :: strip_outer_parentheses
    public :: clear_derived_type_storage
    public :: initialize_type_specifier
    public :: split_derived_type_name_and_params
    public :: set_derived_type_name_info
    public :: process_derived_type_parameters
    public :: analyze_derived_type_tokens
    public :: parse_single_parameter
    public :: is_type_attribute_token
    public :: skip_type_definition_attributes
    public :: parser_is_at_type_definition
end module parser_declarations_type_spec_support_module
