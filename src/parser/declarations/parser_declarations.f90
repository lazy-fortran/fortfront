module parser_declarations
    use parser_declarations_type_spec_support_module, only: type_specifier_t, &
        is_type_attribute_token, &
        parser_is_at_type_definition
    use parser_declarations_type_spec_module, only: parse_type_specifier
    use parser_declarations_core_module, only: parse_declaration, &
        parse_declaration_with_result
    use parser_declarations_multi_module, only: parse_multi_declaration
    use parser_declaration_attributes_module, only: parse_array_dimensions
    use parser_declarations_derived_module, only: parse_derived_type_def, &
        parse_derived_type_component
    implicit none
    private

    public :: type_specifier_t
    public :: parse_declaration
    public :: parse_multi_declaration
    public :: parse_declaration_with_result
    public :: parse_type_specifier
    public :: parse_derived_type_def
    public :: parse_derived_type_component
    public :: parse_array_dimensions
    public :: is_type_attribute_token
    public :: parser_is_at_type_definition

end module parser_declarations
