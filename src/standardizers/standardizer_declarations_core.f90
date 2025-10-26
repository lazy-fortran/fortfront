module standardizer_declarations_core
    use standardizer_declarations_state, only: get_standardizer_type_standardization
    use standardizer_declarations_insertion, only: &
        insert_variable_declarations, has_implicit_none, &
        program_has_variable_declarations, find_declaration_insertion_point, &
        find_declaration_header_end, generate_and_insert_declarations, &
        standardize_declarations, create_single_declaration, &
        has_explicit_declaration
    use standardizer_declarations_collection, only: &
        collect_statement_vars, collect_assignment_vars
    use standardizer_declarations_variables, only: &
        collect_identifier_var, collect_identifier_var_with_type, &
        add_variable, mark_variable_declared
    use standardizer_declarations_inference, only: &
        handle_string_concatenation, get_string_length_from_node, &
        infer_type_from_binary_operation
    use standardizer_declarations_parsing, only: &
        apply_type_string_to_decl, update_existing_declaration_type
    use standardizer_declarations_array, only: &
        parse_dimension_attribute, set_array_properties_from_type
    implicit none
    private

    public :: insert_variable_declarations
    public :: has_implicit_none
    public :: program_has_variable_declarations
    public :: find_declaration_insertion_point
    public :: find_declaration_header_end
    public :: generate_and_insert_declarations
    public :: standardize_declarations
    public :: create_single_declaration
    public :: has_explicit_declaration
    public :: collect_statement_vars
    public :: collect_assignment_vars
    public :: collect_identifier_var
    public :: collect_identifier_var_with_type
    public :: add_variable
    public :: mark_variable_declared
    public :: handle_string_concatenation
    public :: get_string_length_from_node
    public :: infer_type_from_binary_operation
    public :: get_standardizer_type_standardization
    public :: apply_type_string_to_decl
    public :: update_existing_declaration_type
    public :: parse_dimension_attribute
    public :: set_array_properties_from_type

end module standardizer_declarations_core
