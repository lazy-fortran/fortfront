module standardizer_declarations
    ! Variable declaration generation module - main interface
    ! Uses consolidated core functionality
    
    use standardizer_declarations_core
    implicit none
    private

    ! Re-export public interfaces from component modules
    public :: insert_variable_declarations
    public :: has_implicit_none
    public :: program_has_variable_declarations
    public :: find_declaration_insertion_point
    public :: generate_and_insert_declarations
    public :: has_explicit_declaration
    public :: collect_statement_vars
    public :: collect_assignment_vars
    public :: collect_identifier_var
    public :: collect_identifier_var_with_type
    public :: add_variable
    public :: mark_variable_declared
    public :: standardize_declarations
    public :: handle_string_concatenation
    public :: get_string_length_from_node
    public :: infer_type_from_binary_operation

contains

    ! This module now serves as a facade - actual implementation is in component modules

end module standardizer_declarations