module parser_definition_statements_module
    ! Parser module for definition statement types (function, subroutine, interface, derived types)
    ! This module has been refactored into specialized submodules for maintainability
    use parser_parameter_handling_module, only: parse_typed_parameters, merge_parameter_attributes
    use parser_type_definitions_module, only: parse_derived_type, parse_derived_type_parameters
    use parser_procedure_definitions_module, only: parse_function_definition, parse_subroutine_definition, &
                                                   parse_interface_block
    use parser_statement_utilities_module, only: parse_statement_in_if_block
    implicit none
    private

    ! Re-export public interfaces from submodules
    public :: parse_derived_type, parse_function_definition, parse_subroutine_definition
    public :: parse_interface_block, parse_typed_parameters
    public :: parse_statement_in_if_block

    ! Legacy compatibility - re-export parameter handling utilities
    public :: merge_parameter_attributes, parse_derived_type_parameters

contains


end module parser_definition_statements_module