module parser_import_statements_module
    ! Main parser module for import/module statements (refactored)
    use parser_import_resolution_module, only: parse_use_statement, parse_include_statement
    use parser_type_specifications_module, only: parse_implicit_statement
    use parser_module_structures_module, only: parse_module
    implicit none
    private

    public :: parse_use_statement, parse_implicit_statement, parse_include_statement
    public :: parse_module

contains

    ! All functions are now delegated to specialized modules

end module parser_import_statements_module