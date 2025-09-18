module parser_core
    ! Aggregated parser entry points for outsourced expression and definition parsing

    use parser_expressions_module, only: parse_expression, parse_primary
    use parser_definition_statements_module, only: parse_function_definition, &
                                                  parse_subroutine_definition
    implicit none
    private

    public :: parse_expression
    public :: parse_primary
    public :: parse_function_definition
    public :: parse_subroutine_definition

end module parser_core
