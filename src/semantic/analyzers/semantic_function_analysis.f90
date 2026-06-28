module semantic_function_analysis
    use semantic_parameter_analysis, only: analyze_function_parameters, &
        merge_parameter_type, &
        refine_parameters_from_body_usage
    use semantic_function_inference, only: determine_function_return_type
    use semantic_scope_creation, only: create_function_scope
    use semantic_type_context, only: infer_type_from_usage_context, &
        infer_expression_type_static, &
        infer_identifier_type_from_context
    implicit none
    private

    public :: analyze_function_parameters
    public :: merge_parameter_type
    public :: refine_parameters_from_body_usage
    public :: determine_function_return_type
    public :: create_function_scope
    public :: infer_type_from_usage_context
    public :: infer_expression_type_static
    public :: infer_identifier_type_from_context

end module semantic_function_analysis
