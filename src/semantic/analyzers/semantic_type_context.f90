module semantic_type_context
    use semantic_expression_context, only: infer_type_from_usage_context, &
        infer_expression_type_static
    use semantic_identifier_context, only: infer_identifier_type_from_context, &
        find_nearest_scope_owner, &
        find_program_owner
    implicit none
    private

    public :: infer_type_from_usage_context
    public :: infer_expression_type_static
    public :: infer_identifier_type_from_context
    public :: find_nearest_scope_owner
    public :: find_program_owner

end module semantic_type_context
