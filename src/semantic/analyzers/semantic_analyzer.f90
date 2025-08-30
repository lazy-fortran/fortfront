module semantic_analyzer
    ! Main semantic analysis module - refactored for maintainability
    use semantic_analyzer_core, only: semantic_context_t, create_semantic_context, &
                                      analyze_program, validate_array_bounds, &
                                      check_shape_conformance, has_semantic_errors
    use semantic_analyzer_inference, only: infer_literal, infer_identifier, &
                                           infer_binary_op, infer_function_call, &
                                           infer_array_slice, infer_assignment, &
                                           infer_array_literal, infer_implied_do_loop, &
                                           infer_declaration_helper, update_identifier_type_in_arena
    implicit none
    private

    ! Re-export public interfaces
    public :: semantic_context_t, create_semantic_context
    public :: analyze_program
    public :: validate_array_bounds, check_shape_conformance
    public :: has_semantic_errors
    
    ! Re-export inference functions for compatibility
    public :: infer_literal, infer_identifier
    public :: infer_binary_op, infer_function_call, infer_array_slice
    public :: infer_assignment, infer_array_literal, infer_implied_do_loop
    public :: infer_declaration_helper, update_identifier_type_in_arena

end module semantic_analyzer