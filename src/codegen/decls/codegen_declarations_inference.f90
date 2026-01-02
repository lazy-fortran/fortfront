module codegen_declarations_inference
    use codegen_parameter_mapping, only: build_parameter_map
    use codegen_character_types, only: derive_character_return_type, &
                                       character_len_references_params, &
                                       is_deferred_character_return, &
                                       is_allocatable_array_return, &
                                       is_deferred_shape_array, &
                                       has_character_len_result_decl, &
                                       is_character_len_declaration
    use codegen_program_variables, only: collect_program_variable_decls
    implicit none
    private
    public :: build_parameter_map
    public :: derive_character_return_type
    public :: character_len_references_params
    public :: is_deferred_character_return
    public :: is_allocatable_array_return
    public :: is_deferred_shape_array
    public :: has_character_len_result_decl
    public :: is_character_len_declaration
    public :: collect_program_variable_decls
end module codegen_declarations_inference
