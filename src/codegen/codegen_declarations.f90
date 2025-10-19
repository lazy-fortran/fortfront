module codegen_declarations
    use codegen_declarations_subprogram_mod, only: &
        generate_code_function_def, &
        generate_code_subroutine_def
    use codegen_declarations_variable_mod, only: &
        generate_code_declaration, &
        generate_code_parameter_declaration
    use codegen_declarations_module_mod, only: &
        generate_code_module
    use codegen_declarations_interface_mod, only: &
        generate_code_interface_block, &
        generate_code_module_procedure
    use codegen_declarations_type_mod, only: &
        generate_code_derived_type
    use codegen_declarations_program_mod, only: &
        generate_code_program
    implicit none
    private
    public :: generate_code_function_def
    public :: generate_code_subroutine_def
    public :: generate_code_declaration
    public :: generate_code_parameter_declaration
    public :: generate_code_module
    public :: generate_code_interface_block
    public :: generate_code_module_procedure
    public :: generate_code_derived_type
    public :: generate_code_program
end module codegen_declarations
