module codegen_declarations
    use codegen_function_declarations, only: generate_code_function_def
    use codegen_subroutine_declarations, only: generate_code_subroutine_def
    use codegen_declarations_core, only: generate_code_declaration, &
                                         generate_code_parameter_declaration, &
                                         generate_code_derived_type
    use codegen_module_generation, only: generate_code_module, &
                                         generate_code_submodule, &
                                         generate_code_block_data, &
                                         generate_code_interface_block, &
                                         generate_code_module_procedure
    use codegen_program_generation, only: generate_code_program, &
                                          generate_multi_unit_program
    implicit none
    private
    public :: generate_code_function_def
    public :: generate_code_subroutine_def
    public :: generate_code_declaration
    public :: generate_code_parameter_declaration
    public :: generate_code_module
    public :: generate_code_submodule
    public :: generate_code_block_data
    public :: generate_code_interface_block
    public :: generate_code_module_procedure
    public :: generate_code_derived_type
    public :: generate_code_program
    public :: generate_multi_unit_program
end module codegen_declarations
