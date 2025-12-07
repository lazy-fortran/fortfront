module codegen_declarations_programs
    use codegen_module_generation, only: generate_code_module, &
                                         generate_code_submodule, &
                                         generate_code_block_data, &
                                         generate_code_interface_block, &
                                         generate_code_module_procedure
    use codegen_program_generation, only: generate_code_program
    implicit none
    private
    public :: generate_code_program
    public :: generate_code_module
    public :: generate_code_submodule
    public :: generate_code_block_data
    public :: generate_code_interface_block
    public :: generate_code_module_procedure

end module codegen_declarations_programs

