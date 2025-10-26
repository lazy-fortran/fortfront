module codegen_declarations_procedures
    use codegen_function_declarations, only: generate_code_function_def
    use codegen_subroutine_declarations, only: generate_code_subroutine_def
    implicit none
    private
    public :: generate_code_function_def
    public :: generate_code_subroutine_def
end module codegen_declarations_procedures

