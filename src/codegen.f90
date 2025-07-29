module codegen
    ! Main code generation interface
    use codegen_core
    implicit none
    private
    
    ! Re-export code generation functionality
    public :: generate_code
    
contains

    ! Note: For now, we just use core code generation
    ! In the future, this will handle dialect-specific code generation

end module codegen