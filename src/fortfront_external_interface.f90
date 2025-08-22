module fortfront_external_interface
    ! External Fortran interface for libfortfront.a static library
    ! Provides clean API for external Fortran programs to use fortfront
    
    use error_handling
    use frontend
    use string_utils
    implicit none
    private
    
    public :: fortfront_external_compile
    public :: fortfront_external_get_version
    public :: fortfront_external_get_build_info
    
contains
    
    subroutine fortfront_external_compile(source_code, compilation_result)
        ! Compile Fortran source code using fortfront
        ! 
        ! Arguments:
        !   source_code - input Fortran source code
        !   compilation_result - output result with success/error information
        character(len=*), intent(in) :: source_code
        type(result_t), intent(out) :: compilation_result
        
        ! Use the main frontend compilation interface
        compilation_result = compile_source(source_code)
        
    end subroutine fortfront_external_compile
    
    function fortfront_external_get_version() result(version_string)
        ! Get fortfront library version
        character(len=:), allocatable :: version_string
        
        version_string = "0.1.0"
        
    end function fortfront_external_get_version
    
    function fortfront_external_get_build_info() result(build_info)
        ! Get fortfront build information
        character(len=:), allocatable :: build_info
        
        build_info = "fortfront static library - core analysis frontend for lazy fortran"
        
    end function fortfront_external_get_build_info
    
end module fortfront_external_interface