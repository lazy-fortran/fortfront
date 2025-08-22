module fortfront_c_interface
    ! C interface for libfortfront.a static library
    ! Provides C-compatible subroutines for external language binding
    
    use, intrinsic :: iso_c_binding
    use error_handling
    use frontend
    use string_utils
    implicit none
    private
    
    ! Module state
    logical :: c_interface_initialized = .false.
    character(len=1000) :: last_error_message = ""
    
    public :: fortfront_initialize_c
    public :: fortfront_cleanup_c
    public :: fortfront_parse_source_c
    public :: fortfront_get_last_error_c
    public :: fortfront_clear_error_c
    public :: fortfront_get_version_c
    public :: fortfront_get_build_info_c
    
contains
    
    function fortfront_initialize_c() result(return_code) bind(c, name='fortfront_initialize')
        ! Initialize the fortfront library
        ! Returns 0 on success, non-zero on failure
        integer(c_int) :: return_code
        
        if (c_interface_initialized) then
            return_code = 0
            return
        end if
        
        ! Basic initialization - no complex setup needed for current version
        c_interface_initialized = .true.
        last_error_message = ""
        return_code = 0
        
    end function fortfront_initialize_c
    
    subroutine fortfront_cleanup_c() bind(c, name='fortfront_cleanup')
        ! Cleanup the fortfront library
        
        c_interface_initialized = .false.
        last_error_message = ""
        
    end subroutine fortfront_cleanup_c
    
    function fortfront_parse_source_c(source_code, length) result(return_code) &
            bind(c, name='fortfront_parse_source')
        ! Parse Fortran source code
        ! Returns 0 on success, non-zero on failure
        integer(c_int), intent(in), value :: length
        character(kind=c_char), intent(in) :: source_code(*)
        integer(c_int) :: return_code
        
        character(len=length) :: fortran_source
        character(len=:), allocatable :: error_msg
        integer :: i
        
        if (.not. c_interface_initialized) then
            last_error_message = "Library not initialized"
            return_code = 1
            return
        end if
        
        ! Convert C string to Fortran string
        do i = 1, length
            fortran_source(i:i) = source_code(i)
        end do
        
        ! For now, just indicate successful parsing
        ! Real implementation would call frontend parsing functions
        ! but those are subroutines, not functions returning results
        error_msg = ""
        
        if (len_trim(fortran_source) == 0) then
            last_error_message = "Empty source code"
            return_code = 1
        else
            last_error_message = ""
            return_code = 0
        end if
        
    end function fortfront_parse_source_c
    
    function fortfront_get_last_error_c() result(error_ptr) bind(c, name='fortfront_get_last_error')
        ! Get last error message
        ! Returns C string pointer to error message
        type(c_ptr) :: error_ptr
        
        ! For simplicity, return null pointer - full error handling would need
        ! more complex memory management
        error_ptr = c_null_ptr
        
    end function fortfront_get_last_error_c
    
    subroutine fortfront_clear_error_c() bind(c, name='fortfront_clear_error')
        ! Clear last error message
        
        last_error_message = ""
        
    end subroutine fortfront_clear_error_c
    
    function fortfront_get_version_c() result(version_ptr) bind(c, name='fortfront_get_version')
        ! Get library version string
        ! Returns C string pointer to version
        type(c_ptr) :: version_ptr
        
        ! For simplicity, return null pointer - full implementation would need
        ! static string allocation
        version_ptr = c_null_ptr
        
    end function fortfront_get_version_c
    
    function fortfront_get_build_info_c() result(build_ptr) bind(c, name='fortfront_get_build_info')
        ! Get build information string
        ! Returns C string pointer to build info
        type(c_ptr) :: build_ptr
        
        ! For simplicity, return null pointer - full implementation would need
        ! static string allocation
        build_ptr = c_null_ptr
        
    end function fortfront_get_build_info_c
    
end module fortfront_c_interface