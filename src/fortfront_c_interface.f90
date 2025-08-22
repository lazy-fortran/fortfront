module fortfront_c_interface
    ! C-compatible interface for libfortfront.a
    ! Provides C bindings to fortfront functionality for external programs
    
    use iso_c_binding
    use frontend, only: transform_lazy_fortran_string, compilation_options_t, &
                        compile_source, format_options_t, &
                        transform_lazy_fortran_string_with_format
    use error_handling, only: result_t, success_result, create_error_result, &
                               ERROR_VALIDATION, ERROR_MEMORY, ERROR_PARSER, ERROR_ERROR
    
    implicit none
    private

    ! Global state for error handling
    character(len=:, kind=c_char), allocatable, save, target :: last_error_message
    logical, save :: library_initialized = .false.

    ! Library version information
    character(len=*), parameter :: FORTFRONT_VERSION = "0.1.0"
    character(len=*), parameter :: BUILD_INFO = "Static library build"

    ! Public C interface functions
    public :: fortfront_initialize_c, fortfront_cleanup_c, fortfront_parse_source_c
    public :: fortfront_get_last_error_c, fortfront_clear_error_c
    public :: fortfront_get_version_c, fortfront_get_build_info_c

contains

    ! Initialize the fortfront library
    function fortfront_initialize_c() result(status) bind(C, name="fortfront_initialize")
        integer(c_int) :: status
        
        ! Clear any previous error
        call clear_last_error()
        
        ! Simple initialization - no special setup needed currently
        library_initialized = .true.
        status = 0  ! Success
    end function fortfront_initialize_c

    ! Clean up library resources
    subroutine fortfront_cleanup_c() bind(C, name="fortfront_cleanup")
        ! Clear error state
        call clear_last_error()
        
        ! Mark as uninitialized
        library_initialized = .false.
    end subroutine fortfront_cleanup_c

    ! Parse Fortran source code
    function fortfront_parse_source_c(source_ptr, length) result(status) &
            bind(C, name="fortfront_parse_source")
        type(c_ptr), intent(in), value :: source_ptr
        integer(c_int), intent(in), value :: length
        integer(c_int) :: status
        
        character(len=length, kind=c_char), pointer :: source_string
        character(len=:), allocatable :: fortran_source, output, error_msg
        integer :: i
        
        ! Check if library is initialized
        if (.not. library_initialized) then
            call set_last_error("Library not initialized. Call fortfront_initialize() first.")
            status = -1
            return
        end if
        
        ! Clear any previous error
        call clear_last_error()
        
        ! Convert C string to Fortran string
        if (.not. c_associated(source_ptr)) then
            call set_last_error("Null source code pointer")
            status = -2
            return
        end if
        
        if (length <= 0) then
            call set_last_error("Invalid source code length")
            status = -3
            return
        end if
        
        call c_f_pointer(source_ptr, source_string)
        
        ! Convert C string to Fortran string (handle null termination)
        allocate(character(len=length) :: fortran_source)
        fortran_source = ""
        do i = 1, length
            if (source_string(i:i) == c_null_char) exit
            fortran_source(i:i) = source_string(i:i)
        end do
        
        ! Trim to actual content length
        fortran_source = trim(fortran_source)
        
        if (len(fortran_source) == 0) then
            call set_last_error("Empty source code")
            status = -4
            return
        end if
        
        ! Transform the source code
        call transform_lazy_fortran_string(fortran_source, output, error_msg)
        
        if (error_msg /= "") then
            call set_last_error("Parse error: " // error_msg)
            status = -5
            return
        end if
        
        ! Success - output available but not returned through C interface
        ! For now, we just validate that parsing works
        status = 0
        
    end function fortfront_parse_source_c

    ! Get last error message
    function fortfront_get_last_error_c() result(error_ptr) bind(C, name="fortfront_get_last_error")
        type(c_ptr) :: error_ptr
        
        if (allocated(last_error_message)) then
            error_ptr = c_loc(last_error_message)
        else
            error_ptr = c_null_ptr
        end if
    end function fortfront_get_last_error_c

    ! Clear error state
    subroutine fortfront_clear_error_c() bind(C, name="fortfront_clear_error")
        call clear_last_error()
    end subroutine fortfront_clear_error_c

    ! Get library version
    function fortfront_get_version_c() result(version_ptr) bind(C, name="fortfront_get_version")
        type(c_ptr) :: version_ptr
        character(len=len(FORTFRONT_VERSION), kind=c_char), target, save :: version_c
        
        version_c = FORTFRONT_VERSION // c_null_char
        version_ptr = c_loc(version_c)
    end function fortfront_get_version_c

    ! Get build information
    function fortfront_get_build_info_c() result(build_ptr) bind(C, name="fortfront_get_build_info")
        type(c_ptr) :: build_ptr
        character(len=len(BUILD_INFO), kind=c_char), target, save :: build_c
        
        build_c = BUILD_INFO // c_null_char
        build_ptr = c_loc(build_c)
    end function fortfront_get_build_info_c

    ! Internal helper procedures

    ! Set the last error message
    subroutine set_last_error(message)
        character(len=*), intent(in) :: message
        
        if (allocated(last_error_message)) then
            deallocate(last_error_message)
        end if
        
        allocate(character(len=len(message) + 1, kind=c_char) :: last_error_message)
        last_error_message = message // c_null_char
    end subroutine set_last_error

    ! Clear the last error message
    subroutine clear_last_error()
        if (allocated(last_error_message)) then
            deallocate(last_error_message)
        end if
    end subroutine clear_last_error

end module fortfront_c_interface