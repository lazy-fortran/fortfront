module fortfront_c_interface
    ! C-compatible interface for libfortfront.a
    ! Provides C bindings to fortfront functionality for external programs

    use iso_c_binding
    use transformation_api, only: transform_lazy_fortran_string
    use error_handling, only: &
        ERROR_VALIDATION, ERROR_MEMORY, ERROR_PARSER, ERROR_ERROR

    implicit none
    private

    ! Global state for error handling
    character(len=:, kind=c_char), allocatable, save, target :: last_error_message
    ! Transformed output of the most recent successful parse call, exposed to C
    ! callers via fortfront_get_output. Null-terminated for C string use.
    character(len=:, kind=c_char), allocatable, save, target :: last_output
    logical, save :: library_initialized = .false.

    ! Library version information
    character(len=*), parameter :: FORTFRONT_VERSION = "0.1.0"
    character(len=*), parameter :: BUILD_INFO = "Static library build"

    ! Public C interface functions
    public :: fortfront_initialize_c, fortfront_cleanup_c, fortfront_parse_source_c
    public :: fortfront_get_last_error_c, fortfront_clear_error_c
    public :: fortfront_get_output_c
    public :: fortfront_get_version_c, fortfront_get_build_info_c

contains

    ! Initialize the fortfront library
    function fortfront_initialize_c() result(status) bind(C, &
            name="fortfront_initialize")
        integer(c_int) :: status

        ! Clear any previous error
        call clear_last_error()

        ! Simple initialization - no special setup needed currently
        library_initialized = .true.
        status = 0 ! Success
    end function fortfront_initialize_c

    ! Clean up library resources
    subroutine fortfront_cleanup_c() bind(C, name="fortfront_cleanup")
        ! Clear error state
        call clear_last_error()

        ! Release any stored transformed output
        if (allocated(last_output)) deallocate (last_output)

        ! Mark as uninitialized
        library_initialized = .false.
    end subroutine fortfront_cleanup_c

    ! Parse Fortran source code
    function fortfront_parse_source_c(source_ptr, length) result(status) &
            bind(C, name="fortfront_parse_source")
        type(c_ptr), intent(in), value :: source_ptr
        integer(c_int), intent(in), value :: length
        integer(c_int) :: status

        character(len=:), allocatable :: fortran_source, output, error_msg
        character(len=1, kind=c_char), pointer :: char_array(:)
        integer :: actual_length, i

        ! Check if library is initialized
        if (.not. library_initialized) then
            call set_last_error("Library not initialized. Call fortfront_initialize() first.")
            status = -1
            return
        end if

        ! Clear any previous error
        call clear_last_error()

        ! Validate input parameters
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

        ! Convert C char array to Fortran string safely
        call c_f_pointer(source_ptr, char_array, [length])

        ! Find actual string length (stopping at null terminator or end)
        actual_length = 0
        do i = 1, length
            if (char_array(i) == c_null_char) exit
            actual_length = actual_length + 1
        end do

        if (actual_length == 0) then
            call set_last_error("Empty source code")
            status = -4
            return
        end if

        ! Allocate and copy the Fortran string
        allocate (character(len=actual_length) :: fortran_source)
        do i = 1, actual_length
            fortran_source(i:i) = char_array(i)
        end do

        ! Transform the source code
        call transform_lazy_fortran_string(fortran_source, output, error_msg)

        if (len(error_msg) > 0) then
            call set_last_error("Parse error: "//error_msg)
            status = -5
            return
        end if

        ! Store the transformed output so C callers can retrieve it via
        ! fortfront_get_output().
        call set_last_output(output)
        status = 0

    end function fortfront_parse_source_c

    ! Get the transformed output of the most recent successful parse call.
    ! Returns a null pointer when no output is available.
    function fortfront_get_output_c() result(output_ptr) bind(C, &
            name="fortfront_get_output")
        type(c_ptr) :: output_ptr

        if (allocated(last_output)) then
            output_ptr = c_loc(last_output)
        else
            output_ptr = c_null_ptr
        end if
    end function fortfront_get_output_c

    ! Get last error message
    function fortfront_get_last_error_c() result(error_ptr) bind(C, &
            name="fortfront_get_last_error")
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
    function fortfront_get_version_c() result(version_ptr) bind(C, &
            name="fortfront_get_version")
        type(c_ptr) :: version_ptr
        character(len=len(FORTFRONT_VERSION) + 1, kind=c_char), target, save &
            :: version_c

        version_c = FORTFRONT_VERSION//c_null_char
        version_ptr = c_loc(version_c)
    end function fortfront_get_version_c

    ! Get build information
    function fortfront_get_build_info_c() result(build_ptr) bind(C, &
            name="fortfront_get_build_info")
        type(c_ptr) :: build_ptr
        character(len=len(BUILD_INFO) + 1, kind=c_char), target, save :: build_c

        build_c = BUILD_INFO//c_null_char
        build_ptr = c_loc(build_c)
    end function fortfront_get_build_info_c

    ! Internal helper procedures

    ! Set the last error message
    subroutine set_last_error(message)
        character(len=*), intent(in) :: message

        if (allocated(last_error_message)) then
            deallocate (last_error_message)
        end if

        allocate (character(len=len(message) + 1, kind=c_char) :: last_error_message)
        last_error_message = message//c_null_char
    end subroutine set_last_error

    ! Store the transformed output as a null-terminated C string
    subroutine set_last_output(text)
        character(len=*), intent(in) :: text

        if (allocated(last_output)) then
            deallocate (last_output)
        end if

        allocate (character(len=len(text) + 1, kind=c_char) :: last_output)
        last_output = text//c_null_char
    end subroutine set_last_output

    ! Clear the last error message
    subroutine clear_last_error()
        if (allocated(last_error_message)) then
            deallocate (last_error_message)
        end if
    end subroutine clear_last_error

end module fortfront_c_interface
