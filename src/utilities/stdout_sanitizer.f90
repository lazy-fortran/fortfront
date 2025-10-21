module stdout_sanitizer
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private

    public :: sanitize_redirected_stdout
    public :: sanitize_file_path

    integer(c_int), parameter :: STDOUT_FD = int(1, kind=c_int)

    interface
        function ff_sanitize_fd(fd) bind(C, name='ff_sanitize_fd') result(res)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: res
        end function ff_sanitize_fd

        function ff_sanitize_path(path) bind(C, name='ff_sanitize_path') result(res)
            import :: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: path
            integer(c_int) :: res
        end function ff_sanitize_path
    end interface

contains

    subroutine sanitize_redirected_stdout()
        integer(c_int) :: status
        status = ff_sanitize_fd(STDOUT_FD)
        if (status < 0_c_int) then
            call report_sanitizer_failure(status)
        end if
    end subroutine sanitize_redirected_stdout

    integer function sanitize_file_path(path) result(status)
        character(len=*), intent(in) :: path
        character(kind=c_char), allocatable :: c_path(:)
        integer :: i, n

        n = len_trim(path)
        allocate (c_path(0:n))

        do i = 1, n
            c_path(i - 1) = char(iachar(path(i:i)), kind=c_char)
        end do
        c_path(n) = c_null_char

        status = int(ff_sanitize_path(c_path))

        deallocate (c_path)
    end function sanitize_file_path

    subroutine report_sanitizer_failure(status)
        use, intrinsic :: iso_fortran_env, only: error_unit
        integer(c_int), intent(in) :: status
        write (error_unit, '(A,I0)') 'Warning: unable to sanitize stdout (code=', &
            status, ')'
    end subroutine report_sanitizer_failure

end module stdout_sanitizer
