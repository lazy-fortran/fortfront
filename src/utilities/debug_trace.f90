module debug_trace
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    integer, save :: depth = 0
    logical, save :: enabled = .false.
    integer, parameter :: MAX_DEPTH = 2000
    integer, save :: file_u = -1
    character(len=256), save :: file_name = ''

    public :: trace_init, trace_enter, trace_leave

contains

    subroutine trace_init()
        character(len=8) :: val
        integer :: stat
        if (enabled) return
        call get_environment_variable('FORTFRONT_TRACE', val, status=stat)
        if (stat == 0) then
            ! The Windows CRT stack grows conservatively when piping; disable
            ! tracing there to avoid spurious stack overflows in CI.
            if (.not. is_windows_platform()) then
                enabled = .true.
            end if
        end if
        ! Optional: file logging
        call get_environment_variable('FORTFRONT_TRACE_FILE', file_name, status=stat)
        if (stat == 0 .and. len_trim(file_name) > 0) then
            open(newunit=file_u, file=trim(file_name), status='replace', action='write')
        end if
    end subroutine trace_init

    logical function is_windows_platform()
        character(len=16) :: os_name
        integer :: stat
        call get_environment_variable('OS', os_name, status=stat)
        if (stat == 0) then
            is_windows_platform = index(os_name, 'Windows') > 0
            return
        end if
        call get_environment_variable('WINDIR', os_name, status=stat)
        is_windows_platform = (stat == 0)
    end function is_windows_platform

    subroutine trace_enter(name)
        character(len=*), intent(in) :: name
        if (.not. enabled) return
        depth = depth + 1
        if (depth > MAX_DEPTH) then
            write(error_unit, '(A,I0,1X,A)') 'TRACE: Max depth exceeded: ', depth, trim(name)
            error stop 1
        end if
        write(error_unit, '(A,I0,2X,A)') '>> depth', depth, trim(name)
        if (file_u > 0) then
            write(file_u, '(A,I0,2X,A)') '>> depth', depth, trim(name)
            flush(file_u)
        end if
    end subroutine trace_enter

    subroutine trace_leave(name)
        character(len=*), intent(in) :: name
        if (.not. enabled) return
        write(error_unit, '(A,I0,2X,A)') '<< depth', depth, trim(name)
        if (file_u > 0) then
            write(file_u, '(A,I0,2X,A)') '<< depth', depth, trim(name)
            flush(file_u)
        end if
        if (depth > 0) depth = depth - 1
    end subroutine trace_leave

end module debug_trace
