module debug_trace
    use, intrinsic :: iso_fortran_env, only: error_unit
    use cli_env, only: is_truthy
    implicit none
    private

    integer, save :: depth = 0
    logical, save :: enabled = .false.
    integer, parameter :: MAX_DEPTH = 2000
    integer, save :: file_u = -1
    character(len=256), save :: file_name = ''

    public :: trace_init, trace_enter, trace_leave, trace_is_enabled

contains

    subroutine trace_init()
        character(len=64) :: val
        integer :: stat
        if (enabled) return
        val = ''
        call get_environment_variable('FORTFRONT_TRACE', val, status=stat)
        if (stat == 0) then
            ! Opt-in: enable only for truthy values; still avoid enabling on
            ! Windows to prevent CI pipe stack overflows.
            if (is_truthy(trim(val)) .and. .not. is_windows_platform()) then
                enabled = .true.
            end if
        end if
        ! Optional: file logging (open only if tracing enabled)
        if (enabled) then
            call get_environment_variable('FORTFRONT_TRACE_FILE', file_name, &
                                          status=stat)
            if (stat == 0 .and. len_trim(file_name) > 0) then
                ! Preserve any early CLI trace lines by appending instead of replacing.
                open (newunit=file_u, file=trim(file_name), status='unknown', &
                      position='append', action='write')
            end if
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
            write (error_unit, '(A,I0,1X,A)') 'TRACE: Max depth exceeded: ', &
                depth, trim(name)
            error stop 1
        end if
        write (error_unit, '(A,I0,2X,A)') '>> depth', depth, trim(name)
        if (file_u > 0) then
            write (file_u, '(A,I0,2X,A)') '>> depth', depth, trim(name)
            flush (file_u)
        end if
    end subroutine trace_enter

    subroutine trace_leave(name)
        character(len=*), intent(in) :: name
        if (.not. enabled) return
        write (error_unit, '(A,I0,2X,A)') '<< depth', depth, trim(name)
        if (file_u > 0) then
            write (file_u, '(A,I0,2X,A)') '<< depth', depth, trim(name)
            flush (file_u)
        end if
        if (depth > 0) depth = depth - 1
    end subroutine trace_leave

    pure logical function trace_is_enabled()
        trace_is_enabled = enabled
    end function trace_is_enabled

end module debug_trace
