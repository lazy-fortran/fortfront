module debug_trace
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    integer, save :: depth = 0
    logical, save :: enabled = .false.
    integer, parameter :: MAX_DEPTH = 2000

    public :: trace_init, trace_enter, trace_leave

contains

    subroutine trace_init()
        character(len=8) :: val
        integer :: stat
        if (enabled) return
        call get_environment_variable('FORTFRONT_TRACE', val, status=stat)
        if (stat == 0) then
            enabled = .true.
        end if
    end subroutine trace_init

    subroutine trace_enter(name)
        character(len=*), intent(in) :: name
        if (.not. enabled) return
        depth = depth + 1
        if (depth > MAX_DEPTH) then
            write(error_unit, '(A,I0,1X,A)') 'TRACE: Max depth exceeded: ', depth, trim(name)
            error stop 1
        end if
        write(error_unit, '(A,I0,2X,A)') '>> depth', depth, trim(name)
    end subroutine trace_enter

    subroutine trace_leave(name)
        character(len=*), intent(in) :: name
        if (.not. enabled) return
        write(error_unit, '(A,I0,2X,A)') '<< depth', depth, trim(name)
        if (depth > 0) depth = depth - 1
    end subroutine trace_leave

end module debug_trace

