module process_exit
    use, intrinsic :: iso_c_binding, only: c_int
    use debug_trace, only: trace_finalize
    implicit none
    private
    public :: exit_quiet

    interface
        subroutine c_exit(status) bind(C, name="exit")
            import :: c_int
            integer(c_int), value :: status
        end subroutine c_exit
    end interface

contains

    subroutine exit_quiet(status)
        integer, intent(in) :: status
        call trace_finalize()
        call c_exit(int(status, kind=c_int))
    end subroutine exit_quiet

end module process_exit
