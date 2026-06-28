program test_debug_trace_profile
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use debug_trace, only: trace_enter, trace_leave, trace_profile_get_stat, &
        trace_profile_reset, trace_set_profile_enabled
    implicit none

    integer(int64) :: call_count
    integer(int64) :: total_counts
    integer(int64) :: self_counts
    logical :: found

    call trace_profile_reset()
    call trace_set_profile_enabled(.true.)

    call trace_enter('profile:outer')
    call trace_enter('profile:inner')
    call trace_leave('profile:inner')
    call trace_leave('profile:outer')

    call trace_profile_get_stat('profile:outer', call_count, total_counts, &
        self_counts, found)
    if (.not. found) then
        write (error_unit, '(A)') 'FAIL: Expected profile:outer section stats'
        stop 1
    end if
    if (call_count /= 1_int64) then
        write (error_unit, '(A,I0)') 'FAIL: profile:outer call count = ', &
            call_count
        stop 1
    end if

    call trace_profile_get_stat('profile:inner', call_count, total_counts, &
        self_counts, found)
    if (.not. found) then
        write (error_unit, '(A)') 'FAIL: Expected profile:inner section stats'
        stop 1
    end if
    if (call_count /= 1_int64) then
        write (error_unit, '(A,I0)') 'FAIL: profile:inner call count = ', &
            call_count
        stop 1
    end if

    print *, 'PASS: debug_trace profiling collects section stats'
end program test_debug_trace_profile
