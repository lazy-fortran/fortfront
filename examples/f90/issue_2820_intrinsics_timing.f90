! Issue #2820: CPU_TIME and SYSTEM_CLOCK intrinsic calls must round-trip.
program issue_2820_intrinsics_timing
    implicit none
    real :: t
    integer :: n

    call cpu_time(t)
    call system_clock(n)
    print *, t, n
end program issue_2820_intrinsics_timing
