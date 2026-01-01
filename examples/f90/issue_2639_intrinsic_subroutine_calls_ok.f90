program issue_2639_intrinsic_subroutine_calls_ok
    implicit none

    real :: t
    real :: r
    integer :: count
    integer :: rate
    integer :: max_count

    call cpu_time(t)
    call system_clock(count=count, count_rate=rate, count_max=max_count)
    call random_number(r)
end program issue_2639_intrinsic_subroutine_calls_ok
