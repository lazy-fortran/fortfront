program issue_2639_intrinsic_subroutine_calls_ok
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    real(dp) :: t
    real(dp) :: r
    integer :: count
    integer :: rate
    integer :: max_count

    call cpu_time(t)
    call system_clock(count=count, count_rate=rate, count_max=max_count)
    call random_number(r)
end program issue_2639_intrinsic_subroutine_calls_ok
