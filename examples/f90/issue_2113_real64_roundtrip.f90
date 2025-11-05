! Test case for issue #2113: Preserve real(real64) and double precision in round-trip
! Both should be preserved exactly as written

module test_types
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer, parameter :: real64 = dp
contains
    ! Test 1: real(real64) function signature
    real(real64) function compute_real64(val)
        real(real64), intent(in) :: val
        compute_real64 = val * 2.0_real64
    end function compute_real64

    ! Test 2: double precision function signature
    double precision function compute_double(val)
        double precision, intent(in) :: val
        compute_double = val * 2.0d0
    end function compute_double
end module test_types
