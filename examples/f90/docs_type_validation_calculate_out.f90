program main
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: val
    val = calculate(1.0_8, 2.0_8)
contains

    real(dp) function calculate(a, b) result(res)
        implicit none
        real(dp), intent(in) :: a
        real(dp), intent(in) :: b
        res = a + b
    end function calculate
end program main
