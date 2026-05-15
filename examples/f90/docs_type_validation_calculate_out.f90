program main
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: val
    val = calculate(1.0, 2.0)
contains

    real(dp) function calculate(a, b) result(res)
        implicit none
        real, intent(in) :: a
        real, intent(in) :: b
        res = a + b
    end function calculate
end program main
