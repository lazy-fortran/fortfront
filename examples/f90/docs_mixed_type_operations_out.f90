program main
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: value
    value = mixed_calc(1, 2.5_8)
contains

    real(dp) function mixed_calc(i, x) result(y)
        implicit none
        integer, intent(in) :: i
        real(dp), intent(in) :: x
        y = i + x
    end function mixed_calc
end program main
