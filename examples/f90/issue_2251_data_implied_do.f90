program issue_2251_data_implied_do
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp) :: coeff(4)
    integer :: i

    data (coeff(i), coeff(i + 2), i = 1, 2) / 1.0d0, 2.0d0, 3.0d0, 4.0d0 /

    print *, coeff
end program issue_2251_data_implied_do
