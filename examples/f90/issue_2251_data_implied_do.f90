program issue_2251_data_implied_do
    implicit none
    integer :: i
    real :: coeff(2)
    data (coeff(i), i = 1, 2) /1.0, 2.0/
    print *, coeff
end program issue_2251_data_implied_do
