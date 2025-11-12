program issue_2252_data_value_implied_do
    implicit none
    integer :: i
    real :: coeff(2)

    ! Nonstandard DATA value implied-do regression coverage
    data coeff/(i*1.0, i=1, 2)/
    print *, coeff
end program issue_2252_data_value_implied_do
