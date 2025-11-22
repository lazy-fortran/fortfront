program test_data_implied_do
    implicit none
    real :: arr(3, 3)
    integer :: i, j
    data ((arr(i, j), i = 1, j), j = 1, 3) /6 * 1.0/
    print *, arr
end program test_data_implied_do
