! DATA statements with multiple objects should initialize each variable
program data_multi_objects
    implicit none
    integer :: i, j, k
    real :: x, y
    data i, j, k / 1, 2, 3 /
    data x, y / 3.5, 7.2 /
end program data_multi_objects
