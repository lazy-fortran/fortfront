program test_data_trailing_value_comma
    implicit none
    integer :: arr(3)
    data arr /1, 2, 3, /
    print *, arr
end program test_data_trailing_value_comma
