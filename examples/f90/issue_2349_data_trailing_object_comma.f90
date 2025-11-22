program test_data_trailing_object_comma
    implicit none
    integer :: a, b, c
    data a, b, c, / 1, 2, 3 /
    print *, a, b, c
end program test_data_trailing_object_comma
