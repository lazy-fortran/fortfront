program test_array_constructor_type
    implicit none
    real :: real_arr(3)

    real_arr = (/ real :: 1, 2, 3 /)

    print *, real_arr
end program test_array_constructor_type
