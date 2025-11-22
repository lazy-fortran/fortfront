program test_data_trailing
    implicit none
    integer :: table(5)
    data table / 1, 2, 3, 4, 5, /
    print *, table
end program test_data_trailing
