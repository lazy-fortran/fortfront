! DATA statements assigning separate scalars in sequence
program data_multiple_sets
    implicit none
    integer :: a, b
    data a / 1 /, b / 2 /
    print *, a, b
end program data_multiple_sets
