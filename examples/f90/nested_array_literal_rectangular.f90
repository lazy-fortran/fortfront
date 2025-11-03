! Nested array literal should reshape for rectangular matrices
program nested_array_literal_rectangular
    implicit none
    integer :: mat(3,2)
    mat = [[1, 2], [3, 4], [5, 6]]
end program nested_array_literal_rectangular
