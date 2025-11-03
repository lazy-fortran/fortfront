! Nested array literal should be converted to reshape for square matrices
program nested_array_literal_square
    implicit none
    integer :: mat(2,2)
    mat = [[1, 2], [3, 4]]
end program nested_array_literal_square
