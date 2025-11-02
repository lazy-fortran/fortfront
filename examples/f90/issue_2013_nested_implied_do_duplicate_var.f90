program test_implied_do_nested
    implicit none
    integer :: i, j
    integer, dimension(12) :: matrix_flat

    ! Create flattened 3x4 matrix using nested implied-do
    matrix_flat = [(((i - 1) * 4 + j, j = 1, 4), i = 1, 3)]

    print *, 'Flattened matrix:', matrix_flat
end program test_implied_do_nested
