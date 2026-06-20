program implied_do_index_locality_valid
    implicit none
    integer :: i, j
    integer, dimension(12) :: a

    ! Nested implied-DO with distinct, properly scoped indices.
    a = [(((i - 1) * 4 + j, j=1, 4), i=1, 3)]

    print *, a
end program implied_do_index_locality_valid
