program implied_do_index_shadow
    implicit none
    integer :: i
    integer, dimension(9) :: a

    ! The nested implied-DO reuses the index name i of the enclosing
    ! implied-DO, shadowing the outer index.
    a = [((i + i, i=1, 3), i=1, 3)]

    print *, a
end program implied_do_index_shadow
