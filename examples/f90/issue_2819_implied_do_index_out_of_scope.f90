program implied_do_index_out_of_scope
    implicit none
    integer :: i
    integer, dimension(3) :: a

    ! The index i is not in scope within its own controlling triplet, so the
    ! upper bound referencing i is an out-of-scope reference.
    a = [(i, i=1, i)]

    print *, a
end program implied_do_index_out_of_scope
