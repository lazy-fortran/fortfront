! Minimal reproducer for issue #1337: FORALL body assignments dropped
program test_forall
    implicit none
    integer :: i, j
    integer, dimension(3, 3) :: matrix

    forall (i=1:3, j=1:3)
        matrix(i, j) = i * 10 + j
    end forall

    print *, matrix(1, :)
end program test_forall
