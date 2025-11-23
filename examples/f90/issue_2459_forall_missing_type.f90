program test_forall_type
    implicit none
    integer :: n = 5
    integer :: i, j
    real :: a(5, 5)

    forall (i = 1:n, j = 1:n)
        a(i, j) = real(i + j)
    end forall

    print *, a(1, 1)
end program test_forall_type
