program test_equivalence
    implicit none
    integer :: i
    real :: r
    equivalence (i, r)

    i = 42
    print *, 'i =', i
    print *, 'r =', r

end program test_equivalence
