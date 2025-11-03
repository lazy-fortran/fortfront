program test
    real :: a(10,10)
    integer :: i, j
    forall (i=1:10, j=1:10, i==j)
        a(i,j) = 1.0
    end forall
end program test
