program test_openacc
    implicit none
    integer :: i
    real :: a(100), b(100), c(100)

    !$acc parallel loop
    do i = 1, 100
        c(i) = a(i) + b(i)
    end do
    !$acc end parallel loop

    print *, 'Done'
end program test_openacc
