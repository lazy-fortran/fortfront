program test_openacc_clauses
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: i, n
    real(dp) :: a(100), b(100), c(100)

    n = 100

    !$acc parallel loop copyin(a, b) copyout(c)
    do i = 1, n
        c(i) = a(i) + b(i)
    end do
    !$acc end parallel loop

    !$acc kernels async(1) wait(2)
    do i = 1, n
        a(i) = c(i) * 2.0_dp
    end do
    !$acc end kernels

    !$acc update host(a)

    print *, 'Done'
end program test_openacc_clauses
