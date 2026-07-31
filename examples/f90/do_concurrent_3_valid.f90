program do_concurrent_3_valid
    ! VALID neighbour of do_concurrent_3.f90. The impure intrinsic subroutine
    ! is called outside the DO CONCURRENT construct; the construct body only
    ! performs pure work.
    implicit none
    integer :: i
    real :: array(123), val

    call random_number(val)

    do concurrent(i=1:123)
        array(i) = val*real(i)
    end do

    print *, array(1)
end program do_concurrent_3_valid
