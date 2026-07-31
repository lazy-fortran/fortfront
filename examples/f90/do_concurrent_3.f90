program do_concurrent_3
    ! INVALID: F2008 C1283 via 8.1.6.5. The body of a DO CONCURRENT construct
    ! is a pure context, so it may not reference an impure intrinsic
    ! subroutine such as RANDOM_NUMBER.
    implicit none
    integer :: i
    real :: array(123), val

    do concurrent(i=1:123)
        call random_number(val)
        array(i) = val
    end do

    print *, array(1)
end program do_concurrent_3
