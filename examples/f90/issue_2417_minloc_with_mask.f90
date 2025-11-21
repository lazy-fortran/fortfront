program test_minloc_timeout
    implicit none
    real(8) :: arr(10, 10)
    integer :: idx(2)
    logical :: mask(10, 10)
    integer :: i

    ! Initialize array
    do i = 1, 100
        arr(mod(i - 1, 10) + 1, (i - 1)/10 + 1) = real(i, 8)
    end do

    ! Create mask
    mask = arr > 50.0d0

    ! This should not timeout - no dim argument returns rank-1 array
    idx = minloc(arr, mask=mask)
    print *, "Result:", idx
end program test_minloc_timeout
