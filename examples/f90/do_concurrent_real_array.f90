program do_concurrent_real_array
    implicit none
    integer :: i
    real :: arr(10)

    do concurrent (i = 1:10)
        arr(i) = real(i) * 2.0
    end do

    print *, 'Array:', arr
end program do_concurrent_real_array
