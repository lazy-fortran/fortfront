program do_concurrent_simple
    implicit none
    integer :: i
    integer :: arr(10)

    do concurrent (i = 1:10)
        arr(i) = i * 2
    end do

    print *, arr
end program do_concurrent_simple
