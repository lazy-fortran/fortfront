program test
    integer :: i, n, step
    n = 20
    step = 1
    do i = n/2-5, n/2+5, step*2
        print *, i
    end do
end program test
