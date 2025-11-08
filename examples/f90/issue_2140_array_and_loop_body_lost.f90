program test_array_loop
    implicit none
    real :: data(10)
    integer :: i

    do i = 1, 10
        data(i) = real(i)
    end do

    print *, 'Result:', data(1:5)
end program test_array_loop
