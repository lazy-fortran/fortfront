program inline_if_fixture
    implicit none
    real :: x
    integer :: i, j, n
    n = 3
    call random_number(x)
    do i = 1, n
        call random_number(x)
        print*, "x =", x
        if (x > 0.3) print*, "x larger than 0.3"
        if (x > 0.2) then
            print*, "x larger than 0.2"
        end if
        do j = 1, 2
            if (j == 1) print*, "nested iteration", j
        end do
    end do
end program inline_if_fixture
