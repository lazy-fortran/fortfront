program test_missing_type
    implicit none
    integer :: x

    do i = 1, 10
        x = i * 2
        print *, x
    end do
end program test_missing_type
