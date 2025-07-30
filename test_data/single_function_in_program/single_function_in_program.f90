program main
    implicit none
    real(8) :: x
    real(8) :: y

    x = 5.0d0
    y = square(x)
    print *, x, y
contains
    real(8) function square(val)
        implicit none
        real(8), intent(in) :: val
        square = val*val
    end function square
end program main
