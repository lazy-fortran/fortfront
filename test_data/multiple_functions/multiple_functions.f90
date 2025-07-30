program main
    implicit none
    real(8) :: x
    real(8) :: y
    real(8) :: z

    x = 5.0d0
    y = square(x)
    z = cube(x)
    print *, x, y, z
contains
    real(8) function square(val)
        implicit none
        real(8), intent(in) :: val
        square = val*val
    end function square
    real(8) function cube(val)
        implicit none
        real(8), intent(in) :: val
        cube = val*val*val
    end function cube
end program main
