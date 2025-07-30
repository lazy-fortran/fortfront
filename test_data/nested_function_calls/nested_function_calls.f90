program main
    implicit none
    real(8) :: x
    real(8) :: result

    x = 2.0d0
    result = square(double_val(x))
    print *, result
contains
    real(8) function double_val(val)
        implicit none
        real(8), intent(in) :: val
        double_val = val*2.0d0
    end function double_val
    real(8) function square(val)
        implicit none
        real(8), intent(in) :: val
        square = val*val
    end function square
end program main
