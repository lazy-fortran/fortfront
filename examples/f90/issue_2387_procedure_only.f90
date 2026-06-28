function square(x)
    implicit none
    integer, intent(in) :: x
    integer :: square

    square = x * x
end function square

result = square(42)
