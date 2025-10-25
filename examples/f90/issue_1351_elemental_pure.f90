! Minimal reproducer for issue #1351: ELEMENTAL and PURE keywords dropped
elemental function square(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * x
end function square

pure function compute(a, b) result(c)
    integer, intent(in) :: a, b
    integer :: c
    c = a * 2 + b * 3
end function compute

program test_elemental_pure
    implicit none
    real :: arr(3)
    real, external :: square
    integer, external :: compute

    arr = [1.0, 2.0, 3.0]
    arr = square(arr)
    print *, arr
    print *, compute(5, 3)
end program test_elemental_pure
