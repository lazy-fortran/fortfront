program main
    implicit none
    real :: val
    val = calculate(1.0, 2.0)
contains

    real function calculate(a, b) result(res)
        implicit none
        real, intent(in) :: a
        real, intent(in) :: b
        res = a + b
    end function calculate
end program main
