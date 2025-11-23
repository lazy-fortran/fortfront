! Test nested function calls with keyword arguments
program test_nested_function_args
    implicit none
    real :: result

    result = compute(array=[1.0, 2.0, 3.0], scale=2.0, offset=func(10))

    print *, result

contains
    function compute(array, scale, offset) result(val)
        real, intent(in) :: array(:)
        real, intent(in) :: scale
        real, intent(in) :: offset
        real :: val

        val = sum(array) * scale + offset
    end function compute

    function func(x) result(y)
        integer, intent(in) :: x
        real :: y
        y = real(x) * 1.5
    end function func
end program test_nested_function_args
