program test_multi_function_call
    implicit none

    print *, 'hypotenuse(3, 4):', hypotenuse(3.0, 4.0)

contains

    real function square(x)
        real, intent(in) :: x
        square = x * x
    end function square

    real function hypotenuse(a, b)
        real, intent(in) :: a, b
        hypotenuse = sqrt(square(a) + square(b))
    end function hypotenuse

end program test_multi_function_call
