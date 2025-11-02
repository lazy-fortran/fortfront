program test_pure_function
    implicit none
    integer :: x, y, result

    x = 3
    y = 4
    result = pure_multiply(x, y)
    print *, 'Result:', result

contains

    pure integer function pure_multiply(a, b)
        integer, intent(in) :: a, b
        pure_multiply = a * b
    end function pure_multiply

end program test_pure_function
