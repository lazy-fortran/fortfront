program test
    implicit none
    real :: a = 1.0, b = 2.0, c = 3.0
    real :: result
    result = (a + b) * c / (a - b) + sqrt(a**2 + b**2)
    print *, result
end program
