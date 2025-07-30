program main
    implicit none
    real(8) :: x, y, z, w

    ! Trigonometric calculations
    x = 3.14159/4.0
    y = sin(x)
    z = cos(x)
    w = sin(x)*cos(x)
    print *, "sin(pi/4) =", y
    print *, "cos(pi/4) =", z
    print *, "sin * cos =", w
end program main
