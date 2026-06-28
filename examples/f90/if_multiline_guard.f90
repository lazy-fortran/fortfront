program if_multiline_guard
    implicit none
    real :: samples(5)
    real :: tolerance

    samples = [1.0, -2.0, 3.0, -4.0, 5.0]
    tolerance = 0.1

    if (any(abs(samples) &
        > tolerance)) then
        print *, "values exceed tolerance"
    else
        print *, "values within tolerance"
    end if
end program if_multiline_guard
