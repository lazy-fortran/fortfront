program test
    real :: x(10), y(10)
    associate (z => x + y)
        print *, z
    end associate
end program test
