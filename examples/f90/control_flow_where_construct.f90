program test
    real :: a(10), b(10)
    where (a > 0.0)
        b = sqrt(a)
    elsewhere
        b = 0.0
    end where
end program test
