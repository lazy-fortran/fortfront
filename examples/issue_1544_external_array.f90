! Minimal reproducer: array marked external
program test
    implicit none
    real :: arr(5)
    arr(1) = 1.0
    print *, arr(1)
end program test