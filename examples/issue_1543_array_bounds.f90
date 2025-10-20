! Minimal reproducer: array bounds lost
program test
    implicit none
    real :: arr(0:4)
    real :: mat(-1:1, 2:4)

    arr(0) = 1.0
    mat(-1, 2) = 2.0

    print *, arr(0), mat(-1, 2)
end program test