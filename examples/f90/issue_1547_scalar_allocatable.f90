! Minimal reproducer: scalar marked allocatable
program test
    implicit none
    integer :: x

    x = 1
    x = 2
    x = 3

    print *, x
end program test
