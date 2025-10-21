! Test NAMELIST I/O
program test_namelist
    implicit none
    integer :: n
    real :: x
    namelist /params/ n, x

    n = 10
    x = 3.14

    write(*, nml=params)
end program test_namelist
