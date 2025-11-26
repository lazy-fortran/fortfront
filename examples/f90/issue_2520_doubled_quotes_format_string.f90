! Test for doubled quotes in format strings
! Fortran uses '' to represent a literal ' inside single-quoted strings
program test_doubled_quotes
    implicit none
    integer :: i
    real :: a(4)
    a = (/1.0, 2.0, 3.0, 4.0/)
    write(*, '(4(f5.2,'',''))') (a(i), i = 1, 4)
    write(*, '("Hello, I''m Fortran")')
end program test_doubled_quotes
