program main
    implicit none
    integer :: i
    integer :: squares(5)
    integer :: evens(10)
    squares = (/ (i**2, i=1, 5) /)
    evens = (/ (2*i, i=1, 10) /)
    print *, squares
    print *, evens
end program main
