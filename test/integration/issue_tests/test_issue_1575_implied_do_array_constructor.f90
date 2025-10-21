! Test implied DO in array constructor
program test_implied_do
    implicit none
    integer :: arr(10)
    integer :: i

    arr = [(i*2, i=1,10)]

    print *, arr
end program test_implied_do
