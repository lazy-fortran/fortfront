! Demonstration of SELECT RANK construct support
! Note: This example shows that fortfront can parse and emit SELECT RANK constructs
! Full assumed-rank array (..) support is limited due to Fortran 2018 feature complexity
program test_select_rank
    implicit none
    integer :: arr(3, 4)

    ! Simple SELECT RANK example demonstrating the parser works
    select rank(arr)
        rank(2)
        print *, "This is a rank-2 array"
        arr = 42
        rank(1)
        print *, "This is a rank-1 array"
        rank default
        print *, "Unknown rank"
    end select

    print *, "Array initialized"
end program test_select_rank
