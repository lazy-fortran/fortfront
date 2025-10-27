program issue_1970_implied_do_array_constructor
    implicit none

    integer :: i
    real :: halves(5)

    halves = [(real(i) / 2.0, i=1, 5)]
    print *, 'Halves:', halves
end program issue_1970_implied_do_array_constructor
