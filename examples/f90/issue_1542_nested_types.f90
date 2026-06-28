! Minimal reproducer: nested types broken
program test
    implicit none

    type :: pair_t
        real :: start
        real :: end
    end type pair_t

    type(pair_t) :: p
    p%start = 1.0
    p%end = 2.0
    print *, p%start, p%end
end program test
