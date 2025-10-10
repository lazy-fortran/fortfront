! Minimal reproducer for issue #1353: Derived type definitions completely mangled
program test_derived_type
    implicit none

    type :: point_t
        real :: x
        real :: y
    end type point_t

    type(point_t) :: p

    p%x = 1.0
    p%y = 2.0
    print *, p%x, p%y
end program test_derived_type
