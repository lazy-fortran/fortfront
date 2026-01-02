! Reproducer for issue #1353: derived type with extends attribute
program extends_demo
    implicit none

    type :: base_t
        real :: x
    end type base_t

    type, extends(base_t) :: point_t
        real :: y
    end type point_t

    type(point_t) :: p

    p%y = 2.0
    print *, p%y
end program extends_demo
