! Reproducer for issue #1353: derived type definition inside module
module attr_mod
    implicit none

    type, public :: point_t
        real :: x
        real :: y
    end type point_t

contains

    subroutine assign_point()
        type(point_t) :: p

        p%x = 1.0
        p%y = 2.0
        print *, p%x, p%y
    end subroutine assign_point

end module attr_mod
