! Corrected neighbour of finalize_22.f90: the FINAL subroutine takes a
! nonpolymorphic dummy argument of the finalized type.
module finalize_22_valid
    implicit none

    type :: cfml
    contains
        final :: mld
    end type cfml

    type, extends(cfml) :: cfmde
    end type cfmde

contains

    subroutine mld(s)
        type(cfml), intent(inout) :: s
    end subroutine mld

end module finalize_22_valid
