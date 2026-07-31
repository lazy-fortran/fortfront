! The dummy argument of a FINAL subroutine must be a nonpolymorphic variable of
! the type being finalized (F2018 C790).
module finalize_22
    implicit none

    type :: cfml
    contains
        final :: mld
    end type cfml

    type, extends(cfml) :: cfmde
    end type cfmde

contains

    subroutine mld(s)
        class(cfml), intent(inout) :: s
    end subroutine mld

end module finalize_22
