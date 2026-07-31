! Corrected neighbour of pr104572.f90: the FINAL subroutine takes a single data
! object of the finalized type instead of an alternate return indicator.
module pr104572_valid
    implicit none

    type :: t
    contains
        final :: s
    end type t

contains

    subroutine s(x)
        type(t), intent(inout) :: x
    end subroutine s

end module pr104572_valid
