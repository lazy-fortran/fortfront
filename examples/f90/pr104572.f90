! The single dummy argument of a FINAL subroutine must be a data object, so an
! alternate return indicator is not allowed (F2018 C789).
module pr104572
    implicit none

    type :: t
    contains
        final :: s
    end type t

contains

    subroutine s(*)
    end subroutine s

end module pr104572
