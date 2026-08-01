! Corrected neighbour of common_29.f90 (issue #2888).
! The COMMON object has its own name, so the host associated type T keeps
! denoting the type alone.
module common_29_corrected_mod
    implicit none
    type t
        integer :: k
    end type t
contains
    subroutine s()
        type(t) :: x
        integer :: shared
        common /com/ shared

        x%k = 1
        shared = x%k
    end subroutine s
end module common_29_corrected_mod
