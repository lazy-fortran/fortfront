! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! The COMMON statement declares an object T in a scope that already uses the
! host associated derived type T, so one name would denote both.
module common_29_mod
    implicit none
    type t
        integer :: k
    end type t
contains
    subroutine s()
        type(t) :: x
        common t

        x%k = 1
    end subroutine s
end module common_29_mod
