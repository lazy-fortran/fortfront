! Corrected neighbour of pr96102.f90 (issue #2888).
! The internal procedure has a name of its own, so the host associated
! variable keeps its meaning inside S.
module pr96102_corrected_mod
    implicit none
    integer :: n = 2
contains
    subroutine s()
        if (n /= 0) print *, 'nonzero', zero()
    contains
        integer function zero()
            zero = 0
        end function zero
    end subroutine s
end module pr96102_corrected_mod
