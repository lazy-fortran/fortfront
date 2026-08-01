! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! N is host associated into S and referenced there, so an internal procedure
! of S may not carry the same name.
module pr96102_mod
    implicit none
    integer :: n = 2
contains
    subroutine s()
        if (n /= 0) print *, 'nonzero'
    contains
        integer function n()
            n = 0
        end function n
    end subroutine s
end module pr96102_mod
