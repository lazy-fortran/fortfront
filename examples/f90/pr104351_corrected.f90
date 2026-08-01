! Corrected neighbour of pr104351.f90 (issue #2888).
! The variable and the contained function have distinct names.
program pr104351_corrected
    implicit none
    type t
        integer :: k
    end type t
    type(t) :: value

    value%k = 1
    print *, value%k, f()
contains
    real function f() result(z)
        z = 0.0
    end function f
end program pr104351_corrected
