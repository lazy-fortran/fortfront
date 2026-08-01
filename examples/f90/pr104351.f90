! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! The name F is declared as a variable of type T and is also the name of a
! procedure contained in the same scoping unit.
program pr104351
    implicit none
    type t
        integer :: k
    end type t
    type(t) :: f

    f%k = 1
contains
    real function g() result(z)
        z = 0.0
    end function g

    real function f() result(z)
        z = 0.0
    end function f
end program pr104351
