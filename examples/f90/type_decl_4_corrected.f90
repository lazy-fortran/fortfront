! Corrected neighbour of type_decl_4.f90 (issue #2888).
! Declaring a variable OF a derived type in the scoping unit that defines the
! type is legal; only reusing the type name itself as an entity name is not.
program main
    implicit none
    type Xx
        integer :: i
    end type Xx
    type(Xx) :: xx_value
    real :: yy

    xx_value%i = 3
    yy = 2.0
    print *, xx_value%i, yy
end program main
