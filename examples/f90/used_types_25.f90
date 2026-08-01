! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! The main program defines a derived type whose name is already accessible by
! use association.
module used_types_25_mod
    implicit none
    type t
        integer :: k
    end type t
end module used_types_25_mod

program used_types_25
    use used_types_25_mod
    implicit none
    type t
        integer :: j
    end type t
end program used_types_25
