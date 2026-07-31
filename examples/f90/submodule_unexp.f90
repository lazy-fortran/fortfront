! Rejection fixture (gfortran.dg/submodule_unexp.f90, PR fortran/69498):
! a SUBMODULE statement cannot appear inside a derived-type definition.
program submodule_unexp
    implicit none
    type :: t
    submodule (m) sm
    end type t
end program submodule_unexp
