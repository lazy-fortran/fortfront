! Rejection fixture (gfortran.dg/submodule_twice.f90, PR fortran/69498):
! a SUBMODULE statement is a program unit (F2018 R1116) and cannot appear
! inside the body of another program unit.
program submodule_twice
    implicit none
    submodule (m) sm
    submodule (m2) sm2
end program submodule_twice
