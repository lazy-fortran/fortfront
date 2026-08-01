! Invalid: a statement function is not one of the statements BLOCK DATA
! admits (F2018 C1116). Reduced from gfortran.dg/blockdata_8.f90.
block data blockdata_8
    common /one/ x
    real :: x
    f(y) = y
end block data blockdata_8
