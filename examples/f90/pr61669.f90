! Invalid: a data declaration statement cannot appear after the executable
! part has started. Reduced from gfortran.dg/pr61669.f90.
program pr61669
    implicit none
    real :: b
    b = 0.02
    character(len=80) :: a
    print *, b, a
end program pr61669
