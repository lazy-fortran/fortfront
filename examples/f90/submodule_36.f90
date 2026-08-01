! Rejection fixture (gfortran.dg/submodule_36.f90, PR fortran/121379):
! a separate module subprogram that implements a module procedure interface
! must carry the MODULE prefix (F2018 15.6.2.5). The abbreviated END forms
! are intentional: they are part of the original GCC regression.
module m
  interface g
  real module function realg1 (arg1, arg2)
    real, intent(in) :: arg1, arg2
  end
  end interface

  interface h
  real module function realg2 (arg1, arg2)
    real, intent(in) :: arg1, arg2
  end
  end interface

contains
end module m

submodule (m) subm
contains
  real module function realg1 (arg1, arg2)
    real, intent(in) :: arg1, arg2
    realg1 = arg1 + arg2
  end

  real function realg2 (arg1, arg2)  ! { dg-error "requires the MODULE prefix" }
    real, intent(in) :: arg1, arg2
    realg2 = arg1 + arg2
  end
end

  use m
  print *, g(1.0, 1.0), h(2.0, 2.0)
end
