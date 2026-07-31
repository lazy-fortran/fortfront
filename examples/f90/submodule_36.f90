! Rejection fixture (gfortran.dg/submodule_36.f90, PR fortran/121379):
! a separate module subprogram that implements a module procedure interface
! must carry the MODULE prefix (F2018 15.6.2.5).
module submodule_36_mod
    implicit none
    interface h
        real module function realg2(arg1, arg2)
            real, intent(in) :: arg1, arg2
        end function realg2
    end interface h
end module submodule_36_mod

submodule (submodule_36_mod) submodule_36_sub
contains
    real function realg2(arg1, arg2)
        real, intent(in) :: arg1, arg2
        realg2 = arg1 + arg2
    end function realg2
end submodule submodule_36_sub
