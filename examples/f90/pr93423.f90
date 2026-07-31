! Rejection fixture (gfortran.dg/pr93423.f90, PR fortran/93423):
! an mp-subprogram-stmt is "MODULE PROCEDURE procedure-name" (F2018 R1505);
! it must not repeat the dummy argument list of the interface body.
module pr93423_mod
    implicit none
    interface
        module function bp(s) result(res)
            integer, intent(inout) :: s
            integer :: res
        end function bp
    end interface
end module pr93423_mod

submodule (pr93423_mod) pr93423_sub
contains
    module procedure bp(s)
        res = s
    end procedure bp
end submodule pr93423_sub
