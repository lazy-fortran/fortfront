! Invalid: a module specification part has no executable part, so a statement
! function cannot appear there. Reduced from gfortran.dg/stfunc_5.f90.
module stfunc_5
    f(x) = x**2
end module stfunc_5
