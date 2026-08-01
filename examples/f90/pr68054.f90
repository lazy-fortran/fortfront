! Invalid: PROTECTED is only allowed in the specification part of a module.
! Reduced from gfortran.dg/pr68054.f90.
real, protected :: x
x = 1
end
