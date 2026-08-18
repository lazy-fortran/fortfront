! gfortran.dg pdt_30.f90: a derived type with an empty parameter list must be
! rejected ("A type parameter list is required").
program p
  type :: q8 ()
    real :: pj
  end type q8
  type(q8) :: ki
  ki%pj = 1.0
  print *, ki%pj
end program p
