! Corrected neighbour: a non-empty parameter list.
program p
  type :: q8 (k)
    integer, kind :: k
    real :: pj
  end type q8
  type(q8(4)) :: ki
  ki%pj = 1.0
  print *, ki%pj
end program p
