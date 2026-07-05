block data
  integer :: x
  common /c/ x
  data x /7/
end
program p
  integer :: x
  common /c/ x
  print *, x
end program
