! DATA statement mixing repeat counts and explicit values
integer :: arr(7)
DATA arr /3*1, 2, 3*0/
print *, arr(1)
