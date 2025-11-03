! DATA statement with repeat count should expand to literal assignment
integer :: arr(5)
DATA arr /5*0/
print *, arr(1)
