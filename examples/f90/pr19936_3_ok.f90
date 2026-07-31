! Corrected neighbor of pr19936_3.f90: the array item is an implied-do.
program pr19936_3_ok
    integer :: i
    print *, (/(i, i=1, 4)/)
end program pr19936_3_ok
