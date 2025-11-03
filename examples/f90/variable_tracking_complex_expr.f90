implicit none
integer :: a, b, c, d, e, f
a = 1; b = 2; c = 3; d = 4; e = 5; f = 6
if ((a + b) * c > (d - e) / f) then
    print *, 'complex'
end if
