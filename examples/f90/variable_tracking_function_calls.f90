implicit none
real :: x, y
x = 1.0; y = 2.0
if (sin(x) > cos(y)) then
    print *, 'trig'
end if
