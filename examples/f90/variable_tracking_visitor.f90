implicit none
integer :: a, b, c
a = 1; b = 2; c = 3
if (a + b > c) then
    print *, 'visitor'
end if
