implicit none
integer :: x, y, z, w
x = 1; y = 2; z = 3; w = 4
if (x + (y * (z - w)) > 0) then
    print *, 'nested'
end if
