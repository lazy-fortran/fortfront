implicit none
integer :: x, y, z
x = 1; y = 2; z = 3
if (x + y > z) then
    print *, 'condition true'
end if
