implicit none
integer :: x, y, z
x = 1; y = 2; z = 3
if (x + x > y) then
    print *, 'query'
end if
