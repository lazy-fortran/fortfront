program main
    implicit none
    integer :: i
    integer, unsigned :: sum, u, v
    i = 5
    u = uint(i)
    v = uint(7)
    sum = wrap_add(u, v)
end program main
